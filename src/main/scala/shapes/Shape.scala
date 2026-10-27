package com.alecdorrington
package kneiphof
package shapes

import com.alecdorrington.kneiphof.incidence.IncidenceStructure

/**
  * The incidence structure of a shape: the unlabelled graph whose edges are the
  * arcs of a [[Rule]] on vertex numbers, with the vertices [[index]] names,
  * answered from the rule alone.
  *
  * @param link
  *   The edge for an arc between two vertices.
  * @param ends
  *   The ends of an unlabelled edge of the right kind, if it is one.
  * @param directed
  *   `true` if and only if an edge only stands for the arc in its own
  *   direction, rather than in either.
  */
private[kneiphof] final class Shape[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    index: Index[V],
    rule: Rule,
    link: (V, V) => X,
    ends: Any => Option[(Any, Any)],
    directed: Boolean,
  )
  extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] =
    ValueSet.from((0 until rule.order).map(index.vertex))

  override def hasVertex(vertex: Any): Boolean = index.index(vertex).isDefined

  override def vertexValue(vertex: Any): Option[V] = index
    .index(vertex)
    .map(index.vertex)

  override def order: Int = rule.order

  override def multiplicity(edge: Any): M = indices(edge).fold(M.zero):
    (first, second) => copies(first, second)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    incidentEdgeValues(vertex).toSeq.map(edge => edge -> multiplicity(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = ValueSet.from:
    index
      .index(vertex)
      .toSeq
      .flatMap: number =>
        rule.arcsFrom(number).map(this.edge(number, _)) ++
          rule.arcsInto(number).map(this.edge(_, number))

  override def find(edge: Any): Option[X] =
    arc(edge).map((source, target) => this.edge(source, target))

  /**
    * Every edge, listed at the arc from its lower end, or at its only arc, in
    * an undirected shape.
    */
  override def edges: Iterator[(X, M)] = (0 until rule.order)
    .iterator
    .flatMap: source =>
      rule
        .arcsFrom(source)
        .iterator
        .distinct
        .filter(target =>
          directed || source <= target || rule.arcs(target, source) == 0,
        )
        .map(target => edge(source, target) -> copies(source, target))

  override def size: Int = rule.size - merged

  override def degree(vertex: Any): Int =
    if merged > 0 then super.degree(vertex)
    else
      index
        .index(vertex)
        .fold(0)(number =>
          Math.addExact(
            rule.arcsFrom(number).size,
            rule.arcsInto(number).size,
          ),
        )

  override def loops: Iterator[X] =
    if rule.loops == 0 then Iterator.empty
    else
      (0 until rule.order)
        .iterator
        .filter(number => rule.linked(number, number))
        .map(number => edge(number, number))

  /**
    * The number of arcs which add nothing to the edge they repeat, because the
    * edges form a set.
    */
  private val merged: Int = if M.additive then 0 else rule.repeats(directed)

  /** @return The edge for the arc from [[source]] to [[target]]. */
  private def edge(source: Int, target: Int): X = link(
    index.vertex(source),
    index.vertex(target),
  )

  /**
    * @return
    *   The multiplicity of the edge between [[first]] and [[second]], from the
    *   arcs it stands for.
    */
  private def copies(first: Int, second: Int): M = M.fromInt:
    rule.arcs(first, second) +
      (if directed || first == second then 0 else rule.arcs(second, first))

  /**
    * @return
    *   The arc of the shape which [[edge]] stands for, as the numbers of its
    *   source and target, if there is one.
    */
  private def arc(edge: Any): Option[(Int, Int)] = indices(edge).collect:
    case (source, target) if rule.linked(source, target) => (source, target)
    case (target, source) if !directed && rule.linked(source, target) =>
      (source, target)

  /**
    * @return
    *   The numbers of the ends of [[edge]], if it's an unlabelled edge of the
    *   right kind between vertices of the shape.
    */
  private def indices(edge: Any): Option[(Int, Int)] = ends(edge).flatMap:
    (first, second) => index.index(first).zip(index.index(second))

/** The companion object for [[Shape]]. */
private[kneiphof] object Shape:

  /**
    * @return
    *   The undirected shape [[rule]] makes on the vertices [[index]] names.
    */
  def undirected[V, M : Multiplicity]
    (index: Index[V], rule: Rule)
    : Shape[V, M, Edge[V, Unit]] = Shape(
    index,
    rule,
    Edge(_, _),
    pair,
    directed = false,
  )

  /**
    * @return
    *   The directed shape [[rule]] makes on the vertices [[index]] names.
    */
  def directed[V, M : Multiplicity]
    (index: Index[V], rule: Rule)
    : Shape[V, M, Arc[V, Unit]] = Shape(
    index,
    rule,
    DirectedEdge(_, _),
    arrow,
    directed = true,
  )

  /** @return The ends of [[edge]], if it's an unlabelled undirected edge. */
  private def pair(edge: Any): Option[(Any, Any)] = edge match
    case IncidenceStructure.Pair(first, second, ()) => Some((first, second))
    case _                                          => None

  /** @return The ends of [[edge]], if it's an unlabelled directed edge. */
  private def arrow(edge: Any): Option[(Any, Any)] = edge match
    case IncidenceStructure.Arrow(source, target, ()) => Some((source, target))
    case _                                            => None
