package com.alecdorrington
package kneiphof
package shapes

import com.alecdorrington.kneiphof.views.Queries

/**
  * The queries about the unlabelled graph whose edges are the arcs of a
  * [[Shape]], answered from the shape alone.
  *
  * @param link
  *   The edge for an arc between two vertices.
  * @param ends
  *   The ends of an unlabelled edge of the right kind, if it is one.
  * @param directed
  *   `true` if and only if an edge only stands for the arc in its own
  *   direction, rather than in either.
  */
private[kneiphof] final class Shaping[V, M : Multiplicity as M, X]
  (
    indexing: Indexing[V],
    shape: Shape,
    link: (V, V) => X,
    ends: Any => Option[(Any, Any)],
    directed: Boolean,
  )
  extends Queries[V, Unit, M, X]:

  override lazy val vertexValues: ValueSet[V] =
    ValueSet.from((0 until shape.order).map(indexing.vertex))

  override def hasVertex(vertex: Any): Boolean = indexing
    .index(vertex)
    .isDefined

  override def multiplicity(edge: Any): M =
    if arc(edge).isDefined then M.one else M.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = ValueSet.from:
    indexing
      .index(vertex)
      .toSeq
      .flatMap: index =>
        shape.arcsFrom(index).map(edge(index, _)) ++
          shape.arcsInto(index).map(edge(_, index))

  override def find(edge: Any): Option[X] =
    arc(edge).map((source, target) => this.edge(source, target))

  override def size: Option[Int] = Some(shape.size)

  override def degree(vertex: Any): Option[Int] = Some:
    indexing
      .index(vertex)
      .fold(0)(index => shape.arcsFrom(index).size + shape.arcsInto(index).size)

  /** Every edge of the shape. */
  def edges: Iterable[X] = (0 until shape.order).flatMap: source =>
    shape.arcsFrom(source).map(edge(source, _))

  /** @return The edge for the arc from [[source]] to [[target]]. */
  private def edge(source: Int, target: Int): X = link(
    indexing.vertex(source),
    indexing.vertex(target),
  )

  /**
    * @return
    *   The arc of the shape which [[edge]] stands for, as the numbers of its
    *   source and target, if there is one.
    */
  private def arc(edge: Any): Option[(Int, Int)] = ends(edge).flatMap:
    (first, second) =>
      (indexing.index(first), indexing.index(second)) match
        case (Some(source), Some(target)) if shape.linked(source, target) =>
          Some((source, target))
        case (Some(target), Some(source))
          if !directed && shape.linked(source, target) => Some((source, target))
        case _ => None

/** The companion object for [[Shaping]]. */
private[kneiphof] object Shaping:

  /** @return The queries about the undirected graph of [[shape]]. */
  def undirected[V, M : Multiplicity]
    (indexing: Indexing[V], shape: Shape)
    : Shaping[V, M, Edge[V, Unit]] = Shaping(
    indexing,
    shape,
    Edge(_, _),
    pair,
    directed = false,
  )

  /** @return The queries about the directed graph of [[shape]]. */
  def directed[V, M : Multiplicity]
    (indexing: Indexing[V], shape: Shape)
    : Shaping[V, M, Arc[V, Unit]] = Shaping(
    indexing,
    shape,
    DirectedEdge(_, _),
    arrow,
    directed = true,
  )

  /** @return The ends of [[edge]], if it's an unlabelled undirected edge. */
  private def pair(edge: Any): Option[(Any, Any)] = edge match
    case Queries.Pair(first, second, ()) => Some((first, second))
    case _                               => None

  /** @return The ends of [[edge]], if it's an unlabelled directed edge. */
  private def arrow(edge: Any): Option[(Any, Any)] = edge match
    case Queries.Arrow(source, target, ()) => Some((source, target))
    case _                                 => None
