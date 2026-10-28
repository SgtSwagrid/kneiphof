package com.alecdorrington
package kneiphof
package incidence

/**
  * The subgraph of the graph whose incidence structure is [[base]], with only
  * the vertices satisfying every one of [[keeps]], and the edges between them
  * satisfying every one of [[edgeKeeps]]. A subgraph of a subgraph is one
  * subgraph, keeping what satisfies both, which checks the predicates in turn
  * rather than nesting them, so that long chains of filters stay shallow.
  */
private[kneiphof] final class Subgraph[
  +V,
  M : Multiplicity as M,
  +X <: AnyEdge[?, ?, ?],
] private (
  base: IncidenceStructure[V, M, X],
  keeps: Seq[V => Boolean],
  edgeKeeps: Seq[X => Boolean],
) extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] = base.vertexValues.filter(keep)

  override def vertexIterator: Iterator[V] = base.vertexIterator.filter(keep)

  override def hasVertex(vertex: Any): Boolean = vertexValue(vertex).isDefined

  override def maxOrder: Option[Long] = base.maxOrder

  override def vertexValue(vertex: Any): Option[V] = base
    .vertexValue(vertex)
    .filter(keep)

  override def multiplicity(edge: Any): M =
    if !IncidenceStructure.ends(edge).forall(hasVertex) then M.zero
    else if edgeKeeps.nonEmpty && !base.find(edge).exists(keepEdge) then M.zero
    else base.multiplicity(edge)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    if !hasVertex(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .filter((edge, _) =>
          edge.vertexValues.forall(hasVertex) && keepEdge(edge),
        )

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  /**
    * The number of edges, counted from those at the removed vertices when every
    * edge between the remaining ones is kept, and otherwise from every edge.
    */
  override lazy val size: Int =
    if edgeKeeps.nonEmpty then super.size
    else Math.subtractExact(base.size, detachment)

  /**
    * @return
    *   This subgraph, keeping only the vertices which also satisfy
    *   [[predicate]], and the edges which also satisfy [[edgePredicate]], if
    *   given.
    */
  def restrict
    (
      predicate: V => Boolean,
      edgePredicate: Option[X => Boolean],
    )
    : Subgraph[V, M, X] = Subgraph(
    base,
    keeps :+ predicate,
    edgeKeeps ++ edgePredicate,
  )

  /** @return `true` if and only if [[vertex]] satisfies every predicate. */
  private def keep(vertex: V): Boolean = keeps.forall(_(vertex))

  /** @return `true` if and only if [[edge]] satisfies every edge predicate. */
  private def keepEdge(edge: X): Boolean = edgeKeeps.forall(_(edge))

  /** The removed vertices, in the order they're counted in. */
  private lazy val removed: IndexedSeq[V] = base
    .vertexValues
    .filterNot(keep)
    .toIndexedSeq

  /** The position of each removed vertex in [[removed]]. */
  private lazy val positions: Map[Any, Int] = removed
    .view
    .zipWithIndex
    .map((vertex, position) => (vertex: Any) -> position)
    .toMap

  /**
    * The number of edges lost by removing vertices, counting each edge once, at
    * its first removed end, without gathering them.
    */
  private def detachment: Int = IncidenceStructure.total:
    removed
      .iterator
      .zipWithIndex
      .flatMap: (vertex, position) =>
        base
          .incidentEdges(vertex)
          .iterator
          .collect:
            case (edge, multiplicity) if first(edge, position) =>
              multiplicity.count

  /**
    * @return
    *   `true` if and only if no removed end of [[edge]] comes before the one at
    *   [[position]].
    */
  private def first(edge: AnyEdge[?, ?, ?], position: Int): Boolean = edge
    .vertexValues
    .forall(positions.get(_).forall(_ >= position))

/** The companion object for [[Subgraph]]. */
private[kneiphof] object Subgraph:

  /**
    * @return
    *   The subgraph of the graph whose incidence structure is [[base]], with
    *   only the vertices satisfying [[keep]], and the edges between them
    *   satisfying [[keepEdge]], if given, restricting [[base]] further if it's
    *   already a subgraph.
    */
  def of[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      base: IncidenceStructure[V, M, X],
      keep: V => Boolean,
      keepEdge: Option[X => Boolean],
    )
    : Subgraph[V, M, X] = base match
    case base: Subgraph[V, M, X] => base.restrict(keep, keepEdge)
    case _                       => Subgraph(base, Seq(keep), keepEdge.toSeq)
