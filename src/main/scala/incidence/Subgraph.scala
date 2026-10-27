package com.alecdorrington
package kneiphof
package incidence

/**
  * The subgraph of the graph whose incidence structure is [[base]], with only
  * the vertices satisfying [[keep]], and the edges between them satisfying
  * [[keepEdge]], if given.
  */
private[kneiphof] final class Subgraph[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    base: IncidenceStructure[V, M, X],
    keep: V => Boolean,
    keepEdge: Option[X => Boolean],
  )
  extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] = base.vertexValues.filter(keep)

  override def hasVertex(vertex: Any): Boolean = base
    .vertexValues
    .get(vertex)
    .exists(keep)

  override def multiplicity(edge: Any): M =
    if !IncidenceStructure.ends(edge).forall(hasVertex) then M.zero
    else
      keepEdge match
        case Some(predicate) if !base.find(edge).exists(predicate) => M.zero
        case _ => base.multiplicity(edge)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    if !hasVertex(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .filter((edge, _) =>
          edge.vertexValues.forall(hasVertex) && keepEdge.forall(_(edge)),
        )

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  /**
    * The number of edges, counted from those at the removed vertices when every
    * edge between the remaining ones is kept, and otherwise from every edge.
    */
  override lazy val size: Int =
    if keepEdge.isDefined then super.size
    else Math.subtractExact(base.size, detachment)

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
