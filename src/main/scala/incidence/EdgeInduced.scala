package com.alecdorrington
package kneiphof
package incidence

/**
  * The subgraph of the graph whose incidence structure is [[base]] induced by
  * the edges satisfying every one of [[keeps]], with only the vertices incident
  * to one of them.
  *
  * A vertex is kept if and only if a kept edge is at it, so a query only looks
  * at the edges at the vertices it asks about, never at their neighbours'
  * edges. An edge-induced subgraph of one is one, keeping the edges which
  * satisfy both.
  */
private[kneiphof] final class EdgeInduced[
  +V,
  M : Multiplicity as M,
  +X <: AnyEdge[?, ?, ?],
] private (
  base: IncidenceStructure[V, M, X],
  keeps: Seq[X => Boolean],
) extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] = base
    .vertexValues
    .filter(hasVertex)

  override def hasVertex(vertex: Any): Boolean = base
    .incidentEdges(vertex)
    .exists((edge, _) => keep(edge))

  override def vertexValue(vertex: Any): Option[V] = base
    .vertexValue(vertex)
    .filter(hasVertex)

  override def multiplicity(edge: Any): M =
    if find(edge).isDefined then base.multiplicity(edge) else M.zero

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = base
    .incidentEdges(vertex)
    .filter((edge, _) => keep(edge))

  override def find(edge: Any): Option[X] = base.find(edge).filter(keep)

  /**
    * @return
    *   This subgraph, keeping only the edges which also satisfy [[predicate]].
    */
  def restrict(predicate: X => Boolean): EdgeInduced[V, M, X] =
    EdgeInduced(base, keeps :+ predicate)

  /** @return `true` if and only if [[edge]] satisfies every predicate. */
  private def keep(edge: X): Boolean = keeps.forall(_(edge))

/** The companion object for [[EdgeInduced]]. */
private[kneiphof] object EdgeInduced:

  /**
    * @return
    *   The subgraph of the graph whose incidence structure is [[base]] induced
    *   by the edges satisfying [[keep]], restricting [[base]] further if it's
    *   already an edge-induced subgraph.
    */
  def of[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      base: IncidenceStructure[V, M, X],
      keep: X => Boolean,
    )
    : EdgeInduced[V, M, X] = base match
    case base: EdgeInduced[V, M, X] => base.restrict(keep)
    case _                          => EdgeInduced(base, Seq(keep))
