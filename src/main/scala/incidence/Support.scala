package com.alecdorrington
package kneiphof
package incidence

/**
  * The incidence structure of [[base]] with each multiplicity read in [[M]] by
  * [[convert]], such as the support of a multigraph read as an edge set, or an
  * edge set read as a multigraph with one copy of each edge.
  *
  * @param exact
  *   `true` if and only if [[convert]] keeps the number of copies of every
  *   edge, so that sizes and degrees are unchanged.
  */
private[kneiphof] final class Support[
  V,
  N,
  M : Multiplicity,
  X <: AnyEdge[?, ?, ?],
]
  (
    base: IncidenceStructure[V, N, X],
    convert: N => M,
    exact: Boolean,
  )
  extends IncidenceStructure[V, M, X]:

  override def vertexValues: ValueSet[V] = base.vertexValues

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex)

  override def multiplicity(edge: Any): M = convert(base.multiplicity(edge))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = base
    .incidentEdges(vertex)
    .map((edge, multiplicity) => edge -> convert(multiplicity))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = base
    .incidentEdgeValues(vertex)

  override def find(edge: Any): Option[X] = base.find(edge)

  override def edges: Iterator[(X, M)] = base
    .edges
    .map((edge, multiplicity) => edge -> convert(multiplicity))

  override def size: Int = if exact then base.size else super.size

  override def degree(vertex: Any): Int =
    if exact then base.degree(vertex) else super.degree(vertex)

  override def stored: Boolean = base.stored

/** The companion object for [[Support]]. */
private[kneiphof] object Support:

  /**
    * @return
    *   The edge set whose incidence structure is [[base]], read as a multigraph
    *   with one copy of each edge.
    */
  def counted[V, X <: AnyEdge[?, ?, ?]]
    (base: IncidenceStructure[V, Boolean, X])
    : IncidenceStructure[V, Count, X] = Support(
    base,
    present => Count(if present then 1 else 0),
    exact = true,
  )
