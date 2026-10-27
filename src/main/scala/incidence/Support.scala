package com.alecdorrington
package kneiphof
package incidence

/**
  * The incidence structure of [[base]] with its multiplicities read in [[M]] by
  * [[embed]], such as the support of a multigraph, read as an edge set.
  */
private[kneiphof] final class Support[
  V,
  N,
  M : Multiplicity,
  X <: AnyEdge[?, ?, ?],
]
  (
    base: IncidenceStructure[V, N, X],
    embed: Embedding[N, M],
  )
  extends IncidenceStructure[V, M, X]:

  override def vertexValues: ValueSet[V] = base.vertexValues

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex)

  override def multiplicity(edge: Any): M = embed(base.multiplicity(edge))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = base
    .incidentEdges(vertex)
    .map((edge, multiplicity) => edge -> embed(multiplicity))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = base
    .incidentEdgeValues(vertex)

  override def find(edge: Any): Option[X] = base.find(edge)

  override def edges: Iterator[(X, M)] = base
    .edges
    .map((edge, multiplicity) => edge -> embed(multiplicity))

  override def size: Int = if embed.exact then base.size else super.size

  override def degree(vertex: Any): Int =
    if embed.exact then base.degree(vertex) else super.degree(vertex)

  override def stored: Boolean = base.stored
