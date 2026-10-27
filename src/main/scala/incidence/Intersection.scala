package com.alecdorrington
package kneiphof
package incidence

/**
  * The intersection of the graphs whose incidence structures are [[base]] and
  * [[other]], with their common vertices and the meet of each edge's
  * multiplicities. Its edges are those of [[base]], found among the edges of
  * [[other]].
  */
private[kneiphof] final class Intersection[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    base: IncidenceStructure[V, M, X],
    other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
  )
  extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] = base
    .vertexValues
    .filter(other.hasVertex)

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex) &&
    other.hasVertex(vertex)

  override def multiplicity(edge: Any): M = base.multiplicity(edge) &
    other.multiplicity(edge)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    if !hasVertex(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .map((edge, multiplicity) =>
          edge -> (multiplicity & other.multiplicity(edge)),
        )
        .filter(_(1).nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  override def edges: Iterator[(X, M)] = other
    .edges
    .flatMap((edge, multiplicity) =>
      base
        .find(edge)
        .map(found => found -> (base.multiplicity(found) & multiplicity)),
    )
    .filter(_(1).nonZero)

  override lazy val size: Int = IncidenceStructure.total(edges.map(_(1).count))
