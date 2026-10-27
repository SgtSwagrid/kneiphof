package com.alecdorrington
package kneiphof
package incidence

/**
  * The difference of the graphs whose incidence structures are [[base]] and
  * [[other]], with the vertices of [[base]], and the multiplicity of each edge
  * in [[other]] subtracted.
  */
private[kneiphof] final class Difference[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    base: IncidenceStructure[V, M, X],
    other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
  )
  extends IncidenceStructure[V, M, X]:

  override def vertexValues: ValueSet[V] = base.vertexValues

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex)

  override def multiplicity(edge: Any): M = base.multiplicity(edge) -
    other.multiplicity(edge)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = base
    .incidentEdges(vertex)
    .map((edge, multiplicity) =>
      edge -> (multiplicity - other.multiplicity(edge)),
    )
    .filter(_(1).nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  /** The number of edges, counted from those of [[other]], which are lost. */
  override lazy val size: Int = Math.addExact(
    base.size,
    IncidenceStructure.changes(this, base, other.edges.map(_(0))),
  )
