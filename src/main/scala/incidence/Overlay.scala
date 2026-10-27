package com.alecdorrington
package kneiphof
package incidence

/**
  * The overlay of the graphs whose incidence structures are [[first]] and
  * [[second]], with every vertex and edge of either, and each edge's
  * multiplicities combined by [[combine]]: a [[Sum]] or a [[Union]].
  */
private[kneiphof] abstract class Overlay[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    first: IncidenceStructure[V, M, X],
    second: IncidenceStructure[V, M, X],
  )
  extends IncidenceStructure[V, M, X]:

  /**
    * @return
    *   The multiplicity of an edge with multiplicities [[x]] and [[y]].
    */
  protected def combine(x: M, y: M): M

  override lazy val vertexValues: ValueSet[V] = first.vertexValues |
    second.vertexValues

  override def hasVertex(vertex: Any): Boolean = first.hasVertex(vertex) ||
    second.hasVertex(vertex)

  override def multiplicity(edge: Any): M = combine(
    first.multiplicity(edge),
    second.multiplicity(edge),
  )

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = first
    .incidentEdgeValues(vertex) | second.incidentEdgeValues(vertex)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    val firsts  = ValueMap.from(first.incidentEdges(vertex))
    val seconds = ValueMap.from(second.incidentEdges(vertex))
    (firsts.keys ++ seconds.keys.filterNot(firsts.contains)).map: edge =>
      edge -> combine(
        firsts.getOrElse(edge, M.zero),
        seconds.getOrElse(edge, M.zero),
      )

  override def find(edge: Any): Option[X] = first
    .find(edge)
    .orElse(second.find(edge))

  /** `true` if and only if the two graphs share no vertex. */
  protected lazy val disjoint: Boolean =
    !first.vertexValues.exists(second.hasVertex)
