package com.alecdorrington
package kneiphof
package incidence

/**
  * A copy of the graph whose incidence structure is [[base]], with each vertex
  * `v` relabelled `tag(v)`, where [[tag]] is injective with inverse [[untag]].
  * Disjoint unions and lexicographic products tag their parts this way.
  */
private[kneiphof] final class Copy[
  V,
  T,
  E,
  M : Multiplicity as M,
  X[+v, +e] <: AnyEdge[v, e, X],
]
  (
    base: IncidenceStructure[V, M, X[V, E]],
    tag: V => T,
    untag: Any => Option[V],
  )
  extends IncidenceStructure[T, M, X[T, E]]:

  override lazy val vertexValues: ValueSet[T] =
    ValueSet.from(base.vertexValues.map(tag))

  override def hasVertex(vertex: Any): Boolean =
    untag(vertex).exists(base.hasVertex)

  override def vertexValue(vertex: Any): Option[T] = untag(vertex)
    .filter(base.hasVertex)
    .map(tag)

  override def order: Int = base.order

  override def multiplicity(edge: Any): M = edge match
    case edge: AnyEdge[?, ?, ?] => base.multiplicity(edge.mapVertices(original))
    case _                      => M.zero

  override def incidentEdges(vertex: Any): Iterable[(X[T, E], M)] =
    untag(vertex)
      .toSeq
      .flatMap(base.incidentEdges)
      .map((edge, multiplicity) => edge.mapVertices(tag) -> multiplicity)

  override def edges: Iterator[(X[T, E], M)] = base
    .edges
    .map((edge, multiplicity) => edge.mapVertices(tag) -> multiplicity)

  override def size: Int = base.size

  override def listable: Boolean = base.listable

  override def degree(vertex: Any): Int = untag(vertex).fold(0)(base.degree)

  /**
    * @return
    *   The vertex of [[base]] tagged as [[vertex]], or a placeholder which is
    *   never a vertex if there isn't one.
    */
  private def original(vertex: Any): Any =
    untag(vertex).getOrElse(Copy.Untagged)

/** The companion object for [[Copy]]. */
private[kneiphof] object Copy:

  /** A placeholder for values which aren't tagged vertices. */
  private object Untagged
