package com.alecdorrington
package kneiphof
package views

/**
  * The queries about [[graph]] with each vertex `v` relabelled `tag(v)`, which
  * must be injective with inverse [[untag]], answered from [[graph]] itself.
  */
private[kneiphof] final class Tagging[
  V,
  T,
  E,
  M : Multiplicity as M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
]
  (
    graph: G[V, E],
    tag: V => T,
    untag: Any => Option[V],
  )
  extends Queries[T, E, M, X[T, E]]:

  override lazy val vertexValues: ValueSet[T] =
    ValueSet.from(graph.vertexValues.map(tag))

  override def hasVertex(vertex: Any): Boolean =
    untag(vertex).exists(graph.hasVertex)

  override def multiplicity(edge: Any): M = edge match
    case edge: AnyEdge[?, ?, ?] =>
      graph.multiplicity(edge.mapVertices(original))
    case _ => M.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[X[T, E]] = ValueSet
    .from:
      untag(vertex)
        .toSeq
        .flatMap(graph.incidentEdgeValues(_).map(_.mapVertices(tag)))

  override def size: Option[Int] = Some(graph.size)

  override def degree(vertex: Any): Option[Int] =
    Some(untag(vertex).fold(0)(graph.degree))

  /**
    * @return
    *   The vertex of [[graph]] tagged as [[vertex]], or a placeholder which is
    *   never a vertex if there isn't one.
    */
  private def original(vertex: Any): Any =
    untag(vertex).getOrElse(Tagging.Untagged)

/** The companion object for [[Tagging]]. */
private[kneiphof] object Tagging:

  /** A placeholder for values which aren't tagged vertices. */
  private object Untagged
