package com.alecdorrington
package kneiphof
package views

/**
  * The queries about the join of [[left]] and [[right]], answered from the two
  * operands without building the new edges between them.
  *
  * @param link
  *   The new edge from a vertex of [[left]] to a vertex of [[right]].
  * @param links
  *   The number of new edges equal to an edge.
  */
private[kneiphof] final class Joining[
  V,
  E,
  L,
  M : Multiplicity as M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
]
  (
    left: G[V, E],
    right: G[V, E],
    link: (V, V) => X[V, E | L],
    links: Any => Int,
  )
  extends Queries[V, E | L, M, X[V, E | L]]:

  override lazy val vertexValues: ValueSet[V] = left.vertexValues |
    right.vertexValues

  override def hasVertex(vertex: Any): Boolean = left.hasVertex(vertex) ||
    right.hasVertex(vertex)

  override def multiplicity(edge: Any): M = left.multiplicity(edge) +
    right.multiplicity(edge) + M.fromInt(links(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E | L]] = left
    .incidentEdgeValues(vertex) | right.incidentEdgeValues(vertex) |
    ValueSet.from(linksAt(vertex))

  override def size: Option[Int] = Option.when(disjoint):
    Math.addExact(
      left.size + right.size,
      Math.multiplyExact(left.order, right.order),
    )

  override def degree(vertex: Any): Option[Int] = Option.when(disjoint):
    if left.hasVertex(vertex) then left.degree(vertex) + right.order
    else if right.hasVertex(vertex) then right.degree(vertex) + left.order
    else 0

  /** `true` if and only if [[left]] and [[right]] share no vertex. */
  private lazy val disjoint: Boolean =
    !left.vertexValues.exists(right.hasVertex)

  /** @return The new edges incident to [[vertex]]. */
  private def linksAt(vertex: Any): Iterable[X[V, E | L]] = left
    .vertexValues
    .get(vertex)
    .toSeq
    .flatMap(source => right.vertexValues.map(link(source, _))) ++
    right
      .vertexValues
      .get(vertex)
      .toSeq
      .flatMap(target => left.vertexValues.map(link(_, target)))

/** The companion object for [[Joining]]. */
private[kneiphof] object Joining:

  /**
    * @return
    *   The number of undirected edges labelled [[label]] between a vertex of
    *   [[left]] and a vertex of [[right]] which equal [[edge]].
    */
  def undirectedLinks
    (
      left: AnyGraph[?, ?, ?],
      right: AnyGraph[?, ?, ?],
      label: Any,
      edge: Any,
    )
    : Int = edge match
    case Edge(first, second, `label`) if first == second =>
      indicator(left.hasVertex(first) && right.hasVertex(first))
    case Edge(first, second, `label`) =>
      indicator(left.hasVertex(first) && right.hasVertex(second)) +
        indicator(left.hasVertex(second) && right.hasVertex(first))
    case _ => 0

  /**
    * @return
    *   The number of directed edges labelled [[label]] from a vertex of
    *   [[left]] to a vertex of [[right]] which equal [[edge]].
    */
  def directedLinks
    (
      left: AnyGraph[?, ?, ?],
      right: AnyGraph[?, ?, ?],
      label: Any,
      edge: Any,
    )
    : Int = edge match
    case DirectedEdge(source, target, `label`) =>
      indicator(left.hasVertex(source) && right.hasVertex(target))
    case _ => 0

  /** @return `1` if [[condition]] holds, and `0` otherwise. */
  private def indicator(condition: Boolean): Int = if condition then 1 else 0
