package com.alecdorrington
package kneiphof

/**
  * The join of [[left]] and [[right]], answering queries about vertices and
  * edges from the two operands, without building the new edges between them.
  * Anything which needs every edge, such as iterating over them, is answered by
  * the joined graph, which is only built when first needed.
  *
  * @tparam L
  *   The label of the new edges.
  */
private[kneiphof] trait Joining[
  V,
  E,
  L,
  M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
] extends Delegating[V, E | L, M, X, G]:
  this: G[V, E | L] =>

  /** The graph from which the new edges lead. */
  protected def left: G[V, E]

  /** The graph to which the new edges lead. */
  protected def right: G[V, E]

  /**
    * @return
    *   The new edge from [[source]], in [[left]], to [[target]], in [[right]].
    */
  protected def link(source: V, target: V): X[V, E | L]

  /** @return The number of new edges equal to [[edge]]. */
  protected def links(edge: Any): Int

  override lazy val vertexValues: ValueSet[V] = left.vertexValues |
    right.vertexValues

  override def empty: G[Nothing, Nothing] = left.empty

  override def hasVertex(vertex: Any): Boolean = left.hasVertex(vertex) ||
    right.hasVertex(vertex)

  override def multiplicity(edge: Any): M = left.multiplicity(edge) +
    right.multiplicity(edge) + algebra.fromInt(links(edge))

  override def hasEdge(edge: Any): Boolean = multiplicity(edge).nonZero

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E | L]] = left
    .incidentEdgeValues(vertex) | right.incidentEdgeValues(vertex) |
    ValueSet.from(linksAt(vertex))

  override def size: Int =
    if left.vertexValues.exists(right.hasVertex) then delegate.size
    else
      Math.addExact(
        left.size + right.size,
        Math.multiplyExact(left.order, right.order),
      )

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
