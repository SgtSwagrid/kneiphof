package com.alecdorrington
package kneiphof
package incidence

/**
  * The join of the graphs whose incidence structures are [[left]] and
  * [[right]], answered from the two operands without building the new edges
  * between them.
  *
  * @param link
  *   The new edge from a vertex of [[left]] to a vertex of [[right]], labelled
  *   [[label]].
  * @param directed
  *   `true` if and only if the new edges are directed, from [[left]] to
  *   [[right]].
  */
private[kneiphof] final class Join[
  V,
  E,
  L,
  M : Multiplicity as M,
  X[+v, +e] <: AnyEdge[v, e, X],
]
  (
    left: IncidenceStructure[V, M, X[V, E]],
    right: IncidenceStructure[V, M, X[V, E]],
    link: (V, V) => X[V, E | L],
    label: L,
    directed: Boolean,
  )
  extends IncidenceStructure[V, M, X[V, E | L]]:

  override lazy val vertexValues: ValueSet[V] = left.vertexValues |
    right.vertexValues

  override def hasVertex(vertex: Any): Boolean = left.hasVertex(vertex) ||
    right.hasVertex(vertex)

  override def vertexValue(vertex: Any): Option[V] = right
    .vertexValue(vertex)
    .orElse(left.vertexValue(vertex))

  override def order: Int =
    if disjoint then Math.addExact(left.order, right.order)
    else vertexValues.size

  /**
    * The order, known when both operands' are, and they're known to be disjoint
    * by listing a few vertices of the smaller.
    */
  override def maxOrder: Option[Long] =
    IncidenceStructure.knownTotal(Seq(left.maxOrder, right.maxOrder))

  override def knownOrder: Option[Long] =
    (left.knownOrder, right.knownOrder) match
      case (Some(first), Some(second))
        if first.min(second) <= IncidenceStructure.indexable && disjoint =>
        IncidenceStructure.knownTotal(Seq(Some(first), Some(second)))
      case _ => None

  override def multiplicity(edge: Any): M = left.multiplicity(edge) +
    right.multiplicity(edge) + M.fromInt(links(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E | L]] = left
    .incidentEdgeValues(vertex) | right.incidentEdgeValues(vertex) |
    ValueSet.from(linksAt(vertex))

  override def incidentEdges(vertex: Any): Iterable[(X[V, E | L], M)] =
    incidentEdgeValues(vertex).toSeq.map(edge => edge -> multiplicity(edge))

  override lazy val size: Int =
    if !disjoint then super.size
    else
      Math.addExact(
        Math.addExact(left.size, right.size),
        Math.multiplyExact(left.order, right.order),
      )

  override def degree(vertex: Any): Int =
    if !disjoint then super.degree(vertex)
    else if left.hasVertex(vertex) then
      Math.addExact(left.degree(vertex), right.order)
    else if right.hasVertex(vertex) then
      Math.addExact(right.degree(vertex), left.order)
    else 0

  /**
    * `true` if and only if [[left]] and [[right]] share no vertex, found by
    * listing the vertices of whichever has fewer.
    */
  private lazy val disjoint: Boolean =
    val (smaller, larger) =
      if IncidenceStructure.orderBound(left) <=
          IncidenceStructure.orderBound(right)
      then (left, right)
      else (right, left)
    !smaller.vertexValues.exists(larger.hasVertex)

  /** @return The new edges incident to [[vertex]]. */
  private def linksAt(vertex: Any): Iterable[X[V, E | L]] = left
    .vertexValue(vertex)
    .toSeq
    .flatMap(source => right.vertexValues.map(link(source, _))) ++
    right
      .vertexValue(vertex)
      .toSeq
      .flatMap(target => left.vertexValues.map(link(_, target)))

  /** @return The number of new edges equal to [[edge]]. */
  private def links(edge: Any): Int =
    if directed then Join.directedLinks(left, right, label, edge)
    else Join.undirectedLinks(left, right, label, edge)

/** The companion object for [[Join]]. */
private[kneiphof] object Join:

  /**
    * @return
    *   The number of undirected edges labelled [[label]] between a vertex of
    *   [[left]] and a vertex of [[right]] which equal [[edge]].
    */
  private def undirectedLinks
    (
      left: IncidenceStructure[?, ?, ?],
      right: IncidenceStructure[?, ?, ?],
      label: Any,
      edge: Any,
    )
    : Int = edge match
    case IncidenceStructure.Pair(first, second, `label`) if first == second =>
      indicator(left.hasVertex(first) && right.hasVertex(first))
    case IncidenceStructure.Pair(first, second, `label`) =>
      indicator(left.hasVertex(first) && right.hasVertex(second)) +
        indicator(left.hasVertex(second) && right.hasVertex(first))
    case _ => 0

  /**
    * @return
    *   The number of directed edges labelled [[label]] from a vertex of
    *   [[left]] to a vertex of [[right]] which equal [[edge]].
    */
  private def directedLinks
    (
      left: IncidenceStructure[?, ?, ?],
      right: IncidenceStructure[?, ?, ?],
      label: Any,
      edge: Any,
    )
    : Int = edge match
    case IncidenceStructure.Arrow(source, target, `label`) =>
      indicator(left.hasVertex(source) && right.hasVertex(target))
    case _ => 0

  /** @return The number `1` if [[condition]] holds, and `0` otherwise. */
  private def indicator(condition: Boolean): Int = if condition then 1 else 0
