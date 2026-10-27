package com.alecdorrington
package kneiphof
package incidence

/**
  * The Cartesian product of the graphs whose incidence structures are [[first]]
  * and [[second]], on the pairs `(v, w)` of their vertices. Each edge of
  * [[first]] has a copy `e.mapVertices((_, w))` for each vertex `w` of
  * [[second]], and each edge of [[second]] has a copy `f.mapVertices((v, _))`
  * for each vertex `v` of [[first]], keeping their labels.
  *
  * The two kinds of copy only coincide for edges with a single end, such as
  * loops: a loop at `v` and an equal loop at `w` both give a loop at `(v, w)`,
  * which is two copies in a multigraph, and one edge in an edge set.
  */
private[kneiphof] final class CartesianProduct[
  V,
  W,
  E,
  M : Multiplicity as M,
  X[+v, +e] <: AnyEdge[v, e, X],
]
  (
    first: IncidenceStructure[V, M, X[V, E]],
    second: IncidenceStructure[W, M, X[W, E]],
  )
  extends IncidenceStructure[(V, W), M, X[(V, W), E]]:

  override lazy val vertexValues: ValueSet[(V, W)] = ValueSet.from:
    for
      v <- first.vertexValues.iterator
      w <- second.vertexValues.iterator
    yield (v, w)

  override def hasVertex(vertex: Any): Boolean = coordinates(vertex).isDefined

  override def vertexValue(vertex: Any): Option[(V, W)] = coordinates(vertex)

  override def order: Int = Math.multiplyExact(first.order, second.order)

  override def knownOrder: Option[Long] =
    for
      rows    <- first.knownOrder
      columns <- second.knownOrder
      if rows == 0 || columns <= Long.MaxValue / rows
    yield rows * columns

  override def vertexIterator: Iterator[(V, W)] = first
    .vertexIterator
    .flatMap(v => second.vertexIterator.map((v, _)))

  override def sameAs(that: IncidenceStructure[?, ?, ?]): Boolean =
    (this eq that) ||
    that
      .factors
      .exists: (left, right) =>
        first.sameAs(left) && second.sameAs(right)

  override def factors
    : Option[(IncidenceStructure[?, ?, ?], IncidenceStructure[?, ?, ?])] =
    Some((first, second))

  override def multiplicity(edge: Any): M = edge match
    case edge: AnyEdge[?, ?, ?] if edge.vertexValues.forall(isPair) =>
      val ends = edge
        .vertexValues
        .toSeq
        .collect:
          case (v, w) => (v, w)
      val alongFirst = ends.map((_, w) => w).distinct match
        case Seq(w) if second.hasVertex(w) =>
          first.multiplicity(edge.mapVertices(CartesianProduct.left))
        case _ => M.zero
      val alongSecond = ends.map((v, _) => v).distinct match
        case Seq(v) if first.hasVertex(v) =>
          second.multiplicity(edge.mapVertices(CartesianProduct.right))
        case _ => M.zero
      alongFirst + alongSecond
    case _ => M.zero

  override def incidentEdges(vertex: Any): Iterable[(X[(V, W), E], M)] =
    coordinates(vertex).fold(Nil): (v, w) =>
      val copies = first
        .incidentEdgeValues(v)
        .iterator
        .map(_.mapVertices(end => (end, w))) ++
        second
          .incidentEdgeValues(w)
          .iterator
          .map(_.mapVertices(end => (v, end)))
      ValueSet.from(copies).toSeq.map(edge => edge -> multiplicity(edge))

  /**
    * Every edge: the copies of the edges of [[first]], then those of [[second]]
    * which aren't also copies of edges of [[first]].
    */
  override def edges: Iterator[(X[(V, W), E], M)] =
    val alongFirst =
      for
        (edge, _) <- first.edges
        w         <- second.vertexValues.iterator
      yield edge.mapVertices(end => (end, w))
    val alongSecond =
      for
        (edge, _) <- second.edges
        v         <- first.vertexValues.iterator
        copy = edge.mapVertices(end => (v, end))
        if !(edge.vertexValues.size == 1 &&
        first.multiplicity(copy.mapVertices(CartesianProduct.left)).nonZero)
      yield copy
    (alongFirst ++ alongSecond).map(edge => edge -> multiplicity(edge))

  /**
    * The number of edges: a copy of each edge of [[first]] for each vertex of
    * [[second]], and the other way round, less the coinciding loops if the
    * edges form a set.
    */
  override lazy val size: Int = Math.toIntExact:
    first.size.toLong * second.order + first.order.toLong * second.size -
      (if M.additive then 0L else coinciding)

  override def degree(vertex: Any): Int =
    if !M.additive then super.degree(vertex)
    else
      coordinates(vertex).fold(0): (v, w) =>
        Math.addExact(first.degree(v), second.degree(w))

  /** @return The vertices of [[first]] and [[second]] paired as [[vertex]]. */
  private def coordinates(vertex: Any): Option[(V, W)] = vertex match
    case (v, w) => first.vertexValue(v).zip(second.vertexValue(w))
    case _      => None

  /**
    * The number of pairs of a loop of [[first]] and a loop of [[second]] whose
    * copies coincide, which they do when they're the same but for their end.
    */
  private def coinciding: Long =
    val firsts  = CartesianProduct.shapes(first.loops)
    val seconds = CartesianProduct.shapes(second.loops)
    firsts
      .iterator
      .map((shape, count) =>
        Math.multiplyExact(
          count.toLong,
          seconds.getOrElse(shape, 0).toLong,
        ),
      )
      .foldLeft(0L)(Math.addExact)

  /** @return `true` if and only if [[vertex]] is a pair. */
  private def isPair(vertex: Any): Boolean = vertex match
    case (_, _) => true
    case _      => false

/** The companion object for [[CartesianProduct]]. */
private[kneiphof] object CartesianProduct:

  /** @return The first coordinate of [[vertex]], if it's a pair. */
  private def left(vertex: Any): Any = vertex match
    case (v, _) => v
    case _      => vertex

  /** @return The second coordinate of [[vertex]], if it's a pair. */
  private def right(vertex: Any): Any = vertex match
    case (_, w) => w
    case _      => vertex

  /**
    * @return
    *   The number of the given edges with a single end of each shape, which is
    *   the edge with its end replaced by `()`, so that equal shapes give equal
    *   copies at the same pair.
    */
  private def shapes(loops: Iterator[AnyEdge[?, ?, ?]]): Map[Any, Int] = loops
    .map(_.mapVertices(_ => ()))
    .toSeq
    .groupMapReduce(identity)(_ => 1)(_ + _)
