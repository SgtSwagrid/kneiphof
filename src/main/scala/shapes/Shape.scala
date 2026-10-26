package com.alecdorrington
package kneiphof
package shapes

import scala.collection.Searching.{Found, InsertionPoint}

/**
  * The rule for the edges of a simple graph on the vertex indices
  * `0 until order`, from which a shape's edges are computed when needed rather
  * than stored.
  *
  * Each edge is an arc from one index to another. In undirected shapes, each
  * edge is listed as a single arc, in either direction.
  */
private[kneiphof] trait Shape:

  /** The number of vertices. */
  def order: Int

  /** The number of arcs. */
  def size: Int

  /** @return The indices which an arc leads to from [[vertex]]. */
  def arcsFrom(vertex: Int): Iterable[Int]

  /** @return The indices from which an arc leads to [[vertex]]. */
  def arcsInto(vertex: Int): Iterable[Int]

  /**
    * @return
    *   `true` if and only if there is an arc from [[source]] to [[target]].
    */
  def linked(source: Int, target: Int): Boolean

/** The companion object for [[Shape]]. */
private[kneiphof] object Shape:

  /** Each vertex linked to every later one, so that every pair is joined once. */
  final case class Clique(override val order: Int) extends Shape:

    override def size: Int = Math.toIntExact(order.toLong * (order - 1) / 2)

    override def arcsFrom(vertex: Int): Iterable[Int] = vertex + 1 until order

    override def arcsInto(vertex: Int): Iterable[Int] = 0 until vertex

    override def linked(source: Int, target: Int): Boolean = source < target

  /** Each vertex linked to every other one. */
  final case class CompleteDigraph(override val order: Int) extends Shape:

    override def size: Int = Math.toIntExact(order.toLong * (order - 1))

    override def arcsFrom(vertex: Int): Iterable[Int] = others(vertex)

    override def arcsInto(vertex: Int): Iterable[Int] = others(vertex)

    override def linked(source: Int, target: Int): Boolean = source != target

    /** @return Every index except [[vertex]]. */
    private def others(vertex: Int): Iterable[Int] = (0 until order)
      .view
      .filter(_ != vertex)

  /** Each vertex linked to the next. */
  final case class Path(override val order: Int) extends Shape:

    override def size: Int = (order - 1).max(0)

    override def arcsFrom(vertex: Int): Iterable[Int] = Option
      .when(vertex + 1 < order)(vertex + 1)
      .toList

    override def arcsInto(vertex: Int): Iterable[Int] = Option
      .when(vertex > 0)(vertex - 1)
      .toList

    override def linked(source: Int, target: Int): Boolean = target ==
      source + 1

  /** Each vertex linked to the next, and the last to the first. */
  final case class Cycle(override val order: Int) extends Shape:

    override def size: Int = order

    override def arcsFrom(vertex: Int): Iterable[Int] = List(next(vertex))

    override def arcsInto(vertex: Int): Iterable[Int] =
      List((vertex - 1 + order) % order)

    override def linked(source: Int, target: Int): Boolean = target ==
      next(source)

    /** @return The index after [[vertex]], going round the cycle. */
    private def next(vertex: Int): Int = (vertex + 1) % order

  /** The first vertex (the centre) linked to every other one. */
  final case class Star(override val order: Int) extends Shape:

    override def size: Int = (order - 1).max(0)

    override def arcsFrom(vertex: Int): Iterable[Int] =
      if vertex == 0 then 1 until order else Nil

    override def arcsInto(vertex: Int): Iterable[Int] =
      if vertex == 0 then Nil else List(0)

    override def linked(source: Int, target: Int): Boolean = source == 0 &&
      target > 0

  /** The first vertex (the hub) linked to every other one, which form a cycle. */
  final case class Wheel(override val order: Int) extends Shape:

    override def size: Int = 2 * (order - 1)

    override def arcsFrom(vertex: Int): Iterable[Int] =
      if vertex == 0 then 1 until order else List(next(vertex))

    override def arcsInto(vertex: Int): Iterable[Int] =
      if vertex == 0 then Nil else List(0, previous(vertex))

    override def linked(source: Int, target: Int): Boolean =
      if source == 0 then target > 0 else target == next(source)

    /** @return The rim index after [[vertex]], going round the rim. */
    private def next(vertex: Int): Int =
      if vertex == order - 1 then 1 else vertex + 1

    /** @return The rim index before [[vertex]], going round the rim. */
    private def previous(vertex: Int): Int =
      if vertex == 1 then order - 1 else vertex - 1

  /** The first vertex (the hub) linked to every other one, which form a path. */
  final case class Fan(override val order: Int) extends Shape:

    override def size: Int = (order - 1).max(0) + (order - 2).max(0)

    override def arcsFrom(vertex: Int): Iterable[Int] =
      if vertex == 0 then 1 until order
      else Option.when(vertex + 1 < order)(vertex + 1).toList

    override def arcsInto(vertex: Int): Iterable[Int] =
      if vertex == 0 then Nil
      else 0 :: Option.when(vertex > 1)(vertex - 1).toList

    override def linked(source: Int, target: Int): Boolean =
      if source == 0 then target > 0 else target == source + 1

  /**
    * Each vertex linked to its children, numbered in breadth-first order, so
    * that the children of `v` are `branching * v + 1` to `branching * v +
    * branching`.
    */
  final case class Tree(override val order: Int, branching: Int) extends Shape:

    override def size: Int = (order - 1).max(0)

    override def arcsFrom(vertex: Int): Iterable[Int] =
      val first = vertex.toLong * branching + 1
      if first >= order then Nil
      else first.toInt until (first + branching).min(order.toLong).toInt

    override def arcsInto(vertex: Int): Iterable[Int] = Option
      .when(vertex > 0)(parent(vertex))
      .toList

    override def linked(source: Int, target: Int): Boolean = target > 0 &&
      parent(target) == source

    /** @return The index of the parent of [[vertex]]. */
    private def parent(vertex: Int): Int = (vertex - 1) / branching

  /**
    * Every two vertices in different parts linked, where the parts are runs of
    * consecutive indices with the given [[sizes]]. In directed shapes, they're
    * linked both ways.
    */
  final case class Multipartite
    (sizes: Seq[Int], directed: Boolean)
    extends Shape:

    override val order: Int = Math.toIntExact(sizes.map(_.toLong).sum)

    override def size: Int = Math.toIntExact(
      (order.toLong * order - sizes.map(size => size.toLong * size).sum) / 2 *
        (if directed then 2 else 1),
    )

    override def arcsFrom(vertex: Int): Iterable[Int] =
      if directed then others(vertex) else end(vertex) until order

    override def arcsInto(vertex: Int): Iterable[Int] =
      if directed then others(vertex) else 0 until start(vertex)

    override def linked(source: Int, target: Int): Boolean =
      part(source) != part(target) && (directed || source < target)

    /** The first index of each nonempty part, followed by the order. */
    private val starts: IndexedSeq[Int] = sizes
      .filter(_ > 0)
      .scanLeft(0)(_ + _)
      .toIndexedSeq

    /** @return The number of the part containing [[vertex]]. */
    private def part(vertex: Int): Int = starts.search(vertex) match
      case Found(part)          => part
      case InsertionPoint(next) => next - 1

    /** @return The first index of the part containing [[vertex]]. */
    private def start(vertex: Int): Int = starts(part(vertex))

    /** @return The index after the part containing [[vertex]]. */
    private def end(vertex: Int): Int = starts(part(vertex) + 1)

    /** @return The indices outside the part containing [[vertex]]. */
    private def others(vertex: Int): Iterable[Int] = (0 until start(vertex))
      .view ++ (end(vertex) until order)

  /**
    * Each vertex linked to those with one more bit set, so that two vertices
    * are adjacent when they differ in exactly one bit.
    */
  final case class Hypercube(dimension: Int) extends Shape:

    override def order: Int = 1 << dimension

    override def size: Int = Math.toIntExact(dimension.toLong * order / 2)

    override def arcsFrom(vertex: Int): Iterable[Int] = bits
      .filter(bit => (vertex & bit) == 0)
      .map(vertex | _)

    override def arcsInto(vertex: Int): Iterable[Int] = bits
      .filter(bit => (vertex & bit) != 0)
      .map(vertex ^ _)

    override def linked(source: Int, target: Int): Boolean = source < target &&
      Integer.bitCount(source ^ target) == 1

    /** Each bit of an index, as a power of `2`. */
    private def bits: Seq[Int] = (0 until dimension).map(1 << _)

  /**
    * Each vertex `i` linked to `i + jump`, going round the indices, for each of
    * the [[jumps]], none of which is a multiple of the order.
    */
  final case class Circulant
    (
      override val order: Int,
      jumps: Seq[Int],
      directed: Boolean,
    )
    extends Shape:

    override def size: Int = Math.toIntExact:
      steps.map(step => if halving(step) then order / 2L else order.toLong).sum

    override def arcsFrom(vertex: Int): Iterable[Int] = steps
      .filter(step => !halving(step) || vertex < step)
      .map(shift(vertex, _))

    override def arcsInto(vertex: Int): Iterable[Int] = steps
      .filter(step => !halving(step) || vertex >= step)
      .map(step => shift(vertex, -step))

    override def linked(source: Int, target: Int): Boolean = arcsFrom(source)
      .exists(_ == target)

    /**
      * The distinct steps forward which the jumps make. In undirected shapes,
      * jumping forwards or backwards by the same amount links the same
      * vertices, so only the shorter of the two is kept.
      */
    private val steps: Seq[Int] = jumps
      .map(Math.floorMod(_, order))
      .map(step => if directed then step else step.min(order - step))
      .distinct

    /**
      * @return
      *   `true` if and only if [[step]] links opposite vertices of an
      *   undirected shape, so that each edge is listed from its lower end only.
      */
    private def halving(step: Int): Boolean = !directed && 2 * step == order

    /** @return The index [[step]] after [[vertex]], going round the indices. */
    private def shift(vertex: Int, step: Int): Int = Math
      .floorMod(vertex.toLong + step, order.toLong)
      .toInt

  /**
    * The generalised Petersen graph `GP(n, k)`, whose outer indices `0 until n`
    * form a cycle, each linked by a spoke to the inner index `n + i`, with each
    * inner index linked to the one [[k]] places further round. Requires
    * `0 < k < n / 2`.
    */
  final case class Petersen(n: Int, k: Int) extends Shape:

    override def order: Int = 2 * n

    override def size: Int = 3 * n

    override def arcsFrom(vertex: Int): Iterable[Int] =
      if vertex < n then List(around(vertex, 1), n + vertex)
      else List(n + around(vertex - n, k))

    override def arcsInto(vertex: Int): Iterable[Int] =
      if vertex < n then List(around(vertex, -1))
      else List(vertex - n, n + around(vertex - n, -k))

    override def linked(source: Int, target: Int): Boolean = arcsFrom(source)
      .exists(_ == target)

    /** @return The index [[step]] places after [[index]], going round `n`. */
    private def around(index: Int, step: Int): Int =
      Math.floorMod(index + step, n)

  /**
    * The Cartesian product of [[first]] and [[second]], on the pairs of their
    * indices `(row, column)`, numbered `row * second.order + column`. Each pair
    * is linked to the pairs which differ from it in one coordinate, by an arc
    * of the shape for that coordinate.
    */
  final case class Cartesian(first: Shape, second: Shape) extends Shape:

    override def order: Int = Math.multiplyExact(first.order, second.order)

    override def size: Int = Math.toIntExact:
      first.size.toLong * second.order + first.order.toLong * second.size

    override def arcsFrom(vertex: Int): Iterable[Int] =
      along(vertex)(first.arcsFrom, second.arcsFrom)

    override def arcsInto(vertex: Int): Iterable[Int] =
      along(vertex)(first.arcsInto, second.arcsInto)

    override def linked(source: Int, target: Int): Boolean = column(source) ==
      column(target) && first.linked(row(source), row(target)) || row(
        source,
      ) == row(target) && second.linked(column(source), column(target))

    /**
      * @return
      *   The indices linked to [[vertex]] within its column by [[rows]], and
      *   within its row by [[columns]].
      */
    private def along
      (vertex: Int)
      (
        rows: Int => Iterable[Int],
        columns: Int => Iterable[Int],
      )
      : Iterable[Int] = rows(row(vertex)).map(index(_, column(vertex))) ++
      columns(column(vertex)).map(index(row(vertex), _))

    /** @return The index of [[vertex]] in [[first]]. */
    private def row(vertex: Int): Int = vertex / second.order

    /** @return The index of [[vertex]] in [[second]]. */
    private def column(vertex: Int): Int = vertex % second.order

    /** @return The index of the pair `(row, column)`. */
    private def index(row: Int, column: Int): Int = row * second.order + column
