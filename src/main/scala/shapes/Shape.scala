package com.alecdorrington
package kneiphof
package shapes

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
