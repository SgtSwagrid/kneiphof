package com.alecdorrington
package kneiphof
package shapes

/** The vertices of a shape, numbered from `0`. */
private[kneiphof] trait Index[+V]:

  /** The number of vertices. */
  def order: Int

  /**
    * @return
    *   `true` if this index is known to name the same vertices, in the same
    *   order, as [[that]].
    */
  def sameAs(that: Index[?]): Boolean = this eq that

  /** @return The vertex numbered [[index]]. */
  def vertex(index: Int): V

  /** @return The number of [[vertex]], if it's one of these vertices. */
  def index(vertex: Any): Option[Int]

/** The companion object for [[Index]]. */
private[kneiphof] object Index:

  /** The vertices `0 until order`, which are their own numbers. */
  final class Numbers(override val order: Int) extends Index[Int]:

    require(
      order >= 0,
      s"A shape can't have $order vertices.",
    )

    override def vertex(index: Int): Int = index

    override def index(vertex: Any): Option[Int] = Index.number(vertex, order)

    override def sameAs(that: Index[?]): Boolean = that match
      case that: Numbers => order == that.order
      case _             => false

  /**
    * The numbers in [[range]], numbered in order, and found by arithmetic
    * rather than a table.
    */
  final class Interval(private val range: Range) extends Index[Int]:

    override def order: Int = range.size

    override def vertex(index: Int): Int = range(index)

    override def index(vertex: Any): Option[Int] = Some(vertex.##)
      .filter(number => vertex == number && range.contains(number))
      .map(number => (number - range.start) / range.step)

    override def sameAs(that: Index[?]): Boolean = that match
      case that: Interval => range == that.range
      case _              => false

  /** The given [[vertices]], numbered in the order given. */
  final class Vertices[+V]
    (private val vertices: IndexedSeq[V])
    extends Index[V]:

    require(
      vertices.distinct.size == vertices.size,
      s"The vertices of a shape must be distinct, but were $vertices.",
    )

    override def order: Int = vertices.size

    override def vertex(index: Int): V = vertices(index)

    override def index(vertex: Any): Option[Int] = positions.get(vertex)

    override def sameAs(that: Index[?]): Boolean = that match
      case that: Vertices[?] => vertices == that.vertices
      case _                 => false

    /** The number of each vertex. */
    private lazy val positions: Map[Any, Int] = vertices
      .view
      .zipWithIndex
      .map((vertex, index) => (vertex: Any) -> index)
      .toMap

  /**
    * @return
    *   The number in `0 until order` equal to [[value]], if there is one. Like
    *   a hash map, this compares by `==`, so `1L` and `1.0` find `1`. Numbers
    *   equal to an [[Int]] hash to it, so the hash is the only candidate.
    */
  private def number(value: Any, order: Int): Option[Int] = Some(value.##)
    .filter(index => 0 <= index && index < order && value == index)
