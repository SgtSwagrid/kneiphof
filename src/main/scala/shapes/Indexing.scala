package com.alecdorrington
package kneiphof
package shapes

/** The vertices of a shape, numbered from `0`. */
private[kneiphof] trait Indexing[+V]:

  /** The number of vertices. */
  def order: Int

  /** @return The vertex numbered [[index]]. */
  def vertex(index: Int): V

  /** @return The number of [[vertex]], if it's one of these vertices. */
  def index(vertex: Any): Option[Int]

/** The companion object for [[Indexing]]. */
private[kneiphof] object Indexing:

  /** The vertices `0 until order`, which are their own numbers. */
  final class Counting(override val order: Int) extends Indexing[Int]:

    require(
      order >= 0,
      s"A shape can't have $order vertices.",
    )

    override def vertex(index: Int): Int = index

    override def index(vertex: Any): Option[Int] =
      Indexing.number(vertex, order)

  /** The given [[vertices]], numbered in the order given. */
  final class Listing[+V](vertices: IndexedSeq[V]) extends Indexing[V]:

    require(
      vertices.distinct.size == vertices.size,
      s"The vertices of a shape must be distinct, but were $vertices.",
    )

    override def order: Int = vertices.size

    override def vertex(index: Int): V = vertices(index)

    override def index(vertex: Any): Option[Int] = positions.get(vertex)

    /** The number of each vertex. */
    private lazy val positions: Map[Any, Int] = vertices
      .view
      .zipWithIndex
      .map((vertex, index) => (vertex: Any) -> index)
      .toMap

  /**
    * The pairs `(row, column)` for each row in `0 until rows` and column in
    * `0 until columns`, numbered `row * columns + column`.
    */
  final class Pairing(rows: Int, columns: Int) extends Indexing[(Int, Int)]:

    require(
      rows >= 0 && columns >= 0,
      s"A shape can't have $rows rows and $columns columns.",
    )

    override def order: Int = Math.multiplyExact(rows, columns)

    override def vertex(index: Int): (Int, Int) =
      (index / columns, index % columns)

    override def index(vertex: Any): Option[Int] = vertex match
      case (row, column) =>
        for
          row    <- Indexing.number(row, rows)
          column <- Indexing.number(column, columns)
        yield row * columns + column
      case _ => None

  /**
    * @return
    *   The number in `0 until order` equal to [[value]], if there is one. Like
    *   a hash map, this compares by `==`, so `1L` and `1.0` find `1`. Numbers
    *   equal to an [[Int]] hash to it, so the hash is the only candidate.
    */
  private def number(value: Any, order: Int): Option[Int] = Some(value.##)
    .filter(index => 0 <= index && index < order && value == index)
