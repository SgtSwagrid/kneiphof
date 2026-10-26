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

    override def index(vertex: Any): Option[Int] = vertex match
      case vertex: Int if 0 <= vertex && vertex < order => Some(vertex)
      case _                                            => None

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
