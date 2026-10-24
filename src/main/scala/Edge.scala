package com.alecdorrington
package kneiphof

/**
  * An undirected edge between two vertices, whose order doesn't matter, so that
  * `Edge(1, 2) == Edge(2, 1)`.
  *
  * Usually written `first ~ second`, or `first ~ second % label`.
  */
final case class Edge[+V, +E]
  (first: V, second: V, override val label: E)
  extends Hyperedge[V, E], AnyEdge[V, E, Edge]:

  /** `true` if and only if this edge joins a vertex to itself. */
  def isLoop: Boolean = first == second

  override def ends: Seq[V] = Seq(first, second)

  override def rank: Int = 2

  override def vertices: ValueSet[V] = ValueSet(first, second)

  override def map[W, F](vertexMap: V => W, labelMap: E => F): Edge[W, F] =
    Edge(
      vertexMap(first),
      vertexMap(second),
      labelMap(label),
    )

  override def flatMapVertices[W]
    (choices: V => Iterable[W])
    : Iterable[Edge[W, E]] =
    for
      start <- choices(first)
      end   <- choices(second)
    yield Edge(start, end, label)

  override def toString: String = labelled(s"$first ~ $second", label)

/** The companion object for [[Edge]]. */
object Edge:

  /** @return An unlabelled edge between [[first]] and [[second]]. */
  def apply[V](first: V, second: V): Edge[V, Unit] = Edge(first, second, ())
