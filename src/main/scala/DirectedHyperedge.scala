package com.alecdorrington
package kneiphof

import scala.util.hashing.MurmurHash3

/**
  * A directed edge from a multiset of vertices of type [[S]] (its tail) to a
  * multiset of vertices of type [[T]] (its head).
  *
  * As for the ends of a [[Hyperedge]], a vertex may occur more than once in
  * either, so that merging vertices keeps every occurrence, and so every
  * degree. The tail and head may overlap, and either may be empty, but not
  * both. Every directed hyperedge is also the directed multihypergraph
  * containing just itself, once.
  */
trait DirectedHyperedge[+S, +T, +E]
  extends AnyEdge[S | T, E, Hyperarc],
          DirectedHypergraphBase[S | T, E, Count],
          SingleEdge[
            S | T,
            E,
            Hyperarc,
            [v, e] =>> DirectedHypergraphBase[v, e, Count],
          ]:

  /**
    * The vertices this hyperedge leaves, each repeated as often as it occurs.
    * Their order doesn't matter.
    */
  def tail: Seq[S]

  /**
    * The vertices this hyperedge enters, each repeated as often as it occurs.
    * Their order doesn't matter.
    */
  def head: Seq[T]

  /** @return This hyperedge, with its tail and head swapped. */
  def reverse: DirectedHyperedge[T, S, E] =
    DirectedHyperedge.from(head, tail, label)

  override def transpose: DirectedHyperedge[T, S, E] = reverse

  override def vertexValues: ValueSet[S | T] = ValueSet.from(tail ++ head)

  override def occurrences(vertex: Any): Int = tail.count(_ == vertex) +
    head.count(_ == vertex)

  override def map[W, F]
    (vertexMap: (S | T) => W, labelMap: E => F)
    : DirectedHyperedge[W, W, F] = DirectedHyperedge.from(
    tail.map(vertexMap),
    head.map(vertexMap),
    labelMap(label),
  )

  override def mapVertices[W]
    (transform: (S | T) => W)
    : DirectedHyperedge[W, W, E] = map(transform, identity)

  override def substitute[W]
    (choices: (S | T) => Iterable[W])
    : Iterable[DirectedHyperedge[W, W, E]] =
    for
      sources <- cartesian(tail.map(choices))
      targets <- cartesian(head.map(choices))
    yield DirectedHyperedge.from(sources, targets, label)

  override def shapeHash: Int =
    (MurmurHash3.unorderedHash(tail), MurmurHash3.unorderedHash(head), label).##

  override final def equals(that: Any): Boolean = that match
    case that: DirectedHyperedge[?, ?, ?] => label == that.label &&
      DirectedHyperedge.counts(tail) == DirectedHyperedge.counts(that.tail) &&
      DirectedHyperedge.counts(head) == DirectedHyperedge.counts(that.head)
    case _: AnyEdge[?, ?, ?]     => false
    case that: AnyGraph[?, ?, ?] => sameGraph(that)
    case _                       => false

  override def toString: String = describe(
    s"${ tail.mkString("{", ", ", "}") } ~> ${ head.mkString("{", ", ", "}") }",
    label,
  )

/** The companion object for [[DirectedHyperedge]]. */
object DirectedHyperedge:

  /**
    * @return
    *   An unlabelled hyperedge from [[tail]] to [[head]], counting repeats.
    */
  def apply[S, T]
    (tail: Iterable[S], head: Iterable[T])
    : DirectedHyperedge[S, T, Unit] = from(tail, head, ())

  /**
    * @return
    *   A hyperedge from [[tail]] to [[head]] carrying [[label]], counting
    *   repeats. A hyperedge with exactly one source and one target is always a
    *   [[DirectedEdge]].
    * @throws IllegalArgumentException
    *   If both [[tail]] and [[head]] are empty.
    */
  def from[S, T, E]
    (
      tail: Iterable[S],
      head: Iterable[T],
      label: E,
    )
    : DirectedHyperedge[S, T, E] =
    require(
      tail.nonEmpty || head.nonEmpty,
      "A directed hyperedge must have at least one end.",
    )
    (tail.toSeq, head.toSeq) match
      case (Seq(source), Seq(target)) => DirectedEdge(source, target, label)
      case (sources, targets)         => Multisets(sources, targets, label)

  /**
    * @return
    *   The number of times each vertex occurs in [[ends]], which identifies
    *   them as a multiset.
    */
  private def counts(ends: Seq[Any]): Map[Any, Int] =
    ends.groupMapReduce(identity)(_ => 1)(_ + _)

  /** A hyperedge stored as its tail and head. */
  private final class Multisets[+S, +T, +E]
    (
      override val tail: Seq[S],
      override val head: Seq[T],
      override val label: E,
    )
    extends DirectedHyperedge[S, T, E]
