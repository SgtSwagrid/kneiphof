package com.alecdorrington
package kneiphof

import scala.util.hashing.MurmurHash3

/**
  * An undirected edge joining any number of vertices, each of which may occur
  * more than once.
  *
  * Counting repeated vertices makes a graph exactly a hypergraph in which every
  * edge has two ends: a loop is the hyperedge in which one vertex occurs twice.
  * Every hyperedge has at least one end, and is also the multihypergraph
  * containing just itself, once.
  */
trait Hyperedge[+V, +E]
  extends AnyEdge[V, E, Hyperedge],
          HypergraphBase[V, E, Count],
          SingleEdge[
            V,
            E,
            Hyperedge,
            [v, e] =>> HypergraphBase[v, e, Count],
          ]:

  /**
    * The ends of this hyperedge, each vertex repeated as often as it occurs.
    * Their order doesn't matter.
    */
  def ends: Seq[V]

  /** The number of ends of this hyperedge, counting repeats. */
  def rank: Int = ends.size

  override def vertexValues: ValueSet[V] = ValueSet.from(ends)

  override def occurrences[A : NotHandle](vertex: A): Int =
    ends.count(_ == vertex)

  override def map[W : NotHandle, F]
    (vertexMap: V => W, labelMap: E => F)
    : Hyperedge[W, F] = Hyperedge.from(ends.map(vertexMap), labelMap(label))

  override def mapVertices[W : NotHandle](transform: V => W): Hyperedge[W, E] =
    map(transform, identity)

  override def substitute[W : NotHandle]
    (choices: V => Iterable[W])
    : Iterable[Hyperedge[W, E]] =
    cartesian(ends.map(choices)).map(Hyperedge.from(_, label))

  override def shapeHash: Int = MurmurHash3.unorderedHash(ends, label.##)

  override final def equals(that: Any): Boolean = that match
    case that: Hyperedge[?, ?] => label == that.label && rank == that.rank &&
      multiset == that.multiset
    case _: AnyEdge[?, ?, ?]     => false
    case that: AnyGraph[?, ?, ?] => sameGraph(that)
    case _                       => false

  override def toString: String = describe(
    ends.mkString("Hyperedge(", ", ", ")"),
    label,
  )

  /** The number of times each vertex occurs, which identifies the ends. */
  private def multiset: Map[Any, Int] =
    ends.groupMapReduce[Any, Int](identity)(_ => 1)(_ + _)

/** The companion object for [[Hyperedge]]. */
object Hyperedge:

  /**
    * @return
    *   The unlabelled hyperedge with the given [[ends]], counting repeats.
    */
  def apply[V](ends: V*): Hyperedge[V, Unit] = from(ends, ())

  /**
    * @return
    *   The hyperedge with the given [[ends]] and [[label]], counting repeats. A
    *   hyperedge with exactly two ends is always an [[Edge]].
    * @throws IllegalArgumentException
    *   If there are no [[ends]].
    */
  def from[V, E](ends: Iterable[V], label: E): Hyperedge[V, E] =
    require(
      ends.nonEmpty,
      "A hyperedge must have at least one end.",
    )
    ends.toSeq match
      case Seq(first, second) => Edge(first, second, label)
      case ends               => Multiset(ends, label)

  /** A hyperedge stored as the list of its ends. */
  private final class Multiset[+V, +E]
    (
      override val ends: Seq[V],
      override val label: E,
    )
    extends Hyperedge[V, E]
