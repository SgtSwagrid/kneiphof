package com.alecdorrington
package kneiphof

/**
  * A directed edge from a set of vertices of type [[S]] (its tail) to a set of
  * vertices of type [[T]] (its head).
  *
  * The tail and head may overlap, and either may be empty.
  */
trait DirectedHyperedge[+S, +T, +E] extends AnyEdge[S | T, E, Hyperarc]:

  /** The vertices this hyperedge leaves. */
  def tail: ValueSet[S]

  /** The vertices this hyperedge enters. */
  def head: ValueSet[T]

  /** @return This hyperedge, with its tail and head swapped. */
  def reverse: DirectedHyperedge[T, S, E] =
    DirectedHyperedge.from(head, tail, label)

  override def vertices: ValueSet[S | T] = tail | head

  override def occurrences(vertex: Any): Int =
    Seq(tail, head).count(_.contains(vertex))

  override def map[W, F]
    (vertexMap: (S | T) => W, labelMap: E => F)
    : DirectedHyperedge[W, W, F] = DirectedHyperedge.from(
    tail.map(vertexMap),
    head.map(vertexMap),
    labelMap(label),
  )

  override def flatMapVertices[W]
    (choices: (S | T) => Iterable[W])
    : Iterable[DirectedHyperedge[W, W, E]] =
    for
      sources <- cartesian(tail.toSeq.map(choices))
      targets <- cartesian(head.toSeq.map(choices))
    yield DirectedHyperedge.from(sources, targets, label)

  override final def equals(that: Any): Boolean = that match
    case that: DirectedHyperedge[?, ?, ?] => tail == that.tail &&
      head == that.head && label == that.label
    case _ => false

  override final def hashCode: Int = (tail, head, label).##

  override def toString: String = labelled(
    s"${ tail.mkString("{", ", ", "}") } ~> ${ head.mkString("{", ", ", "}") }",
    label,
  )

/** The companion object for [[DirectedHyperedge]]. */
object DirectedHyperedge:

  /** @return An unlabelled hyperedge from [[tail]] to [[head]]. */
  def apply[S, T]
    (tail: Iterable[S], head: Iterable[T])
    : DirectedHyperedge[S, T, Unit] = from(tail, head, ())

  /**
    * @return
    *   A hyperedge from [[tail]] to [[head]] carrying [[label]]. A hyperedge
    *   with exactly one source and one target is always a [[DirectedEdge]].
    */
  def from[S, T, E]
    (
      tail: Iterable[S],
      head: Iterable[T],
      label: E,
    )
    : DirectedHyperedge[S, T, E] =
    (ValueSet.from(tail), ValueSet.from(head)) match
      case (Single(source), Single(target)) =>
        DirectedEdge(source, target, label)
      case (sources, targets) => Sets(sources, targets, label)

  /** A hyperedge stored as its tail and head. */
  private final class Sets[+S, +T, +E]
    (
      override val tail: ValueSet[S],
      override val head: ValueSet[T],
      override val label: E,
    )
    extends DirectedHyperedge[S, T, E]

  /** Matches a set with exactly one element. */
  private object Single:

    def unapply[A](set: ValueSet[A]): Option[A] =
      if set.size == 1 then set.headOption else None
