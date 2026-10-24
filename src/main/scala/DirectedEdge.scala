package com.alecdorrington
package kneiphof

/**
  * A directed edge from a [[source]] of type [[S]] to a [[target]] of type
  * [[T]].
  *
  * Usually written `source ~> target`, or `source ~> target % label`.
  */
final case class DirectedEdge[+S, +T, +E]
  (source: S, target: T, override val label: E)
  extends DirectedHyperedge[S, T, E], AnyEdge[S | T, E, Arc]:

  override def tail: ValueSet[S] = ValueSet(source)

  override def head: ValueSet[T] = ValueSet(target)

  override def reverse: DirectedEdge[T, S, E] =
    DirectedEdge(target, source, label)

  override def map[W, F]
    (vertexMap: (S | T) => W, labelMap: E => F)
    : DirectedEdge[W, W, F] = DirectedEdge(
    vertexMap(source),
    vertexMap(target),
    labelMap(label),
  )

  override def flatMapVertices[W]
    (choices: (S | T) => Iterable[W])
    : Iterable[DirectedEdge[W, W, E]] =
    for
      start <- choices(source)
      end   <- choices(target)
    yield DirectedEdge(start, end, label)

  override def toString: String = labelled(s"$source ~> $target", label)

/** The companion object for [[DirectedEdge]]. */
object DirectedEdge:

  /** @return An unlabelled edge from [[source]] to [[target]]. */
  def apply[S, T](source: S, target: T): DirectedEdge[S, T, Unit] =
    DirectedEdge(source, target, ())
