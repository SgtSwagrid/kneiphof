package com.alecdorrington
package kneiphof

/**
  * A directed edge from a [[source]] of type [[S]] to a [[target]] of type
  * [[T]].
  *
  * Usually written `source ~> target`, or `source ~> target % label`. An edge
  * is the directed multigraph containing just itself, once, and the
  * [[DirectedChain]] of one edge, so `source ~> target ~> next` adds the edge
  * from [[target]] to `next`.
  */
final case class DirectedEdge[+S, +T, +E]
  (source: S, target: T, override val label: E)
  extends DirectedHyperedge[S, T, E],
          AnyEdge[S | T, E, Arc],
          DirectedGraphBase[S | T, E, Count],
          SingleEdge[
            S | T,
            E,
            Arc,
            [v, e] =>> DirectedGraphBase[v, e, Count],
          ],
          DirectedChain[S | T, Nothing, E]:

  override def tail: ValueSet[S] = ValueSet(source)

  override def head: ValueSet[T] = ValueSet(target)

  override def vertexValues: ValueSet[S | T] = ValueSet(source, target)

  override def tip: T = target

  override def reverse: DirectedEdge[T, S, E] =
    DirectedEdge(target, source, label)

  override def transpose: DirectedEdge[T, S, E] = reverse

  override def map[W, F]
    (vertexMap: (S | T) => W, labelMap: E => F)
    : DirectedEdge[W, W, F] = DirectedEdge(
    vertexMap(source),
    vertexMap(target),
    labelMap(label),
  )

  override def mapVertices[W](transform: (S | T) => W): DirectedEdge[W, W, E] =
    map(transform, identity)

  override def substitute[W]
    (choices: (S | T) => Iterable[W])
    : Iterable[DirectedEdge[W, W, E]] =
    for
      start <- choices(source)
      end   <- choices(target)
    yield DirectedEdge(start, end, label)

  override def updated[W >: S | T, F >: E]
    (edge: Arc[W, F], multiplicity: Count)
    : DirectedGraphBase[W, F, Count] = delegate.updated(edge, multiplicity)

  override def updated[W >: S | T, F >: E]
    (edge: Hyperarc[W, F], multiplicity: Count)
    : DirectedHypergraphBase[W, F, Count] = delegate.updated(edge, multiplicity)

  override infix def % [F : NotGraph](label: F): DirectedEdge[S, T, F] =
    DirectedEdge(source, target, label)

  override def toString: String = labelled(s"$source ~> $target", label)

  override private[kneiphof] def steps: Seq[DirectedEdge[S, T, E]] = Seq(this)

  override protected def delegate: DirectedGraphBase[S | T, E, Count] =
    DirectedGraphBase.kind[Count].from(Seq(this))

/** The companion object for [[DirectedEdge]]. */
object DirectedEdge:

  /** @return An unlabelled edge from [[source]] to [[target]]. */
  def apply[S, T](source: S, target: T): DirectedEdge[S, T, Unit] =
    DirectedEdge(source, target, ())
