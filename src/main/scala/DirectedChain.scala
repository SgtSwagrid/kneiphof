package com.alecdorrington
package kneiphof

/**
  * A directed multigraph built by chaining edges, as in `1 ~> 2 ~> 3`, which
  * holds the edges `1 ~> 2` and `2 ~> 3`.
  *
  * As a multigraph, a chain only records its edges and how many times each
  * occurs. It also remembers where it was last extended, so that it can be
  * extended again. Every [[DirectedEdge]] is a chain of one edge.
  *
  * @tparam E
  *   The labels of every edge but the last.
  * @tparam L
  *   The label of the last edge.
  */
trait DirectedChain[+V, +E, +L] extends DirectedGraphBase[V, E | L, Count]:

  /** The vertex this chain continues from, at the target of its last edge. */
  def tip: V

  /**
    * @return
    *   This chain, extended by an unlabelled edge from [[tip]] to [[next]].
    */
  infix def ~> [W >: V](next: W): DirectedChain[W, E | L, Unit] = DirectedChain
    .Steps(steps, DirectedEdge(tip, next))

  /** @return This chain, with its last edge labelled [[label]] instead. */
  infix def % [F](label: F): DirectedChain[V, E, F]

  /** The edges of this chain, in the order they were written. */
  private[kneiphof] def steps: Seq[Arc[V, E | L]]

/** The companion object for [[DirectedChain]]. */
object DirectedChain:

  /** A chain of at least two edges. */
  private final class Steps[+V, +E, +L]
    (earlier: Seq[Arc[V, E]], last: Arc[V, L])
    extends DirectedChain[V, E, L],
            Delegating[
              V,
              E | L,
              Count,
              Arc,
              [v, e] =>> DirectedGraphBase[v, e, Count],
            ]:

    override def tip: V = last.target

    override def updated[W >: V, F >: E | L]
      (edge: Arc[W, F], multiplicity: Count)
      : DirectedGraphBase[W, F, Count] = delegate.updated(edge, multiplicity)

    override def transpose: DirectedGraphBase[V, E | L, Count] =
      delegate.transpose

    override infix def % [F](label: F): DirectedChain[V, E, F] =
      Steps(earlier, last % label)

    override private[kneiphof] def steps: Seq[Arc[V, E | L]] = earlier :+ last

    override protected lazy val delegate: DirectedGraphBase[V, E | L, Count] =
      DirectedGraphBase.kind[Count].from(steps)
