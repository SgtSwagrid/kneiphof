package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceStructure

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
  def to[W >: V](next: W): DirectedChain[W, E | L, Unit] =
    DirectedChain.Steps(steps, DirectedEdge(tip, next))

  /** Alias of [[to]]. */
  infix def ~> [W >: V](next: W): DirectedChain[W, E | L, Unit] = to(next)

  /** @return This chain, with its last edge labelled [[label]] instead. */
  def labelled[F : NotGraph](label: F): DirectedChain[V, E, F]

  /** Alias of [[labelled]]. */
  infix def % [F : NotGraph](label: F): DirectedChain[V, E, F] = labelled(label)

  /** The edges of this chain, in the order they were written. */
  private[kneiphof] def steps: Seq[Arc[V, E | L]]

/** The companion object for [[DirectedChain]]. */
object DirectedChain:

  /** A chain of at least two edges. */
  private final class Steps[+V, +E, +L]
    (earlier: Seq[Arc[V, E]], last: Arc[V, L])
    extends DirectedChain[V, E, L]:

    override def tip: V = last.target

    override def labelled[F : NotGraph](label: F): DirectedChain[V, E, F] =
      Steps(earlier, last.labelled(label))

    override private[kneiphof] def steps: Seq[Arc[V, E | L]] = earlier :+ last

    override private[kneiphof] lazy val structure
      : IncidenceStructure[V, Count, Arc[V, E | L]] = DirectedGraphBase
      .kind[Count]
      .from(steps)
      .structure
