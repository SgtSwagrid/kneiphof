package com.alecdorrington
package kneiphof

/**
  * A multigraph built by chaining edges, as in `1 ~ 2 ~ 3`, which holds the
  * edges `1 ~ 2` and `2 ~ 3`.
  *
  * As a multigraph, a chain only records its edges and how many times each
  * occurs, so `1 ~ 2 ~ 3 == 3 ~ 2 ~ 1`. It also remembers where it was last
  * extended, so that it can be extended again. Every [[Edge]] is a chain of one
  * edge.
  *
  * @tparam E
  *   The labels of every edge but the last.
  * @tparam L
  *   The label of the last edge.
  */
trait Chain[+V, +E, +L] extends GraphBase[V, E | L, Count]:

  /** The vertex this chain continues from, at the end of its last edge. */
  def tip: V

  /**
    * @return
    *   This chain, extended by an unlabelled edge from [[tip]] to [[next]].
    */
  def to[W >: V](next: W): Chain[W, E | L, Unit] =
    Chain.Steps(steps, kneiphof.Edge(tip, next))

  /** Alias of [[to]]. */
  infix def ~ [W >: V](next: W): Chain[W, E | L, Unit] = to(next)

  /**
    * @return
    *   This chain, with its last edge labelled [[label]] instead. To label an
    *   earlier edge, use brackets, as in `(1 ~ 2 % "a") ~ 3`, since `%` binds
    *   more loosely than `~` (see [[NotGraph]]).
    */
  def labelled[F : NotGraph](label: F): Chain[V, E, F]

  /** Alias of [[labelled]]. */
  infix def % [F : NotGraph](label: F): Chain[V, E, F] = labelled(label)

  /** The edges of this chain, in the order they were written. */
  private[kneiphof] def steps: Seq[kneiphof.Edge[V, E | L]]

/** The companion object for [[Chain]]. */
object Chain:

  /** A chain of at least two edges. */
  private final class Steps[+V, +E, +L]
    (earlier: Seq[Edge[V, E]], last: Edge[V, L])
    extends Chain[V, E, L],
            Delegating[
              V,
              E | L,
              Count,
              Edge,
              [v, e] =>> GraphBase[v, e, Count],
            ]:

    override def tip: V = last.second

    override def updated[W >: V, F >: E | L]
      (
        edge: kneiphof.Edge[W, F],
        multiplicity: Count,
      )
      : GraphBase[W, F, Count] = delegate.updated(edge, multiplicity)

    override def labelled[F : NotGraph](label: F): Chain[V, E, F] =
      Steps(earlier, last.labelled(label))

    override private[kneiphof] def steps: Seq[kneiphof.Edge[V, E | L]] =
      earlier :+ last

    override protected lazy val delegate: GraphBase[V, E | L, Count] = GraphBase
      .kind[Count]
      .from(steps)
