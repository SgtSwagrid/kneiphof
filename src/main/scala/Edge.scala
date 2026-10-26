package com.alecdorrington
package kneiphof

/**
  * An undirected edge between two vertices, whose order doesn't matter, so that
  * `Edge(1, 2) == Edge(2, 1)`.
  *
  * Usually written `first ~ second`, or `first ~ second % label`. An edge is
  * the multigraph containing just itself, once, and the [[Chain]] of one edge,
  * so `first ~ second ~ next` adds the edge from [[second]] to `next`.
  */
final case class Edge[+V, +E]
  (first: V, second: V, override val label: E)
  extends Hyperedge[V, E],
          AnyEdge[V, E, Edge],
          GraphBase[V, E, Count],
          SingleEdge[V, E, Edge, [v, e] =>> GraphBase[v, e, Count]],
          Chain[V, Nothing, E]:

  /** `true` if and only if this edge joins a vertex to itself. */
  def isLoop: Boolean = first == second

  override def ends: Seq[V] = Seq(first, second)

  override def rank: Int = 2

  override def vertexValues: ValueSet[V] = ValueSet(first, second)

  override def tip: V = second

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : kneiphof.Edge[W, F] = Edge(
    vertexMap(first),
    vertexMap(second),
    labelMap(label),
  )

  override def mapVertices[W](transform: V => W): kneiphof.Edge[W, E] =
    map(transform, identity)

  override def substitute[W]
    (choices: V => Iterable[W])
    : Iterable[kneiphof.Edge[W, E]] =
    for
      start <- choices(first)
      end   <- choices(second)
    yield Edge(start, end, label)

  override def updated[W >: V, F >: E]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: Count,
    )
    : GraphBase[W, F, Count] = delegate.updated(edge, multiplicity)

  override def updated[W >: V, F >: E]
    (
      edge: Hyperedge[W, F],
      multiplicity: Count,
    )
    : HypergraphBase[W, F, Count] = delegate.updated(edge, multiplicity)

  override infix def % [F : NotGraph](label: F): kneiphof.Edge[V, F] =
    Edge(first, second, label)

  override def toString: String = labelled(s"$first ~ $second", label)

  override private[kneiphof] def steps: Seq[kneiphof.Edge[V, E]] = Seq(this)

  override protected def delegate: GraphBase[V, E, Count] = GraphBase
    .kind[Count]
    .from(Seq(this))

/** The companion object for [[Edge]]. */
object Edge:

  /** @return An unlabelled edge between [[first]] and [[second]]. */
  def apply[V](first: V, second: V): Edge[V, Unit] = Edge(first, second, ())
