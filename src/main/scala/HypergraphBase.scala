package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceHypergraph
import com.alecdorrington.kneiphof.views.{
  Joining, LazyHypergraph, Queries, Viewing,
}

/**
  * A hypergraph, whose edges may join any number of vertices, with
  * multiplicities in [[M]].
  *
  * Use the aliases [[Hypergraph]] (edge sets) or [[Multihypergraph]] (edge
  * counts) in most code.
  */
trait HypergraphBase[+V, +E, M]
  extends GraphOps[
    V,
    E,
    M,
    Hyperedge,
    [v, e] =>> HypergraphBase[v, e, M],
  ]:

  override type Vertex = VertexOps

  override type Edge = EdgeOps

  /**
    * @return
    *   This hypergraph with the multiplicity of [[edge]] set to
    *   [[multiplicity]], adding its vertices if necessary.
    */
  def updated[W >: V, F >: E](edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M]

  /**
    * @return
    *   The sum of this hypergraph and [[that]], which adds the multiplicities
    *   of their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `Hyperedge(1, 2, 3)`.
    */
  infix def + [W >: V, F >: E, N]
    (that: HypergraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : HypergraphBase[W, F, M] = combined(that, embed, algebra.additive)(_ + _)

  /**
    * @return
    *   The union of this hypergraph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for `+`.
    */
  infix def | [W >: V, F >: E, N]
    (that: HypergraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : HypergraphBase[W, F, M] = combined(that, embed, false)(_ | _)

  /**
    * @return
    *   The disjoint union of this hypergraph and [[that]], whose vertices are
    *   tagged by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[Either[V, W], F, M] = hypergraphs.disjointUnion(this, that)

  /** An alias for [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The hypergraph in which each vertex `v` is replaced by the hypergraph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  def flatMapVertices[W, F >: E]
    (transform: V => HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = hypergraphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The hypergraph obtained by replacing each vertex with the hypergraph it
    *   holds (see [[flatMapVertices]]).
    */
  def flatten[W, F >: E]
    (using nested: V <:< HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = flatMapVertices(nested)

  /**
    * @return
    *   The lexicographic sum (or X-join), in which each vertex `v` is replaced
    *   by a disjoint copy of the hypergraph `transform(v)`.
    */
  def lexicographicSum[W, F >: E]
    (transform: V => HypergraphBase[W, F, M])
    : HypergraphBase[(V, W), F, M] =
    hypergraphs.lexicographicSum(this, transform)

  /**
    * @return
    *   The lexicographic product `G[H]`, in which each vertex is replaced by a
    *   copy of [[that]].
    */
  def lexicographicProduct[W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[(V, W), F, M] = lexicographicSum(_ => that)

  /**
    * @return
    *   The join of this hypergraph and [[that]], with an extra edge labelled
    *   [[label]] between every vertex of this hypergraph and every vertex of
    *   [[that]]. Vertices in both also gain a loop.
    */
  def join[W >: V, F >: E]
    (that: HypergraphBase[W, F, M], label: F)
    : HypergraphBase[W, F, M] = HypergraphBase.Join(this, that) % label

  /**
    * @return
    *   The join `that :~: this`, with an edge labelled `()` between every
    *   vertex of [[that]] and every vertex of this hypergraph (see [[join]]).
    *   The new edges can be relabelled, as in `(that :~: this) % label`.
    */
  infix def :~: [W >: V, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase.Join[W, F, Unit, M] = HypergraphBase.Join(that, this)

  /**
    * @return
    *   The cone `this :~ vertex`, with an edge labelled `()` from every vertex
    *   of this hypergraph to [[vertex]] (see [[join]]). The new edges can be
    *   relabelled, as in `(this :~ vertex) % label`.
    */
  infix def :~ [W >: V](vertex: W): HypergraphBase.Join[W, E, Unit, M] =
    HypergraphBase.Join(this, hypergraphs.vertex(vertex))

  /**
    * @return
    *   The cone `vertex ~: this`, which is the same as `this :~ vertex`. The
    *   new edges can be relabelled, as in `vertex ~: this % label`.
    */
  infix def ~: [W >: V](vertex: W): HypergraphBase.Join[W, E, Unit, M] = this :~
    vertex

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : HypergraphBase[W, F, M] = hypergraphs.imaged(this, viewing, edgeValues)(
    vertexMap,
    edge => List(edge.map(vertexMap, labelMap)),
    kept = algebra.additive,
  )

  override def view: HypergraphBase[V, E, M] = hypergraphs.view(viewing, this)

  override def vertex(value: Any): Option[Vertex] = vertexValues
    .get(value)
    .map(found =>
      new VertexOps:
        override val value: V = found,
    )

  override def edge(value: Any): Option[Edge] = found(value).map(found =>
    new EdgeOps:
      override val value: Hyperedge[V, E] = found,
  )

  override protected def kindName: String = "Hypergraph"

  override protected def refined
    (
      keep: V => Boolean,
      keepEdge: Option[Hyperedge[V, E] => Boolean],
      reweigh: (Any, M) => M,
      changed: => Iterable[Any],
      support: Option[() => Iterable[Any]],
      operands: AnyGraph[?, ?, ?]*,
    )
    : HypergraphBase[V, E, M] = hypergraphs.refined(
    this,
    keep,
    keepEdge,
    reweigh,
    changed,
    support,
    operands,
  )

  /**
    * @return
    *   The union or sum of this hypergraph and [[that]], whose multiplicities
    *   are combined by [[combine]].
    */
  private def combined[W >: V, F >: E, N]
    (
      that: HypergraphBase[W, F, N],
      embed: Embedding[N, M],
      additive: Boolean,
    )
    (combine: (M, M) => M)
    : HypergraphBase[W, F, M] = hypergraphs.combined[W, F](
    this,
    that.viewing(embed),
    that,
    that.vertexValues,
    that.edgeValues.map((edge, n) => edge -> embed(n)),
    additive,
  )(combine)

  /** The kind of this hypergraph, in its own representation. */
  private def hypergraphs: HypergraphBase.Kind[M] = HypergraphBase.Kind(empty)

/** The companion object for [[HypergraphBase]]. */
object HypergraphBase:

  /**
    * The join of two hypergraphs, which remembers its new edges so that they
    * can be relabelled with `%`.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M] extends HypergraphBase[V, E | L, M]:

    /** @return This join, with every new edge labelled [[label]] instead. */
    infix def % [F : NotGraph](label: F): Join[V, E, F, M]

  /** The companion object for [[Join]]. */
  object Join:

    /**
      * @return
      *   The join of [[left]] and [[right]], whose new edges are labelled `()`.
      */
    def apply[V, E, M : Multiplicity]
      (
        left: HypergraphBase[V, E, M],
        right: HypergraphBase[V, E, M],
      )
      : Join[V, E, Unit, M] = Joined(left, right, ())

    /**
      * A join, held as the hypergraphs it joins. It's always lazy, since its
      * size is the product of theirs.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: HypergraphBase[V, E, M],
        right: HypergraphBase[V, E, M],
        label: L,
      )
      extends LazyHypergraph[V, E | L, M](
        left.empty,
        Kind(left.empty).joining[V, E, L](left, right)(
          kneiphof.Edge(_, _, label),
          Joining.undirectedLinks(left, right, label, _),
        ),
        () =>
          Kind(left.empty).joined[V, E | L](left, right)(
            kneiphof.Edge(_, _, label),
          ),
      ),
              Join[V, E, L, M]:

      override infix def % [F : NotGraph](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

  /** The kind of hypergraphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: HypergraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Hyperedge,
      [v, e] =>> HypergraphBase[v, e, M],
      [v, e] =>> HypergraphBase[v, e, Count],
    ]:

    override private[kneiphof] def view[V, E]
      (
        queries: Queries[V, E, M, Hyperedge[V, E]],
        build: => HypergraphBase[V, E, M],
      )
      : HypergraphBase[V, E, M] = LazyHypergraph(empty, queries, () => build)

    override def updated[V, E]
      (
        graph: HypergraphBase[V, E, M],
        edge: Hyperedge[V, E],
        multiplicity: M,
      )
      : HypergraphBase[V, E, M] = graph.updated(edge, multiplicity)

  /** @return The kind of hypergraphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceHypergraph.empty)

/** Hypergraphs whose edges form a set, in the default representation. */
object Hypergraph
  extends HypergraphBase.Kind[Boolean](IncidenceHypergraph.empty)

/** Hypergraphs with edge counts, in the default representation. */
object Multihypergraph
  extends HypergraphBase.Kind[Count](IncidenceHypergraph.empty)
