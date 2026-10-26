package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceDirectedHypergraph
import com.alecdorrington.kneiphof.views.{
  Joining, LazyDirectedHypergraph, Queries, Viewing,
}

/**
  * A directed hypergraph, whose edges each lead from a set of vertices to a set
  * of vertices, with multiplicities in [[M]].
  *
  * Use the aliases [[DirectedHypergraph]] (edge sets) or
  * [[DirectedMultihypergraph]] (edge counts) in most code.
  */
trait DirectedHypergraphBase[+V, +E, M]
  extends GraphOps[
    V,
    E,
    M,
    Hyperarc,
    [v, e] =>> DirectedHypergraphBase[v, e, M],
  ]:

  override type Vertex = DirectedVertexOps

  override type Edge = DirectedEdgeOps

  /**
    * @return
    *   This hypergraph with the multiplicity of [[edge]] set to
    *   [[multiplicity]], adding its vertices if necessary.
    */
  def updated[W >: V, F >: E](edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M]

  /** The hypergraph with every edge reversed. */
  def transpose: DirectedHypergraphBase[V, E, M] = hypergraphs.imaged(
    this,
    hypergraphs.viewing(this),
    edgeValues,
  )(
    identity,
    edge => List(edge.reverse),
    exact = true,
  )

  /**
    * @return
    *   The sum of this hypergraph and [[that]], which adds the multiplicities
    *   of their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `DirectedHyperedge(Seq(1, 2), Seq(3))`.
    */
  infix def + [W >: V, F >: E, N]
    (that: DirectedHypergraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedHypergraphBase[W, F, M] = combined(that, embed)(_ + _)

  /**
    * @return
    *   The union of this hypergraph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for `+`.
    */
  infix def | [W >: V, F >: E, N]
    (that: DirectedHypergraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedHypergraphBase[W, F, M] = combined(that, embed)(_ | _)

  /**
    * @return
    *   The disjoint union of this hypergraph and [[that]], whose vertices are
    *   tagged by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[Either[V, W], F, M] =
    hypergraphs.disjointUnion(this, that)

  /** An alias for [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The hypergraph in which each vertex `v` is replaced by the hypergraph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  def flatMapVertices[W, F >: E]
    (transform: V => DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[W, F, M] =
    hypergraphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The hypergraph obtained by replacing each vertex with the hypergraph it
    *   holds (see [[flatMapVertices]]).
    */
  def flatten[W, F >: E]
    (using nested: V <:< DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[W, F, M] = flatMapVertices(nested)

  /**
    * @return
    *   The lexicographic sum (or X-join), in which each vertex `v` is replaced
    *   by a disjoint copy of the hypergraph `transform(v)`.
    */
  def lexicographicSum[W, F >: E]
    (transform: V => DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[(V, W), F, M] =
    hypergraphs.lexicographicSum(this, transform)

  /**
    * @return
    *   The lexicographic product `G[H]`, in which each vertex is replaced by a
    *   copy of [[that]].
    */
  def lexicographicProduct[W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[(V, W), F, M] = lexicographicSum(_ => that)

  /**
    * @return
    *   The join of this hypergraph and [[that]], with an extra edge labelled
    *   [[label]] from every vertex of this hypergraph to every vertex of
    *   [[that]]. Vertices in both also gain a loop.
    */
  def join[W >: V, F >: E]
    (
      that: DirectedHypergraphBase[W, F, M],
      label: F,
    )
    : DirectedHypergraphBase[W, F, M] =
    DirectedHypergraphBase.Join(this, that) % label

  /**
    * @return
    *   The join `that :~>: this`, with an edge labelled `()` from every vertex
    *   of [[that]] to every vertex of this hypergraph (see [[join]]). The new
    *   edges can be relabelled, as in `(that :~>: this) % label`.
    */
  infix def :~>: [W >: V, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase.Join[W, F, Unit, M] =
    DirectedHypergraphBase.Join(that, this)

  /**
    * @return
    *   The cone `this :~> vertex`, with an edge labelled `()` from every vertex
    *   of this hypergraph to [[vertex]] (see [[join]]). The new edges can be
    *   relabelled, as in `(this :~> vertex) % label`.
    */
  infix def :~> [W >: V]
    (vertex: W)
    : DirectedHypergraphBase.Join[W, E, Unit, M] =
    DirectedHypergraphBase.Join(this, hypergraphs.vertex(vertex))

  /**
    * @return
    *   The cone `vertex ~>: this`, with an edge labelled `()` from [[vertex]]
    *   to every vertex of this hypergraph (see [[join]]). The new edges can be
    *   relabelled, as in `vertex ~>: this % label`.
    */
  infix def ~>: [W >: V]
    (vertex: W)
    : DirectedHypergraphBase.Join[W, E, Unit, M] =
    DirectedHypergraphBase.Join(hypergraphs.vertex(vertex), this)

  /**
    * The undirected hypergraph obtained by forgetting the direction of every
    * edge, in the default representation. A vertex in both the tail and head of
    * an edge occurs twice in its image.
    */
  def underlying: HypergraphBase[V, E, M] = HypergraphBase
    .kind[M]
    .imaged(
      this,
      hypergraphs.viewing(this),
      edgeValues,
    )(
      identity,
      edge => List(Hyperedge.from(edge.tail.toSeq ++ edge.head, edge.label)),
      exact = false,
    )

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : DirectedHypergraphBase[W, F, M] = hypergraphs.imaged(
    this,
    hypergraphs.viewing(this),
    edgeValues,
  )(
    vertexMap,
    edge => List(edge.map(vertexMap, labelMap)),
    exact = false,
  )

  override def view: DirectedHypergraphBase[V, E, M] =
    hypergraphs.view(hypergraphs.viewing(this), this)

  override def vertex(value: Any): Option[Vertex] = vertexValues
    .get(value)
    .map(found =>
      new DirectedVertexOps:
        override val value: V = found,
    )

  override def edge(value: Any): Option[Edge] = edgeValues
    .getKey(value)
    .map: found =>
      new DirectedEdgeOps:
        override val value: Hyperarc[V, E] = found

  override protected def kindName: String = "DirectedHypergraph"

  override protected def refined
    (
      keep: V => Boolean,
      reweigh: (Any, M) => M,
      changed: => Iterable[Any],
      operands: AnyGraph[?, ?, ?]*,
    )
    : DirectedHypergraphBase[V, E, M] =
    hypergraphs.refined(this, keep, reweigh, changed, operands)

  /**
    * @return
    *   The union or sum of this hypergraph and [[that]], whose multiplicities
    *   are combined by [[combine]].
    */
  private def combined[W >: V, F >: E, N]
    (
      that: DirectedHypergraphBase[W, F, N],
      embed: Embedding[N, M],
    )
    (combine: (M, M) => M)
    : DirectedHypergraphBase[W, F, M] = hypergraphs.combined[W, F](
    this,
    DirectedHypergraphBase.viewing(that, embed),
    that,
    that.vertexValues,
    that.edgeValues.map((edge, n) => edge -> embed(n)),
  )(combine)

  /** The kind of this hypergraph, in its own representation. */
  private def hypergraphs: DirectedHypergraphBase.Kind[M] =
    DirectedHypergraphBase.Kind(empty)

  /** The operations on a vertex of this directed hypergraph. */
  trait DirectedVertexOps extends VertexOps:

    /** The edges leaving this vertex. */
    def outEdges: Set[Edge] = outgoing.flatMap(edge).toSet

    /** The edges entering this vertex. */
    def inEdges: Set[Edge] = incoming.flatMap(edge).toSet

    /** The vertices which an edge leads to from this vertex. */
    def successors: Set[Vertex] = outgoing.flatMap(_.head).flatMap(vertex).toSet

    /** The vertices from which an edge leads to this vertex. */
    def predecessors: Set[Vertex] = incoming
      .flatMap(_.tail)
      .flatMap(vertex)
      .toSet

    /** The number of edges leaving this vertex, counting every copy. */
    def outDegree: Int = outgoing.toSeq.map(multiplicity(_).count).sum

    /** The number of edges entering this vertex, counting every copy. */
    def inDegree: Int = incoming.toSeq.map(multiplicity(_).count).sum

    /** The values of the edges leaving this vertex. */
    private def outgoing: ValueSet[Hyperarc[V, E]] = incidentEdgeValues(value)
      .filter(_.tail.contains(value))

    /** The values of the edges entering this vertex. */
    private def incoming: ValueSet[Hyperarc[V, E]] = incidentEdgeValues(value)
      .filter(_.head.contains(value))

  /** The operations on an edge of this directed hypergraph. */
  trait DirectedEdgeOps extends EdgeOps:

    /** The vertices this edge leaves. */
    def tail: Set[Vertex] = value.tail.flatMap(vertex).toSet

    /** The vertices this edge enters. */
    def head: Set[Vertex] = value.head.flatMap(vertex).toSet

/** The companion object for [[DirectedHypergraphBase]]. */
object DirectedHypergraphBase:

  /**
    * The join of two directed hypergraphs, which remembers its new edges so
    * that they can be relabelled with `%`.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M] extends DirectedHypergraphBase[V, E | L, M]:

    /** @return This join, with every new edge labelled [[label]] instead. */
    infix def % [F](label: F): Join[V, E, F, M]

  /** The companion object for [[Join]]. */
  object Join:

    /**
      * @return
      *   The join from [[left]] to [[right]], whose new edges are labelled
      *   `()`.
      */
    def apply[V, E, M : Multiplicity]
      (
        left: DirectedHypergraphBase[V, E, M],
        right: DirectedHypergraphBase[V, E, M],
      )
      : Join[V, E, Unit, M] = Joined(left, right, ())

    /**
      * A join, held as the hypergraphs it joins. It's always lazy, since its
      * size is the product of theirs.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: DirectedHypergraphBase[V, E, M],
        right: DirectedHypergraphBase[V, E, M],
        label: L,
      )
      extends LazyDirectedHypergraph[V, E | L, M](
        left.empty,
        Joining[
          V,
          E,
          L,
          M,
          Hyperarc,
          [v, e] =>> DirectedHypergraphBase[v, e, M],
        ](
          left,
          right,
          DirectedEdge(_, _, label),
          Joining.directedLinks(left, right, label, _),
        ),
        () =>
          Kind(left.empty).joined[V, E | L](left, right)(
            DirectedEdge(_, _, label),
          ),
      ),
              Join[V, E, L, M]:

      override infix def % [F](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

  /** The kind of directed hypergraphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: DirectedHypergraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Hyperarc,
      [v, e] =>> DirectedHypergraphBase[v, e, M],
      [v, e] =>> DirectedHypergraphBase[v, e, Count],
    ]:

    override private[kneiphof] def view[V, E]
      (
        queries: Queries[V, E, M, Hyperarc[V, E]],
        build: => DirectedHypergraphBase[V, E, M],
      )
      : DirectedHypergraphBase[V, E, M] =
      LazyDirectedHypergraph(empty, queries, () => build)

    override def updated[V, E]
      (
        graph: DirectedHypergraphBase[V, E, M],
        edge: Hyperarc[V, E],
        multiplicity: M,
      )
      : DirectedHypergraphBase[V, E, M] = graph.updated(edge, multiplicity)

  /**
    * @return
    *   The kind of directed hypergraphs, in the default representation.
    */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceDirectedHypergraph.empty)

  /**
    * @return
    *   The queries about [[graph]], with its multiplicities read in [[M]] by
    *   [[embed]], answered by [[graph]] itself.
    */
  private[kneiphof] def viewing[V, E, N, M : Multiplicity]
    (
      graph: DirectedHypergraphBase[V, E, N],
      embed: Embedding[N, M],
    )
    : Queries[V, E, M, Hyperarc[V, E]] = Viewing[
    V,
    E,
    N,
    M,
    Hyperarc,
    [v, e] =>> DirectedHypergraphBase[v, e, N],
  ](graph, embed)

/** Directed hypergraphs whose edges form a set, in the default representation. */
object DirectedHypergraph
  extends DirectedHypergraphBase.Kind[Boolean](
    IncidenceDirectedHypergraph.empty,
  )

/** Directed hypergraphs with edge counts, in the default representation. */
object DirectedMultihypergraph
  extends DirectedHypergraphBase.Kind[Count](IncidenceDirectedHypergraph.empty)
