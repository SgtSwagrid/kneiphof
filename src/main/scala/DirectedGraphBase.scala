package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceDirectedGraph
import com.alecdorrington.kneiphof.shapes.{DirectedShaping, Indexing, Shape}
import com.alecdorrington.kneiphof.views.{
  Joining, LazyDirectedGraph, Queries, Viewing,
}
import scala.annotation.targetName

/**
  * A directed graph, whose edges each lead from one vertex to another, with
  * multiplicities in [[M]].
  *
  * Every directed graph is a directed hypergraph. Operations that add edges
  * come in two versions: one for [[Arc]] values, which keeps this a directed
  * graph, and one for [[Hyperarc]] values, which may turn it into a directed
  * hypergraph.
  *
  * Use the aliases [[DirectedGraph]] (edge sets) or [[DirectedMultigraph]]
  * (edge counts) in most code.
  */
trait DirectedGraphBase[+V, +E, M]
  extends DirectedHypergraphBase[V, E, M],
          GraphOps[
            V,
            E,
            M,
            Arc,
            [v, e] =>> DirectedGraphBase[v, e, M],
          ]:

  /**
    * @return
    *   This graph with the multiplicity of [[edge]] set to [[multiplicity]],
    *   adding its vertices if necessary.
    */
  def updated[W >: V, F >: E](edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M]

  override def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] = edge match
    case edge: Arc[W, F] => updated(edge, multiplicity)
    case _               => toDirectedHypergraph.updated(edge, multiplicity)

  override def edge(value: Any): Option[Edge] = edgeValues
    .getKey(value)
    .map(found =>
      new DirectedEdgeOps:
        override val value: Arc[V, E] = found,
    )

  override def transpose: DirectedGraphBase[V, E, M] =
    graphs.imaged(this, graphs.viewing(this), edgeValues)(
      identity,
      edge => List(edge.reverse),
      exact = true,
    )

  /**
    * @return
    *   The sum of this graph and [[that]], which adds the multiplicities of
    *   their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `1 ~> 2 ~> 3`.
    */
  infix def + [W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedGraphBase[W, F, M] = combined(that, embed)(_ + _)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for `+`.
    */
  infix def | [W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedGraphBase[W, F, M] = combined(that, embed)(_ | _)

  /**
    * @return
    *   The disjoint union of this graph and [[that]], whose vertices are tagged
    *   by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[Either[V, W], F, M] = graphs.disjointUnion(this, that)

  /** An alias for [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The graph in which each vertex `v` is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  @targetName("flatMapDirectedGraphVertices")
  def flatMapVertices[W, F >: E]
    (transform: V => DirectedGraphBase[W, F, M])
    : DirectedGraphBase[W, F, M] = graphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The graph obtained by replacing each vertex with the graph it holds (see
    *   [[flatMapVertices]]).
    */
  @targetName("flattenDirectedGraph")
  def flatten[W, F >: E]
    (using nested: V <:< DirectedGraphBase[W, F, M])
    : DirectedGraphBase[W, F, M] = flatMapVertices(nested)

  /**
    * @return
    *   The lexicographic sum (or X-join), in which each vertex `v` is replaced
    *   by a disjoint copy of the graph `transform(v)`.
    */
  @targetName("directedGraphLexicographicSum")
  def lexicographicSum[W, F >: E]
    (transform: V => DirectedGraphBase[W, F, M])
    : DirectedGraphBase[(V, W), F, M] = graphs.lexicographicSum(this, transform)

  /**
    * @return
    *   The lexicographic product `G[H]`, in which each vertex is replaced by a
    *   copy of [[that]].
    */
  def lexicographicProduct[W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[(V, W), F, M] = lexicographicSum(_ => that)

  /**
    * @return
    *   The join of this graph and [[that]], with an extra edge labelled
    *   [[label]] from every vertex of this graph to every vertex of [[that]].
    *   Vertices in both also gain a loop.
    */
  def join[W >: V, F >: E]
    (that: DirectedGraphBase[W, F, M], label: F)
    : DirectedGraphBase[W, F, M] = DirectedGraphBase.Join(this, that) % label

  /**
    * @return
    *   The join `that :~>: this`, with an edge labelled `()` from every vertex
    *   of [[that]] to every vertex of this graph (see [[join]]). The new edges
    *   can be relabelled, as in `(that :~>: this) % label`.
    */
  infix def :~>: [W >: V, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase.Join[W, F, Unit, M] = DirectedGraphBase.Join(that, this)

  override infix def :~> [W >: V]
    (vertex: W)
    : DirectedGraphBase.Join[W, E, Unit, M] =
    DirectedGraphBase.Join(this, graphs.vertex(vertex))

  override infix def ~>: [W >: V]
    (vertex: W)
    : DirectedGraphBase.Join[W, E, Unit, M] =
    DirectedGraphBase.Join(graphs.vertex(vertex), this)

  /** This graph as a directed hypergraph, in the default representation. */
  def toDirectedHypergraph: DirectedHypergraphBase[V, E, M] =
    val hypergraphs = DirectedHypergraphBase.kind[M]
    hypergraphs.lazily(this)(
      DirectedHypergraphBase.viewing(this, Embedding.identity),
      hypergraphs.fromMultiplicities(vertexValues, edgeValues),
    )

  /**
    * The undirected graph obtained by forgetting the direction of every edge,
    * in the default representation.
    */
  override def underlying: GraphBase[V, E, M] = GraphBase
    .kind[M]
    .imaged(this, graphs.viewing(this), edgeValues)(
      identity,
      edge => List(kneiphof.Edge(edge.source, edge.target, edge.label)),
      exact = false,
    )

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : DirectedGraphBase[W, F, M] =
    graphs.imaged(this, graphs.viewing(this), edgeValues)(
      vertexMap,
      edge => List(edge.map(vertexMap, labelMap)),
      exact = false,
    )

  override def view: DirectedGraphBase[V, E, M] =
    graphs.view(graphs.viewing(this), this)

  override protected def kindName: String = "DirectedGraph"

  override protected def refined
    (
      keep: V => Boolean,
      reweigh: (Any, M) => M,
      changed: => Iterable[Any],
      operands: AnyGraph[?, ?, ?]*,
    )
    : DirectedGraphBase[V, E, M] =
    graphs.refined(this, keep, reweigh, changed, operands)

  /**
    * @return
    *   The union or sum of this graph and [[that]], whose multiplicities are
    *   combined by [[combine]].
    */
  private def combined[W >: V, F >: E, N]
    (
      that: DirectedGraphBase[W, F, N],
      embed: Embedding[N, M],
    )
    (combine: (M, M) => M)
    : DirectedGraphBase[W, F, M] = graphs.combined[W, F](
    this,
    DirectedGraphBase.viewing(that, embed),
    that,
    that.vertexValues,
    that.edgeValues.map((edge, n) => edge -> embed(n)),
  )(combine)

  /** The kind of this graph, in its own representation. */
  private def graphs: DirectedGraphBase.Kind[M] = DirectedGraphBase.Kind(empty)

/** The companion object for [[DirectedGraphBase]]. */
object DirectedGraphBase:

  /**
    * The join of two directed graphs, which remembers its new edges so that
    * they can be relabelled with `%`.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M]
    extends DirectedGraphBase[V, E | L, M],
            DirectedHypergraphBase.Join[V, E, L, M]:

    override infix def % [F](label: F): Join[V, E, F, M]

  /** The companion object for [[Join]]. */
  object Join:

    /**
      * @return
      *   The join from [[left]] to [[right]], whose new edges are labelled
      *   `()`.
      */
    def apply[V, E, M : Multiplicity]
      (
        left: DirectedGraphBase[V, E, M],
        right: DirectedGraphBase[V, E, M],
      )
      : Join[V, E, Unit, M] = Joined(left, right, ())

    /**
      * A join, held as the graphs it joins. It's always lazy, since its size is
      * the product of theirs.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: DirectedGraphBase[V, E, M],
        right: DirectedGraphBase[V, E, M],
        label: L,
      )
      extends LazyDirectedGraph[V, E | L, M](
        left.empty,
        Joining[
          V,
          E,
          L,
          M,
          Arc,
          [v, e] =>> DirectedGraphBase[v, e, M],
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

  /** The kind of directed graphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: DirectedGraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Arc,
      [v, e] =>> DirectedGraphBase[v, e, M],
      [v, e] =>> DirectedGraphBase[v, e, Count],
    ],
            Shapes[[v, e] =>> DirectedGraphBase[v, e, M]]:

    override private[kneiphof] def view[V, E]
      (
        queries: Queries[V, E, M, Arc[V, E]],
        build: => DirectedGraphBase[V, E, M],
      )
      : DirectedGraphBase[V, E, M] =
      LazyDirectedGraph(empty, queries, () => build)

    override def updated[V, E]
      (
        graph: DirectedGraphBase[V, E, M],
        edge: Arc[V, E],
        multiplicity: M,
      )
      : DirectedGraphBase[V, E, M] = graph.updated(edge, multiplicity)

    override private[kneiphof] def shaped[V]
      (vertices: Indexing[V], shape: Shape)
      : DirectedGraphBase[V, Unit, M] =
      val shaping = DirectedShaping[V, M](vertices, shape)
      view(
        shaping,
        from(shaping.vertexValues, shaping.edges),
      )

    override private[kneiphof] def complete(order: Int): Shape = Shape
      .CompleteDigraph(order)

  /** @return The kind of directed graphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceDirectedGraph.empty)

  /**
    * @return
    *   The queries about [[graph]], with its multiplicities read in [[M]] by
    *   [[embed]], answered by [[graph]] itself.
    */
  private[kneiphof] def viewing[V, E, N, M : Multiplicity]
    (
      graph: DirectedGraphBase[V, E, N],
      embed: Embedding[N, M],
    )
    : Queries[V, E, M, Arc[V, E]] = Viewing[
    V,
    E,
    N,
    M,
    Arc,
    [v, e] =>> DirectedGraphBase[v, e, N],
  ](graph, embed)

/** Directed graphs whose edges form a set, in the default representation. */
object DirectedGraph
  extends DirectedGraphBase.Kind[Boolean](IncidenceDirectedGraph.empty)

/** Directed graphs with edge counts, in the default representation. */
object DirectedMultigraph
  extends DirectedGraphBase.Kind[Count](IncidenceDirectedGraph.empty)
