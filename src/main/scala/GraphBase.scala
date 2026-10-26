package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceGraph
import com.alecdorrington.kneiphof.shapes.{Indexing, Shape, Shaping}
import com.alecdorrington.kneiphof.views.{Joining, LazyGraph, Queries, Viewing}
import scala.annotation.targetName

/**
  * A graph, whose edges each join two vertices, with multiplicities in [[M]].
  *
  * Every graph is a hypergraph. Operations that add edges come in two versions:
  * one for [[kneiphof.Edge]] values, which keeps this a graph, and one for
  * [[Hyperedge]] values, which may turn it into a hypergraph.
  *
  * Use the aliases [[Graph]] (edge sets) or [[Multigraph]] (edge counts) in
  * most code.
  */
trait GraphBase[+V, +E, M]
  extends HypergraphBase[V, E, M],
          GraphOps[V, E, M, Edge, [v, e] =>> GraphBase[v, e, M]]:

  /**
    * @return
    *   This graph with the multiplicity of [[edge]] set to [[multiplicity]],
    *   adding its vertices if necessary.
    */
  def updated[W >: V, F >: E]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: M,
    )
    : GraphBase[W, F, M]

  override def updated[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] = edge match
    case edge: kneiphof.Edge[W, F] => updated(edge, multiplicity)
    case _                         => toHypergraph.updated(edge, multiplicity)

  override def edge(value: Any): Option[Edge] = edgeValues
    .getKey(value)
    .map(found =>
      new EdgeOps:
        override val value: kneiphof.Edge[V, E] = found,
    )

  /**
    * @return
    *   The sum of this graph and [[that]], which adds the multiplicities of
    *   their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `1 ~ 2 ~ 3`.
    */
  infix def + [W >: V, F >: E, N]
    (that: GraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : GraphBase[W, F, M] = combined(that, embed)(_ + _)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for `+`.
    */
  infix def | [W >: V, F >: E, N]
    (that: GraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : GraphBase[W, F, M] = combined(that, embed)(_ | _)

  /**
    * @return
    *   The disjoint union of this graph and [[that]], whose vertices are tagged
    *   by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[Either[V, W], F, M] = graphs.disjointUnion(this, that)

  /** An alias for [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The graph in which each vertex `v` is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  @targetName("flatMapGraphVertices")
  def flatMapVertices[W, F >: E]
    (transform: V => GraphBase[W, F, M])
    : GraphBase[W, F, M] = graphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The graph obtained by replacing each vertex with the graph it holds (see
    *   [[flatMapVertices]]).
    */
  @targetName("flattenGraph")
  def flatten[W, F >: E]
    (using nested: V <:< GraphBase[W, F, M])
    : GraphBase[W, F, M] = flatMapVertices(nested)

  /**
    * @return
    *   The lexicographic sum (or X-join), in which each vertex `v` is replaced
    *   by a disjoint copy of the graph `transform(v)`.
    */
  @targetName("graphLexicographicSum")
  def lexicographicSum[W, F >: E]
    (transform: V => GraphBase[W, F, M])
    : GraphBase[(V, W), F, M] = graphs.lexicographicSum(this, transform)

  /**
    * @return
    *   The lexicographic product `G[H]`, in which each vertex is replaced by a
    *   copy of [[that]].
    */
  def lexicographicProduct[W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[(V, W), F, M] = lexicographicSum(_ => that)

  /**
    * @return
    *   The join of this graph and [[that]], with an extra edge labelled
    *   [[label]] between every vertex of this graph and every vertex of
    *   [[that]]. Vertices in both also gain a loop.
    */
  def join[W >: V, F >: E]
    (that: GraphBase[W, F, M], label: F)
    : GraphBase[W, F, M] = GraphBase.Join(this, that) % label

  /**
    * @return
    *   The join `that :~: this`, with an edge labelled `()` between every
    *   vertex of [[that]] and every vertex of this graph (see [[join]]). The
    *   new edges can be relabelled, as in `(that :~: this) % label`.
    */
  infix def :~: [W >: V, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase.Join[W, F, Unit, M] = GraphBase.Join(that, this)

  override infix def :~ [W >: V](vertex: W): GraphBase.Join[W, E, Unit, M] =
    GraphBase.Join(this, graphs.vertex(vertex))

  override infix def ~: [W >: V](vertex: W): GraphBase.Join[W, E, Unit, M] =
    this :~ vertex

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : GraphBase[W, F, M] =
    graphs.imaged(this, graphs.viewing(this), edgeValues)(
      vertexMap,
      edge => List(edge.map(vertexMap, labelMap)),
      exact = false,
    )

  override def view: GraphBase[V, E, M] =
    graphs.view(graphs.viewing(this), this)

  /** This graph as a hypergraph, in the default representation. */
  def toHypergraph: HypergraphBase[V, E, M] =
    val hypergraphs = HypergraphBase.kind[M]
    hypergraphs.lazily(this)(
      HypergraphBase.viewing(this, Embedding.identity),
      hypergraphs.fromMultiplicities(vertexValues, edgeValues),
    )

  /**
    * The directed graph with an edge each way for every edge of this graph, and
    * a single loop for every loop.
    */
  def symmetric: DirectedGraphBase[V, E, M] = DirectedGraphBase
    .kind[M]
    .imaged(this, graphs.viewing(this), edgeValues)(
      identity,
      edge =>
        val forward = DirectedEdge(edge.first, edge.second, edge.label)
        List(forward, forward.reverse).distinct
      ,
      exact = false,
    )

  override protected def kindName: String = "Graph"

  override protected def refined
    (
      keep: V => Boolean,
      reweigh: (Any, M) => M,
      changed: => Iterable[Any],
      operands: AnyGraph[?, ?, ?]*,
    )
    : GraphBase[V, E, M] =
    graphs.refined(this, keep, reweigh, changed, operands)

  /**
    * @return
    *   The union or sum of this graph and [[that]], whose multiplicities are
    *   combined by [[combine]].
    */
  private def combined[W >: V, F >: E, N]
    (
      that: GraphBase[W, F, N],
      embed: Embedding[N, M],
    )
    (combine: (M, M) => M)
    : GraphBase[W, F, M] = graphs.combined[W, F](
    this,
    GraphBase.viewing(that, embed),
    that,
    that.vertexValues,
    that.edgeValues.map((edge, n) => edge -> embed(n)),
  )(combine)

  /** The kind of this graph, in its own representation. */
  private def graphs: GraphBase.Kind[M] = GraphBase.Kind(empty)

/** The companion object for [[GraphBase]]. */
object GraphBase:

  /**
    * The join of two graphs, which remembers its new edges so that they can be
    * relabelled with `%`.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M]
    extends GraphBase[V, E | L, M], HypergraphBase.Join[V, E, L, M]:

    override infix def % [F](label: F): Join[V, E, F, M]

  /** The companion object for [[Join]]. */
  object Join:

    /**
      * @return
      *   The join of [[left]] and [[right]], whose new edges are labelled `()`.
      */
    def apply[V, E, M : Multiplicity]
      (
        left: GraphBase[V, E, M],
        right: GraphBase[V, E, M],
      )
      : Join[V, E, Unit, M] = Joined(left, right, ())

    /**
      * A join, held as the graphs it joins. It's always lazy, since its size is
      * the product of theirs.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: GraphBase[V, E, M],
        right: GraphBase[V, E, M],
        label: L,
      )
      extends LazyGraph[V, E | L, M](
        left.empty,
        Joining[
          V,
          E,
          L,
          M,
          kneiphof.Edge,
          [v, e] =>> GraphBase[v, e, M],
        ](
          left,
          right,
          kneiphof.Edge(_, _, label),
          Joining.undirectedLinks(left, right, label, _),
        ),
        () =>
          Kind(left.empty).joined[V, E | L](left, right)(
            kneiphof.Edge(_, _, label),
          ),
      ),
              Join[V, E, L, M]:

      override infix def % [F](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

  /** The kind of graphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: GraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Edge,
      [v, e] =>> GraphBase[v, e, M],
      [v, e] =>> GraphBase[v, e, Count],
    ],
            Shapes[[v, e] =>> GraphBase[v, e, M]]:

    override private[kneiphof] def view[V, E]
      (
        queries: Queries[V, E, M, Edge[V, E]],
        build: => GraphBase[V, E, M],
      )
      : GraphBase[V, E, M] = LazyGraph(empty, queries, () => build)

    override def updated[V, E]
      (
        graph: GraphBase[V, E, M],
        edge: Edge[V, E],
        multiplicity: M,
      )
      : GraphBase[V, E, M] = graph.updated(edge, multiplicity)

    override private[kneiphof] def shaped[V]
      (vertices: Indexing[V], shape: Shape)
      : GraphBase[V, Unit, M] =
      val shaping = Shaping[V, M](vertices, shape)
      view(
        shaping,
        from(shaping.vertexValues, shaping.edges),
      )

    override private[kneiphof] def directed: Boolean = false

  /** @return The kind of graphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceGraph.empty)

  /**
    * @return
    *   The queries about [[graph]], with its multiplicities read in [[M]] by
    *   [[embed]], answered by [[graph]] itself.
    */
  private[kneiphof] def viewing[V, E, N, M : Multiplicity]
    (
      graph: GraphBase[V, E, N],
      embed: Embedding[N, M],
    )
    : Queries[V, E, M, kneiphof.Edge[V, E]] = Viewing[
    V,
    E,
    N,
    M,
    kneiphof.Edge,
    [v, e] =>> GraphBase[v, e, N],
  ](graph, embed)

/** Graphs whose edges form a set, in the default representation. */
object Graph extends GraphBase.Kind[Boolean](IncidenceGraph.empty)

/** Graphs with edge counts, in the default representation. */
object Multigraph extends GraphBase.Kind[Count](IncidenceGraph.empty)
