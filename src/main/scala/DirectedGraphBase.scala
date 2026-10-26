package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceDirectedGraph
import com.alecdorrington.kneiphof.shapes.{DirectedShapeGraph, Indexing, Shape}
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

  override def transpose: DirectedGraphBase[V, E, M]

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
    : DirectedGraphBase[W, F, M] = graphs.merged(
    this,
    that.vertexValues,
    that.edgeValues.map((e, n) => e -> embed(n)),
  )(_ + _)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for `+`.
    */
  infix def | [W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedGraphBase[W, F, M] = graphs.merged(
    this,
    that.vertexValues,
    that.edgeValues.map((e, n) => e -> embed(n)),
  )(_ | _)

  /**
    * @return
    *   The disjoint union of this graph and [[that]], whose vertices are tagged
    *   by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[Either[V, W], F, M] =
    mapVertices[Either[V, W]](Left(_)) |
      that.mapVertices[Either[V, W]](Right(_))

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
    : DirectedGraphBase[(V, W), F, M] =
    flatMapVertices(v => transform(v).mapVertices((v, _)))

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
    : DirectedGraphBase[W, F, M] =
    graphs.joined(this, that)(DirectedEdge(_, _, label))

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
    DirectedHypergraphBase.kind[M].fromMultiplicities(vertexValues, edgeValues)

  /**
    * The undirected graph obtained by forgetting the direction of every edge,
    * in the default representation.
    */
  override def underlying: GraphBase[V, E, M] = GraphBase
    .kind[M]
    .fromMultiplicities(
      vertexValues,
      edgeValues
        .toSeq
        .map: (edge, multiplicity) =>
          kneiphof.Edge(edge.source, edge.target, edge.label) -> multiplicity,
    )

  override protected def kindName: String = "DirectedGraph"

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

    /** A join, held as the graphs it joins. */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        override protected val left: DirectedGraphBase[V, E, M],
        override protected val right: DirectedGraphBase[V, E, M],
        label: L,
      )
      extends Join[V, E, L, M],
              Joining[
                V,
                E,
                L,
                M,
                Arc,
                [v, e] =>> DirectedGraphBase[v, e, M],
              ]:

      override protected lazy val delegate: DirectedGraphBase[V, E | L, M] =
        left.join[V, E | L](right, label)

      override def updated[W >: V, F >: E | L]
        (edge: Arc[W, F], multiplicity: M)
        : DirectedGraphBase[W, F, M] = delegate.updated(edge, multiplicity)

      override def transpose: DirectedGraphBase[V, E | L, M] =
        delegate.transpose

      override infix def % [F](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

      override protected def link(source: V, target: V): Arc[V, E | L] =
        DirectedEdge(source, target, label)

      override protected def links(edge: Any): Int =
        Joining.directedLinks(left, right, label, edge)

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
      DirectedShapeGraph(this, vertices, shape)

    override private[kneiphof] def complete(order: Int): Shape = Shape
      .CompleteDigraph(order)

  /** @return The kind of directed graphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceDirectedGraph.empty)

/** Directed graphs whose edges form a set, in the default representation. */
object DirectedGraph
  extends DirectedGraphBase.Kind[Boolean](IncidenceDirectedGraph.empty)

/** Directed graphs with edge counts, in the default representation. */
object DirectedMultigraph
  extends DirectedGraphBase.Kind[Count](IncidenceDirectedGraph.empty)
