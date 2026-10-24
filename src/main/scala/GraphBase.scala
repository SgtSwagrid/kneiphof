package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceGraph
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

  /** @return This graph with one more copy of [[edge]]. */
  infix def + [W >: V, F >: E](edge: kneiphof.Edge[W, F]): GraphBase[W, F, M] =
    updated(edge, multiplicity(edge) + algebra.one)

  /**
    * @return
    *   The sum of this graph and [[that]], which adds the multiplicities of
    *   their edges. For edge sets, this is the same as their union.
    */
  infix def + [W >: V, F >: E](that: GraphBase[W, F, M]): GraphBase[W, F, M] =
    graphs.merged(this, that)(_ + _)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide.
    */
  infix def | [W >: V, F >: E](that: GraphBase[W, F, M]): GraphBase[W, F, M] =
    graphs.merged(this, that)(_ | _)

  /**
    * @return
    *   The disjoint union of this graph and [[that]], whose vertices are tagged
    *   by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[Either[V, W], F, M] = mapVertices[Either[V, W]](Left(_)) |
    that.mapVertices[Either[V, W]](Right(_))

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
    : GraphBase[(V, W), F, M] =
    flatMapVertices(v => transform(v).mapVertices((v, _)))

  /**
    * @return
    *   The lexicographic product `G[H]`, in which each vertex is replaced by a
    *   copy of [[that]].
    */
  def lexicographicProduct[W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[(V, W), F, M] = lexicographicSum(_ => that)

  /** This graph as a hypergraph, in the default representation. */
  def toHypergraph: HypergraphBase[V, E, M] = HypergraphBase
    .kind[M]
    .fromMultiplicities(vertexValues, edgeValues)

  /**
    * The directed graph with an edge each way for every edge of this graph, and
    * a single loop for every loop.
    */
  def symmetric: DirectedGraphBase[V, E, M] = DirectedGraphBase
    .kind[M]
    .fromMultiplicities(
      vertexValues,
      edgeValues
        .toSeq
        .flatMap: (edge, multiplicity) =>
          val forward = DirectedEdge(edge.first, edge.second, edge.label)
          Seq(forward, forward.reverse).distinct.map(_ -> multiplicity),
    )

  override protected def kindName: String = "Graph"

  /** The kind of this graph, in its own representation. */
  private def graphs: GraphBase.Kind[M] = GraphBase.Kind(empty)

/** The companion object for [[GraphBase]]. */
object GraphBase:

  /** The kind of graphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: GraphBase[Nothing, Nothing, M])
    extends GraphKind[M, Edge, [v, e] =>> GraphBase[v, e, M]]:

    override def updated[V, E]
      (
        graph: GraphBase[V, E, M],
        edge: Edge[V, E],
        multiplicity: M,
      )
      : GraphBase[V, E, M] = graph.updated(edge, multiplicity)

  /** @return The kind of graphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceGraph.empty)

/** Graphs whose edges form a set, in the default representation. */
object Graph extends GraphBase.Kind[Boolean](IncidenceGraph.empty)

/** Graphs with edge counts, in the default representation. */
object Multigraph extends GraphBase.Kind[Count](IncidenceGraph.empty)
