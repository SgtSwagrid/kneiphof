package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceDirectedGraph
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

  /** @return This graph with one more copy of [[edge]]. */
  infix def + [W >: V, F >: E](edge: Arc[W, F]): DirectedGraphBase[W, F, M] =
    updated(edge, multiplicity(edge) + algebra.one)

  /**
    * @return
    *   The sum of this graph and [[that]], which adds the multiplicities of
    *   their edges. For edge sets, this is the same as their union.
    */
  infix def + [W >: V, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[W, F, M] = graphs.merged(this, that)(_ + _)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide.
    */
  infix def | [W >: V, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[W, F, M] = graphs.merged(this, that)(_ | _)

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

  /** The kind of directed graphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: DirectedGraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Arc,
      [v, e] =>> DirectedGraphBase[v, e, M],
    ]:

    override def updated[V, E]
      (
        graph: DirectedGraphBase[V, E, M],
        edge: Arc[V, E],
        multiplicity: M,
      )
      : DirectedGraphBase[V, E, M] = graph.updated(edge, multiplicity)

  /** @return The kind of directed graphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceDirectedGraph.empty)

/** Directed graphs whose edges form a set, in the default representation. */
object DirectedGraph
  extends DirectedGraphBase.Kind[Boolean](IncidenceDirectedGraph.empty)

/** Directed graphs with edge counts, in the default representation. */
object DirectedMultigraph
  extends DirectedGraphBase.Kind[Count](IncidenceDirectedGraph.empty)
