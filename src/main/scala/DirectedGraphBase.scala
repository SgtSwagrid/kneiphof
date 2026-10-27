package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{IncidenceStructure, Support}
import com.alecdorrington.kneiphof.shapes.{Index, Rule, Shape}
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
  def updated[W >: V, F >: E]
    (edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M] =
    graphs.updated[W, F](this, edge, multiplicity)

  override def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] = edge match
    case edge: Arc[W, F] => updated(edge, multiplicity)
    case _               => toDirectedHypergraph.updated(edge, multiplicity)

  override def edge(value: Any): Option[Edge] = found(value).map(found =>
    new DirectedEdgeOps:
      override val value: Arc[V, E] = found,
  )

  override def transpose: DirectedGraphBase[V, E, M] = graphs.imaged(structure)(
    identity,
    edge => List(edge.reverse),
    kept = true,
  )

  /**
    * @return
    *   The sum of this graph and [[that]], which adds the multiplicities of
    *   their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `1 ~> 2 ~> 3`.
    */
  def sum[W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedGraphBase[W, F, M] =
    graphs.sum[W, F](this, embed.embedded(that.structure))

  /** Alias of [[sum]]. */
  infix def + [W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using Embedding[N, M])
    : DirectedGraphBase[W, F, M] = sum(that)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for [[sum]].
    */
  def union[W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedGraphBase[W, F, M] =
    graphs.union[W, F](this, embed.embedded(that.structure))

  /** Alias of [[union]]. */
  infix def | [W >: V, F >: E, N]
    (that: DirectedGraphBase[W, F, N])
    (using Embedding[N, M])
    : DirectedGraphBase[W, F, M] = union(that)

  /**
    * @return
    *   The disjoint union of this graph and [[that]], whose vertices are tagged
    *   by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[Either[V, W], F, M] = graphs.disjointUnion(this, that)

  /** Alias of [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The graph in which each vertex `v` is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge. Each end is
    *   replaced independently, so a loop at `v` becomes a copy for every
    *   ordered pair of vertices of `transform(v)`: a loop at each, and an edge
    *   each way between every two.
    */
  @targetName("flatMapDirectedGraphVertices")
  def flatMapVertices[W, F >: E]
    (transform: V => DirectedGraphBase[W, F, M])
    : DirectedGraphBase[W, F, M] = graphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The graph obtained by replacing each vertex with the graph it holds (see
    *   [[flatMapVertices]], including how loops are replaced).
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
    *   The Cartesian product `G □ H` of this graph and [[that]], on the pairs
    *   `(v, w)` of their vertices, with a copy of each edge of this graph for
    *   each vertex `w` of [[that]], and a copy of each edge of [[that]] for
    *   each vertex `v` of this graph. Copies keep their labels. The copies of
    *   two loops which differ only in their end coincide, which is one edge,
    *   for edge sets. So `path(m) □ path(n)` is a grid.
    */
  def cartesianProduct[W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[(V, W), F, M] =
    graphs.cartesianProduct[V, W, F](this, that)

  /** Alias of [[cartesianProduct]]. */
  infix def □ [W, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase[(V, W), F, M] = cartesianProduct(that)

  /**
    * @return
    *   The join of this graph and [[that]], with an extra edge labelled `()`
    *   from every vertex of this graph to every vertex of [[that]], which
    *   `labelled` can relabel. This is the flattening of the edge
    *   `this ~> that`, so it pairs each vertex of this graph with each vertex
    *   of [[that]] even where the two share vertices: a vertex in both gains a
    *   loop, and two vertices in both gain an edge each way.
    */
  def join[W >: V, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase.Join[W, F, Unit, M] = DirectedGraphBase.Join(this, that)

  /** Alias of [[join]]. */
  infix def :~>: [W >: V, F >: E]
    (that: DirectedGraphBase[W, F, M])
    : DirectedGraphBase.Join[W, F, Unit, M] = that.join(this)

  override def coneTo[W >: V]
    (vertex: W)
    : DirectedGraphBase.Join[W, E, Unit, M] =
    DirectedGraphBase.Join(this, graphs.vertex(vertex))

  override infix def :~> [W >: V]
    (vertex: W)
    : DirectedGraphBase.Join[W, E, Unit, M] = coneTo(vertex)

  override def coneFrom[W >: V]
    (vertex: W)
    : DirectedGraphBase.Join[W, E, Unit, M] =
    DirectedGraphBase.Join(graphs.vertex(vertex), this)

  override infix def ~>: [W >: V]
    (vertex: W)
    : DirectedGraphBase.Join[W, E, Unit, M] = coneFrom(vertex)

  /** This graph as a directed hypergraph. */
  def toDirectedHypergraph: DirectedHypergraphBase[V, E, M] =
    DirectedHypergraphBase.kind[M].wrap(structure)

  /** The undirected graph obtained by forgetting the direction of every edge. */
  override def underlying: GraphBase[V, E, M] = GraphBase
    .kind[M]
    .imaged(structure)(
      identity,
      edge => List(kneiphof.Edge(edge.source, edge.target, edge.label)),
      kept = algebra.additive,
    )

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : DirectedGraphBase[W, F, M] = graphs.imaged(structure)(
    vertexMap,
    edge => List(edge.map(vertexMap, labelMap)),
    kept = algebra.additive,
  )

  /**
    * @return
    *   The edge set of this graph, in which each edge with any copies is
    *   present once.
    */
  override def support
    (using embed: Embedding[M, Boolean])
    : DirectedGraphBase[V, E, Boolean] = DirectedGraphBase
    .kind[Boolean]
    .wrap(embed.embedded(structure))

  /**
    * @return
    *   This graph as a multigraph, with one copy of each edge. An edge set
    *   doesn't count copies, so it only mixes with multigraphs through this, as
    *   in `m & g.counted`, or `m - g.counted` to remove one copy of each edge
    *   of `g`.
    */
  override def counted
    (using edges: M =:= Boolean)
    : DirectedGraphBase[V, E, Count] = DirectedGraphBase
    .kind[Count]
    .wrap(Support.counted(
      edges.substituteCo[[m] =>> IncidenceStructure[V, m, Arc[V, E]]](structure),
    ))

  override protected def kindName: String = "DirectedGraph"

  override def empty: DirectedGraphBase[Nothing, Nothing, M] = graphs.empty

  override def addVertex[W >: V](vertex: W): DirectedGraphBase[W, E, M] = graphs
    .withVertex[W, E](this, vertex)

  override def removeVertex(vertex: Any): DirectedGraphBase[V, E, M] = graphs
    .withoutVertex(this, vertex)

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : DirectedGraphBase[V, E, M] =
    graphs.withMultiplicity(this, edge, multiplicity)

  override def materialise: DirectedGraphBase[V, E, M] =
    graphs.materialise(this)

  override protected def subgraph
    (
      keep: V => Boolean,
      keepEdge: Option[Arc[V, E] => Boolean],
    )
    : DirectedGraphBase[V, E, M] = graphs.subgraph(this, keep, keepEdge)

  override protected def intersected[N]
    (
      that: AnyGraph[Any, Any, N],
      embed: Embedding[N, M],
    )
    : DirectedGraphBase[V, E, M] =
    graphs.intersection(this, embed.embedded(that.structure))

  override protected def subtracted[N]
    (
      that: AnyGraph[Any, Any, N],
      embed: Embedding[N, M],
    )
    : DirectedGraphBase[V, E, M] =
    graphs.difference(this, embed.embedded(that.structure))

  /** The kind of this graph, in its own representation. */
  private def graphs: DirectedGraphBase.Kind[M] = DirectedGraphBase.kind[M]

/** The companion object for [[DirectedGraphBase]]. */
object DirectedGraphBase:

  /**
    * The join of two directed graphs, which remembers its new edges so that
    * they can be relabelled with `labelled`. Equality compares only the graphs,
    * so equal joins can relabel different edges (see [[HypergraphBase.Join]]).
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M]
    extends DirectedGraphBase[V, E | L, M],
            DirectedHypergraphBase.Join[V, E, L, M]:

    override def labelled[F : NotGraph](label: F): Join[V, E, F, M]

    override infix def % [F : NotGraph](label: F): Join[V, E, F, M] =
      labelled(label)

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
      * A join, held as the graphs it joins. Its new edges are computed from
      * them only when they're needed.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: DirectedGraphBase[V, E, M],
        right: DirectedGraphBase[V, E, M],
        label: L,
      )
      extends Join[V, E, L, M]:

      override private[kneiphof] lazy val structure
        : IncidenceStructure[V, M, Arc[V, E | L]] =
        incidence.Join[V, E, L, M, Arc](
          left.structure,
          right.structure,
          DirectedEdge(_, _, label),
          incidence
            .Join
            .directedLinks(
              left.structure,
              right.structure,
              label,
              _,
            ),
        )

      override def labelled[F : NotGraph](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

  /** A directed graph with the given incidence [[structure]]. */
  private final class Wrapper[V, E, M : Multiplicity]
    (
      override private[kneiphof] val structure: IncidenceStructure[
        V,
        M,
        Arc[V, E],
      ],
    )
    extends DirectedGraphBase[V, E, M]

  /** The kind of directed graphs. */
  class Kind[M : Multiplicity]
    extends GraphKind[
      M,
      Arc,
      [v, e] =>> DirectedGraphBase[v, e, M],
      [v, e] =>> DirectedGraphBase[v, e, Count],
    ],
            DirectedShapes[[v, e] =>> DirectedGraphBase[v, e, M]]:

    override private[kneiphof] def wrap[V, E]
      (structure: IncidenceStructure[V, M, Arc[V, E]])
      : DirectedGraphBase[V, E, M] = Wrapper(structure)

    override private[kneiphof] def shaped[V]
      (index: Index[V], rule: Rule)
      : DirectedGraphBase[V, Unit, M] = wrap(Shape.directed[V, M](index, rule))

    override private[kneiphof] def coned[V]
      (hub: V, index: Index[V], rule: Rule)
      : DirectedGraphBase[V, Unit, M] = shaped(index, rule).coneFrom(hub)

    override private[kneiphof] def product[V, W]
      (
        left: Index[V],
        first: Rule,
        right: Index[W],
        second: Rule,
      )
      : DirectedGraphBase[(V, W), Unit, M] = shaped(left, first)
      .cartesianProduct(shaped(right, second))

  /** @return The kind of directed graphs with multiplicities in [[M]]. */
  def kind[M : Multiplicity]: Kind[M] = Kind[M]()

/** Directed graphs whose edges form a set. */
object DirectedGraph extends DirectedGraphBase.Kind[Boolean]

/** Directed graphs with edge counts. */
object DirectedMultigraph extends DirectedGraphBase.Kind[Count]
