package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{IncidenceStructure, Support}
import com.alecdorrington.kneiphof.shapes.{Index, Rule, Shape}
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
    : GraphBase[W, F, M] = graphs.updated[W, F](this, edge, multiplicity)

  override def updated[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] = edge match
    case edge: kneiphof.Edge[W, F] => updated(edge, multiplicity)
    case _                         => toHypergraph.updated(edge, multiplicity)

  override def edge(value: Any): Option[Edge] = found(value).map(found =>
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
  def sum[W >: V, F >: E, N]
    (that: GraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : GraphBase[W, F, M] =
    graphs.sum[W, F](this, embed.embedded(that.structure))

  /** Alias of [[sum]]. */
  infix def + [W >: V, F >: E, N]
    (that: GraphBase[W, F, N])
    (using Embedding[N, M])
    : GraphBase[W, F, M] = sum(that)

  /**
    * @return
    *   The union of this graph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for [[sum]].
    */
  def union[W >: V, F >: E, N]
    (that: GraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : GraphBase[W, F, M] =
    graphs.union[W, F](this, embed.embedded(that.structure))

  /** Alias of [[union]]. */
  infix def | [W >: V, F >: E, N]
    (that: GraphBase[W, F, N])
    (using Embedding[N, M])
    : GraphBase[W, F, M] = union(that)

  /**
    * @return
    *   The disjoint union of this graph and [[that]], whose vertices are tagged
    *   by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[Either[V, W], F, M] = graphs.disjointUnion(this, that)

  /** Alias of [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The graph in which each vertex `v` is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge. Each end is
    *   replaced independently, even where a vertex repeats, so a loop at `v`
    *   becomes a copy for every ordered pair of vertices of `transform(v)`: a
    *   loop at each, and two copies of the edge between every two (one, for
    *   edge sets).
    */
  @targetName("flatMapGraphVertices")
  def flatMapVertices[W, F >: E]
    (transform: V => GraphBase[W, F, M])
    : GraphBase[W, F, M] = graphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The graph obtained by replacing each vertex with the graph it holds (see
    *   [[flatMapVertices]], including how loops are replaced).
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
    *   The Cartesian product `G □ H` of this graph and [[that]], on the pairs
    *   `(v, w)` of their vertices, with a copy of each edge of this graph for
    *   each vertex `w` of [[that]], and a copy of each edge of [[that]] for
    *   each vertex `v` of this graph. Copies keep their labels. The copies of
    *   two loops which differ only in their end coincide, which is one edge,
    *   for edge sets. So `path(m) □ path(n)` is a grid.
    */
  def cartesianProduct[W, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase[(V, W), F, M] = graphs.cartesianProduct[V, W, F](this, that)

  /** Alias of [[cartesianProduct]]. */
  infix def □ [W, F >: E](that: GraphBase[W, F, M]): GraphBase[(V, W), F, M] =
    cartesianProduct(that)

  /**
    * @return
    *   The join of this graph and [[that]], with an extra edge labelled `()`
    *   between every vertex of this graph and every vertex of [[that]], which
    *   `labelled` can relabel. This is the flattening of the edge
    *   `this ~ that`, so it pairs each vertex of this graph with each vertex of
    *   [[that]] even where the two share vertices: a vertex in both gains a
    *   loop, and two vertices in both are joined once for each way of pairing
    *   them (which is just once, for edge sets).
    */
  def join[W >: V, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase.Join[W, F, Unit, M] = GraphBase.Join(this, that)

  /** Alias of [[join]]. */
  infix def :~: [W >: V, F >: E]
    (that: GraphBase[W, F, M])
    : GraphBase.Join[W, F, Unit, M] = that.join(this)

  override def cone[W >: V](vertex: W): GraphBase.Join[W, E, Unit, M] =
    GraphBase.Join(this, graphs.vertex(vertex))

  override infix def :~ [W >: V](vertex: W): GraphBase.Join[W, E, Unit, M] =
    cone(vertex)

  override infix def ~: [W >: V](vertex: W): GraphBase.Join[W, E, Unit, M] =
    cone(vertex)

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : GraphBase[W, F, M] = graphs.imaged(structure)(
    vertexMap,
    edge => List(edge.map(vertexMap, labelMap)),
    kept = algebra.additive,
  )

  /** This graph as a hypergraph. */
  def toHypergraph: HypergraphBase[V, E, M] = HypergraphBase
    .kind[M]
    .wrap(structure)

  /**
    * The directed graph with an edge each way for every edge of this graph, and
    * a single loop for every loop.
    */
  def symmetric: DirectedGraphBase[V, E, M] = DirectedGraphBase
    .kind[M]
    .imaged(structure)(
      identity,
      edge =>
        val forward = DirectedEdge(edge.first, edge.second, edge.label)
        List(forward, forward.reverse).distinct
      ,
      kept = false,
    )

  /**
    * @return
    *   The edge set of this graph, in which each edge with any copies is
    *   present once.
    */
  override def support
    (using embed: Embedding[M, Boolean])
    : GraphBase[V, E, Boolean] = GraphBase
    .kind[Boolean]
    .wrap(embed.embedded(structure))

  /**
    * @return
    *   This graph as a multigraph, with one copy of each edge. An edge set
    *   doesn't count copies, so it only mixes with multigraphs through this, as
    *   in `m & g.counted`, or `m - g.counted` to remove one copy of each edge
    *   of `g`.
    */
  override def counted(using edges: M =:= Boolean): GraphBase[V, E, Count] =
    GraphBase
      .kind[Count]
      .wrap(Support.counted(
        edges.substituteCo[[m] =>> IncidenceStructure[
          V,
          m,
          kneiphof.Edge[V, E],
        ]](structure),
      ))

  override protected def kindName: String = "Graph"

  override def empty: GraphBase[Nothing, Nothing, M] = graphs.empty

  override def addVertex[W >: V](vertex: W): GraphBase[W, E, M] = graphs
    .withVertex[W, E](this, vertex)

  override def removeVertex(vertex: Any): GraphBase[V, E, M] = graphs
    .withoutVertex(this, vertex)

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : GraphBase[V, E, M] = graphs.withMultiplicity(this, edge, multiplicity)

  override def materialise: GraphBase[V, E, M] = graphs.materialise(this)

  override protected def subgraph
    (
      keep: V => Boolean,
      keepEdge: Option[kneiphof.Edge[V, E] => Boolean],
    )
    : GraphBase[V, E, M] = graphs.subgraph(this, keep, keepEdge)

  override protected def intersected[N]
    (
      that: AnyGraph[Any, Any, N],
      embed: Embedding[N, M],
    )
    : GraphBase[V, E, M] =
    graphs.intersection(this, embed.embedded(that.structure))

  override protected def subtracted[N]
    (
      that: AnyGraph[Any, Any, N],
      embed: Embedding[N, M],
    )
    : GraphBase[V, E, M] =
    graphs.difference(this, embed.embedded(that.structure))

  /** The kind of this graph, in its own representation. */
  private def graphs: GraphBase.Kind[M] = GraphBase.kind[M]

/** The companion object for [[GraphBase]]. */
object GraphBase:

  /**
    * The join of two graphs, which remembers its new edges so that they can be
    * relabelled with `labelled`.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M]
    extends GraphBase[V, E | L, M], HypergraphBase.Join[V, E, L, M]:

    override def labelled[F : NotGraph](label: F): Join[V, E, F, M]

    override infix def % [F : NotGraph](label: F): Join[V, E, F, M] =
      labelled(label)

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
      * A join, held as the graphs it joins. Its new edges are computed from
      * them only when they're needed.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: GraphBase[V, E, M],
        right: GraphBase[V, E, M],
        label: L,
      )
      extends Join[V, E, L, M]:

      override private[kneiphof] lazy val structure
        : IncidenceStructure[V, M, kneiphof.Edge[V, E | L]] =
        incidence.Join[V, E, L, M, kneiphof.Edge](
          left.structure,
          right.structure,
          kneiphof.Edge(_, _, label),
          incidence
            .Join
            .undirectedLinks(
              left.structure,
              right.structure,
              label,
              _,
            ),
        )

      override def labelled[F : NotGraph](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

  /** A graph with the given incidence [[structure]]. */
  private final class Wrapper[V, E, M : Multiplicity]
    (
      override private[kneiphof] val structure: IncidenceStructure[
        V,
        M,
        kneiphof.Edge[V, E],
      ],
    )
    extends GraphBase[V, E, M]

  /** The kind of graphs. */
  class Kind[M : Multiplicity]
    extends GraphKind[
      M,
      Edge,
      [v, e] =>> GraphBase[v, e, M],
      [v, e] =>> GraphBase[v, e, Count],
    ],
            Shapes[[v, e] =>> GraphBase[v, e, M]]:

    override private[kneiphof] def wrap[V, E]
      (structure: IncidenceStructure[V, M, Edge[V, E]])
      : GraphBase[V, E, M] = Wrapper(structure)

    override private[kneiphof] def shaped[V]
      (index: Index[V], rule: Rule)
      : GraphBase[V, Unit, M] = wrap(Shape.undirected[V, M](index, rule))

    override private[kneiphof] def coned[V]
      (hub: V, index: Index[V], rule: Rule)
      : GraphBase[V, Unit, M] = shaped(index, rule).cone(hub)

    override private[kneiphof] def product[V, W]
      (
        left: Index[V],
        first: Rule,
        right: Index[W],
        second: Rule,
      )
      : GraphBase[(V, W), Unit, M] =
      shaped(left, first).cartesianProduct(shaped(right, second))

    override private[kneiphof] def directed: Boolean = false

  /** @return The kind of graphs with multiplicities in [[M]]. */
  def kind[M : Multiplicity]: Kind[M] = Kind[M]()

/** Graphs whose edges form a set. */
object Graph extends GraphBase.Kind[Boolean]

/** Graphs with edge counts. */
object Multigraph extends GraphBase.Kind[Count]
