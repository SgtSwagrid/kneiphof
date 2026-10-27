package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{IncidenceStructure, Support}

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
  def withEdge[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] =
    hypergraphs.withEdge[W, F](this, edge, multiplicity)

  /**
    * @return
    *   The sum of this hypergraph and [[that]], which adds the multiplicities
    *   of their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `Hyperedge(1, 2, 3)`.
    */
  def sum[W >: V, F >: E, N]
    (that: HypergraphBase[W, F, N])
    (using embedding: Embedding[N, M])
    : HypergraphBase[W, F, M] =
    hypergraphs.sum[W, F](this, embedding.embed(that.structure))

  /** Alias of [[sum]]. */
  infix def + [W >: V, F >: E, N]
    (that: HypergraphBase[W, F, N])
    (using Embedding[N, M])
    : HypergraphBase[W, F, M] = sum(that)

  /**
    * @return
    *   The union of this hypergraph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for [[sum]].
    */
  def union[W >: V, F >: E, N]
    (that: HypergraphBase[W, F, N])
    (using embedding: Embedding[N, M])
    : HypergraphBase[W, F, M] =
    hypergraphs.union[W, F](this, embedding.embed(that.structure))

  /** Alias of [[union]]. */
  infix def | [W >: V, F >: E, N]
    (that: HypergraphBase[W, F, N])
    (using Embedding[N, M])
    : HypergraphBase[W, F, M] = union(that)

  /**
    * @return
    *   The difference of this hypergraph and [[that]], which keeps the vertices
    *   of this hypergraph and subtracts the multiplicity of each edge in
    *   [[that]]. So `Graph.complete(n) - g` is the complement of a
    *   `Graph[Int, Unit]` `g` on the vertices `0 until n`. To remove the
    *   vertices of [[that]] as well, with their edges, use
    *   `removeVertices(that.vertexValues)`. [[that]] may be any hypergraph or
    *   graph whose multiplicities embed in [[M]], so it may also be an edge or
    *   chain, as in `g - (1 ~ 2)`.
    */
  def difference[N]
    (that: HypergraphBase[Any, Any, N])
    (using embedding: Embedding[N, M])
    : HypergraphBase[V, E, M] =
    hypergraphs.difference(this, embedding.embed(that.structure))

  /** Alias of [[difference]]. */
  infix def - [N]
    (that: HypergraphBase[Any, Any, N])
    (using Embedding[N, M])
    : HypergraphBase[V, E, M] = difference(that)

  /**
    * @return
    *   The intersection of this hypergraph and [[that]], which keeps their
    *   common vertices and the meet of each edge's multiplicities, so that it's
    *   the largest subgraph of both. For the subgraph of this hypergraph with
    *   every edge between the vertices of [[that]], use
    *   `induced(that.vertexValues)`. [[that]] may be any hypergraph or graph
    *   whose multiplicities embed in [[M]], as for [[difference]].
    */
  def intersection[N]
    (that: HypergraphBase[Any, Any, N])
    (using embedding: Embedding[N, M])
    : HypergraphBase[V, E, M] =
    hypergraphs.intersection(this, embedding.embed(that.structure))

  /** Alias of [[intersection]]. */
  infix def & [N]
    (that: HypergraphBase[Any, Any, N])
    (using Embedding[N, M])
    : HypergraphBase[V, E, M] = intersection(that)

  /**
    * @return
    *   The disjoint union of this hypergraph and [[that]], whose vertices are
    *   tagged by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[Either[V, W], F, M] = hypergraphs.disjointUnion(this, that)

  /** Alias of [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The hypergraph in which each vertex `v` is replaced by the hypergraph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge. Each end is
    *   replaced independently, even where a vertex repeats, so a loop at `v`
    *   becomes a copy for every ordered pair of vertices of `transform(v)`: a
    *   loop at each, and two copies of the edge between every two (one, for
    *   edge sets).
    */
  def flatMapVertices[W, F >: E]
    (transform: V => HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = hypergraphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The hypergraph obtained by replacing each vertex with the hypergraph it
    *   holds (see [[flatMapVertices]], including how loops are replaced).
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
    *   The Cartesian product `G □ H` of this hypergraph and [[that]], on the
    *   pairs `(v, w)` of their vertices, with a copy of each edge of this
    *   hypergraph for each vertex `w` of [[that]], and a copy of each edge of
    *   [[that]] for each vertex `v` of this hypergraph. Copies keep their
    *   labels. The copies of two loops which differ only in their end coincide,
    *   which is one edge, for edge sets. So `path(m) □ path(n)` is a grid.
    */
  def cartesianProduct[W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[(V, W), F, M] =
    hypergraphs.cartesianProduct[V, W, F](this, that)

  /** Alias of [[cartesianProduct]]. */
  infix def □ [W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[(V, W), F, M] = cartesianProduct(that)

  /**
    * @return
    *   The join of this hypergraph and [[that]], with an extra edge labelled
    *   `()` between every vertex of this hypergraph and every vertex of
    *   [[that]], which [[HypergraphBase.Join.withLabel]] can relabel. This is
    *   the flattening of the edge `this ~ that`, so it pairs each vertex of
    *   this hypergraph with each vertex of [[that]] even where the two share
    *   vertices: a vertex in both gains a loop, and two vertices in both are
    *   joined once for each way of pairing them (which is just once, for edge
    *   sets).
    */
  def join[W >: V, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase.Join[W, F, Unit, M] = HypergraphBase.Join(this, that)

  /** Alias of [[join]]. */
  infix def :~: [W >: V, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase.Join[W, F, Unit, M] = that.join(this)

  /**
    * @return
    *   The cone over this hypergraph with apex [[vertex]], which joins every
    *   vertex of this hypergraph to [[vertex]] by an edge labelled `()` (see
    *   [[join]]).
    */
  def cone[W >: V, A <: W : NotHandle]
    (vertex: A)
    : HypergraphBase.Join[W, E, Unit, M] =
    HypergraphBase.Join(this, hypergraphs.vertex(vertex))

  /** Alias of [[cone]]. */
  infix def :~ [W >: V, A <: W : NotHandle]
    (vertex: A)
    : HypergraphBase.Join[W, E, Unit, M] = cone(vertex)

  /** Alias of [[cone]]. */
  infix def ~: [W >: V, A <: W : NotHandle]
    (vertex: A)
    : HypergraphBase.Join[W, E, Unit, M] = cone(vertex)

  override def map[W : NotHandle, F]
    (vertexMap: V => W, labelMap: E => F)
    : HypergraphBase[W, F, M] = hypergraphs.image(structure)(
    vertexMap,
    edge => List(edge.map(vertexMap, labelMap)),
    kept = algebra.additive,
  )

  override def vertex[A : NotHandle](value: A): Option[Vertex] = structure
    .vertexValue(value)
    .map(found =>
      new VertexOps:
        override val value: V = found,
    )

  override def edge[A : NotHandle](value: A): Option[Edge] =
    found(value).map(handle)

  override private[kneiphof] def edgesAt(vertex: Any): Iterable[Edge] =
    incidentEdgeValues(vertex).map(handle)

  override private[kneiphof] def allEdges: Iterable[Edge] = edgeValues
    .keys
    .map(handle)

  /** @return The handle on [[found]], an edge of this hypergraph. */
  private def handle(found: Hyperedge[V, E]): Edge = new EdgeOps:
    override val value: Hyperedge[V, E] = found

  /**
    * @return
    *   The edge set of this hypergraph, in which each edge with any copies is
    *   present once.
    */
  def support
    (using embedding: Embedding[M, Boolean])
    : HypergraphBase[V, E, Boolean] = HypergraphBase
    .kind[Boolean]
    .wrap(embedding.embed(structure))

  /**
    * @return
    *   This hypergraph as a multihypergraph, with one copy of each edge. An
    *   edge set doesn't count copies, so it only mixes with multihypergraphs
    *   through this, as in `m & g.counted`, or `m - g.counted` to remove one
    *   copy of each edge of `g`.
    */
  def counted(using M =:= Boolean): HypergraphBase[V, E, Count] = HypergraphBase
    .kind[Count]
    .wrap(Support.counted(structure))

  override private[kneiphof] def directed: Boolean = false

  override protected def kindName: String = "Hypergraph"

  override def empty: HypergraphBase[Nothing, Nothing, M] = hypergraphs.empty

  override def addVertex[W >: V, A <: W : NotHandle]
    (vertex: A)
    : HypergraphBase[W, E, M] = hypergraphs.withVertex[W, E](this, vertex)

  override def removeVertex[A : NotHandle](vertex: A): HypergraphBase[V, E, M] =
    hypergraphs.withoutVertex(this, vertex)

  override def withMultiplicity[A : NotHandle]
    (edge: A, multiplicity: M)
    : HypergraphBase[V, E, M] =
    hypergraphs.withMultiplicity(this, edge, multiplicity)

  override def materialise: HypergraphBase[V, E, M] =
    hypergraphs.materialise(this)

  override protected def subgraph
    (
      keep: V => Boolean,
      keepEdge: Option[Hyperedge[V, E] => Boolean],
    )
    : HypergraphBase[V, E, M] = hypergraphs.subgraph(this, keep, keepEdge)

  override protected def edgeSubgraph
    (keep: Hyperedge[V, E] => Boolean)
    : HypergraphBase[V, E, M] = hypergraphs.edgeInduced(this, keep)

  /** The kind of this hypergraph, in its own representation. */
  private def hypergraphs: HypergraphBase.Kind[M] = HypergraphBase.kind[M]

/** The companion object for [[HypergraphBase]]. */
object HypergraphBase:

  /**
    * The join of two hypergraphs, which remembers its new edges so that they
    * can be relabelled with [[withLabel]]. Equality compares only the graphs,
    * so equal joins can relabel different edges:
    * `Graph(1 ~ 2) :~: Graph.vertex(3)` and `Graph.vertex(1) :~: Graph(2 ~ 3)`
    * are the same triangle, but [[withLabel]] relabels the edges at `3` in the
    * first, and at `1` in the second.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M] extends HypergraphBase[V, E | L, M]:

    /** @return This join, with every new edge labelled [[label]] instead. */
    def withLabel[F : NotGraph](label: F): Join[V, E, F, M]

    /** Alias of [[withLabel]]. */
    infix def % [F : NotGraph](label: F): Join[V, E, F, M] = withLabel(label)

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
      * A join, held as the hypergraphs it joins. Its new edges are computed
      * from them only when they're needed.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: HypergraphBase[V, E, M],
        right: HypergraphBase[V, E, M],
        label: L,
      )
      extends Join[V, E, L, M]:

      override private[kneiphof] lazy val structure
        : IncidenceStructure[V, M, Hyperedge[V, E | L]] =
        kind[M].join(left.structure, right.structure, label)

      override def withLabel[F : NotGraph](label: F): Join[V, E, F, M] =
        Joined(left, right, label)

  /** A hypergraph with the given incidence [[structure]]. */
  private final class Wrapper[V, E, M : Multiplicity]
    (
      override private[kneiphof] val structure: IncidenceStructure[
        V,
        M,
        Hyperedge[V, E],
      ],
    )
    extends HypergraphBase[V, E, M]

  /** The kind of hypergraphs. */
  class Kind[M : Multiplicity]
    extends GraphKind[
      M,
      Hyperedge,
      [v, e] =>> HypergraphBase[v, e, M],
      [v, e] =>> HypergraphBase[v, e, Count],
    ]:

    override private[kneiphof] def wrap[V, E]
      (structure: IncidenceStructure[V, M, Hyperedge[V, E]])
      : HypergraphBase[V, E, M] = Wrapper(structure)

    override private[kneiphof] def link[V, E]
      (source: V, target: V, label: E)
      : Hyperedge[V, E] = kneiphof.Edge(source, target, label)

    override private[kneiphof] def directed: Boolean = false

  /** @return The kind of hypergraphs with multiplicities in [[M]]. */
  def kind[M : Multiplicity]: Kind[M] = Kind[M]()

/** Hypergraphs whose edges form a set. */
object Hypergraph extends HypergraphBase.Kind[Boolean]

/** Hypergraphs with edge counts. */
object Multihypergraph extends HypergraphBase.Kind[Count]
