package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{IncidenceStructure, Support}

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
  def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] =
    hypergraphs.updated[W, F](this, edge, multiplicity)

  /** The hypergraph with every edge reversed. */
  def transpose: DirectedHypergraphBase[V, E, M] =
    hypergraphs.imaged(structure)(
      identity,
      edge => List(edge.reverse),
      kept = true,
    )

  /**
    * @return
    *   The sum of this hypergraph and [[that]], which adds the multiplicities
    *   of their edges. For edge sets, this is the same as their union. [[that]]
    *   may have any multiplicities which embed in [[M]], so it may also be an
    *   edge or chain, such as `DirectedHyperedge(Seq(1, 2), Seq(3))`.
    */
  def sum[W >: V, F >: E, N]
    (that: DirectedHypergraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedHypergraphBase[W, F, M] =
    hypergraphs.sum[W, F](this, embed.embedded(that.structure))

  /** Alias of [[sum]]. */
  infix def + [W >: V, F >: E, N]
    (that: DirectedHypergraphBase[W, F, N])
    (using Embedding[N, M])
    : DirectedHypergraphBase[W, F, M] = sum(that)

  /**
    * @return
    *   The union of this hypergraph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide. [[that]] may have
    *   any multiplicities which embed in [[M]], as for [[sum]].
    */
  def union[W >: V, F >: E, N]
    (that: DirectedHypergraphBase[W, F, N])
    (using embed: Embedding[N, M])
    : DirectedHypergraphBase[W, F, M] =
    hypergraphs.union[W, F](this, embed.embedded(that.structure))

  /** Alias of [[union]]. */
  infix def | [W >: V, F >: E, N]
    (that: DirectedHypergraphBase[W, F, N])
    (using Embedding[N, M])
    : DirectedHypergraphBase[W, F, M] = union(that)

  /**
    * @return
    *   The disjoint union of this hypergraph and [[that]], whose vertices are
    *   tagged by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[Either[V, W], F, M] =
    hypergraphs.disjointUnion(this, that)

  /** Alias of [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The hypergraph in which each vertex `v` is replaced by the hypergraph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge. Each end is
    *   replaced independently, so a loop at `v` becomes a copy for every
    *   ordered pair of vertices of `transform(v)`: a loop at each, and an edge
    *   each way between every two.
    */
  def flatMapVertices[W, F >: E]
    (transform: V => DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[W, F, M] =
    hypergraphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The hypergraph obtained by replacing each vertex with the hypergraph it
    *   holds (see [[flatMapVertices]], including how loops are replaced).
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
    *   The Cartesian product `G □ H` of this hypergraph and [[that]], on the
    *   pairs `(v, w)` of their vertices, with a copy of each edge of this
    *   hypergraph for each vertex `w` of [[that]], and a copy of each edge of
    *   [[that]] for each vertex `v` of this hypergraph. Copies keep their
    *   labels. The copies of two loops which differ only in their end coincide,
    *   which is one edge, for edge sets. So `path(m) □ path(n)` is a grid.
    */
  def cartesianProduct[W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[(V, W), F, M] =
    hypergraphs.cartesianProduct[V, W, F](this, that)

  /** Alias of [[cartesianProduct]]. */
  infix def □ [W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[(V, W), F, M] = cartesianProduct(that)

  /**
    * @return
    *   The join of this hypergraph and [[that]], with an extra edge labelled
    *   `()` from every vertex of this hypergraph to every vertex of [[that]],
    *   which `labelled` can relabel. This is the flattening of the edge
    *   `this ~> that`, so it pairs each vertex of this hypergraph with each
    *   vertex of [[that]] even where the two share vertices: a vertex in both
    *   gains a loop, and two vertices in both gain an edge each way.
    */
  def join[W >: V, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase.Join[W, F, Unit, M] =
    DirectedHypergraphBase.Join(this, that)

  /** Alias of [[join]]. */
  infix def :~>: [W >: V, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase.Join[W, F, Unit, M] = that.join(this)

  /**
    * @return
    *   The cone over this hypergraph with apex [[vertex]], with an edge
    *   labelled `()` from every vertex of this hypergraph to [[vertex]] (see
    *   [[join]]).
    */
  def coneTo[W >: V](vertex: W): DirectedHypergraphBase.Join[W, E, Unit, M] =
    DirectedHypergraphBase.Join(this, hypergraphs.vertex(vertex))

  /** Alias of [[coneTo]]. */
  infix def :~> [W >: V]
    (vertex: W)
    : DirectedHypergraphBase.Join[W, E, Unit, M] = coneTo(vertex)

  /**
    * @return
    *   The cone over this hypergraph with apex [[vertex]], with an edge
    *   labelled `()` from [[vertex]] to every vertex of this hypergraph (see
    *   [[join]]).
    */
  def coneFrom[W >: V](vertex: W): DirectedHypergraphBase.Join[W, E, Unit, M] =
    DirectedHypergraphBase.Join(hypergraphs.vertex(vertex), this)

  /** Alias of [[coneFrom]]. */
  infix def ~>: [W >: V]
    (vertex: W)
    : DirectedHypergraphBase.Join[W, E, Unit, M] = coneFrom(vertex)

  /**
    * The undirected hypergraph obtained by forgetting the direction of every
    * edge. A vertex in both the tail and head of an edge occurs twice in its
    * image.
    */
  def underlying: HypergraphBase[V, E, M] = HypergraphBase
    .kind[M]
    .imaged(structure)(
      identity,
      edge => List(Hyperedge.from(edge.tail ++ edge.head, edge.label)),
      kept = algebra.additive,
    )

  override def map[W, F]
    (vertexMap: V => W, labelMap: E => F)
    : DirectedHypergraphBase[W, F, M] = hypergraphs.imaged(structure)(
    vertexMap,
    edge => List(edge.map(vertexMap, labelMap)),
    kept = algebra.additive,
  )

  override def vertex(value: Any): Option[Vertex] = vertexValues
    .get(value)
    .map(found =>
      new DirectedVertexOps:
        override val value: V = found,
    )

  override def edge(value: Any): Option[Edge] = found(value).map: found =>
    new DirectedEdgeOps:
      override val value: Hyperarc[V, E] = found

  /**
    * @return
    *   The edge set of this hypergraph, in which each edge with any copies is
    *   present once.
    */
  def support
    (using embed: Embedding[M, Boolean])
    : DirectedHypergraphBase[V, E, Boolean] = DirectedHypergraphBase
    .kind[Boolean]
    .wrap(embed.embedded(structure))

  /**
    * @return
    *   This hypergraph as a multihypergraph, with one copy of each edge. An
    *   edge set doesn't count copies, so it only mixes with multihypergraphs
    *   through this, as in `m & g.counted`, or `m - g.counted` to remove one
    *   copy of each edge of `g`.
    */
  def counted(using edges: M =:= Boolean): DirectedHypergraphBase[V, E, Count] =
    DirectedHypergraphBase
      .kind[Count]
      .wrap(Support.counted(
        edges.substituteCo[[m] =>> IncidenceStructure[V, m, Hyperarc[V, E]]](
          structure,
        ),
      ))

  override protected def kindName: String = "DirectedHypergraph"

  override def empty: DirectedHypergraphBase[Nothing, Nothing, M] =
    hypergraphs.empty

  override def addVertex[W >: V](vertex: W): DirectedHypergraphBase[W, E, M] =
    hypergraphs.withVertex[W, E](this, vertex)

  override def removeVertex(vertex: Any): DirectedHypergraphBase[V, E, M] =
    hypergraphs.withoutVertex(this, vertex)

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : DirectedHypergraphBase[V, E, M] =
    hypergraphs.withMultiplicity(this, edge, multiplicity)

  override def materialise: DirectedHypergraphBase[V, E, M] = hypergraphs
    .materialise(this)

  override protected def subgraph
    (
      keep: V => Boolean,
      keepEdge: Option[Hyperarc[V, E] => Boolean],
    )
    : DirectedHypergraphBase[V, E, M] =
    hypergraphs.subgraph(this, keep, keepEdge)

  override protected def intersected[N]
    (
      that: AnyGraph[Any, Any, N],
      embed: Embedding[N, M],
    )
    : DirectedHypergraphBase[V, E, M] =
    hypergraphs.intersection(this, embed.embedded(that.structure))

  override protected def subtracted[N]
    (
      that: AnyGraph[Any, Any, N],
      embed: Embedding[N, M],
    )
    : DirectedHypergraphBase[V, E, M] =
    hypergraphs.difference(this, embed.embedded(that.structure))

  /** The kind of this hypergraph, in its own representation. */
  private def hypergraphs: DirectedHypergraphBase.Kind[M] =
    DirectedHypergraphBase.kind[M]

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

    /**
      * The number of edges leaving this vertex, counting every copy of each
      * edge, and each time this vertex occurs in its tail.
      */
    def outDegree: Int = outgoing
      .toSeq
      .map(edge => edge.tail.count(_ == value) * multiplicity(edge).count)
      .sum

    /**
      * The number of edges entering this vertex, counting every copy of each
      * edge, and each time this vertex occurs in its head.
      */
    def inDegree: Int = incoming
      .toSeq
      .map(edge => edge.head.count(_ == value) * multiplicity(edge).count)
      .sum

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
    * that they can be relabelled with `labelled`.
    *
    * @tparam L
    *   The label of the new edges.
    */
  trait Join[+V, +E, +L, M] extends DirectedHypergraphBase[V, E | L, M]:

    /** @return This join, with every new edge labelled [[label]] instead. */
    def labelled[F : NotGraph](label: F): Join[V, E, F, M]

    /** Alias of [[labelled]]. */
    infix def % [F : NotGraph](label: F): Join[V, E, F, M] = labelled(label)

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
      * A join, held as the hypergraphs it joins. Its new edges are computed
      * from them only when they're needed.
      */
    private final class Joined[V, E, L, M : Multiplicity]
      (
        left: DirectedHypergraphBase[V, E, M],
        right: DirectedHypergraphBase[V, E, M],
        label: L,
      )
      extends Join[V, E, L, M]:

      override private[kneiphof] lazy val structure
        : IncidenceStructure[V, M, Hyperarc[V, E | L]] =
        incidence.Join[V, E, L, M, Hyperarc](
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

  /** A directed hypergraph with the given incidence [[structure]]. */
  private final class Wrapper[V, E, M : Multiplicity]
    (
      override private[kneiphof] val structure: IncidenceStructure[
        V,
        M,
        Hyperarc[V, E],
      ],
    )
    extends DirectedHypergraphBase[V, E, M]

  /** The kind of directed hypergraphs. */
  class Kind[M : Multiplicity]
    extends GraphKind[
      M,
      Hyperarc,
      [v, e] =>> DirectedHypergraphBase[v, e, M],
      [v, e] =>> DirectedHypergraphBase[v, e, Count],
    ]:

    override private[kneiphof] def wrap[V, E]
      (structure: IncidenceStructure[V, M, Hyperarc[V, E]])
      : DirectedHypergraphBase[V, E, M] = Wrapper(structure)

  /** @return The kind of directed hypergraphs with multiplicities in [[M]]. */
  def kind[M : Multiplicity]: Kind[M] = Kind[M]()

/** Directed hypergraphs whose edges form a set. */
object DirectedHypergraph extends DirectedHypergraphBase.Kind[Boolean]

/** Directed hypergraphs with edge counts. */
object DirectedMultihypergraph extends DirectedHypergraphBase.Kind[Count]
