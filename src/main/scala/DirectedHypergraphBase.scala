package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceDirectedHypergraph

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
  def transpose: DirectedHypergraphBase[V, E, M]

  /** @return This hypergraph with one more copy of [[edge]]. */
  infix def + [W >: V, F >: E]
    (edge: Hyperarc[W, F])
    : DirectedHypergraphBase[W, F, M] =
    updated(edge, multiplicity(edge) + algebra.one)

  /**
    * @return
    *   The sum of this hypergraph and [[that]], which adds the multiplicities
    *   of their edges. For edge sets, this is the same as their union.
    */
  infix def + [W >: V, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[W, F, M] = hypergraphs.merged(this, that)(_ + _)

  /**
    * @return
    *   The union of this hypergraph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide.
    */
  infix def | [W >: V, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[W, F, M] = hypergraphs.merged(this, that)(_ | _)

  /**
    * @return
    *   The disjoint union of this hypergraph and [[that]], whose vertices are
    *   tagged by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: DirectedHypergraphBase[W, F, M])
    : DirectedHypergraphBase[Either[V, W], F, M] =
    mapVertices[Either[V, W]](Left(_)) |
      that.mapVertices[Either[V, W]](Right(_))

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
    flatMapVertices(v => transform(v).mapVertices((v, _)))

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
    hypergraphs.joined(this, that)(DirectedEdge(_, _, label))

  /**
    * @return
    *   The join `that :~>: this`, with an unlabelled edge from every vertex of
    *   [[that]] to every vertex of this hypergraph (see [[join]]).
    */
  infix def :~>: [W >: V]
    (that: DirectedHypergraphBase[W, Unit, M])
    (using E <:< Unit)
    : DirectedHypergraphBase[W, Unit, M] =
    that.join(unlabelledDirectedHypergraph, ())

  /**
    * @return
    *   The cone `this :~> vertex`, with an unlabelled edge from every vertex of
    *   this hypergraph to [[vertex]] (see [[join]]).
    */
  infix def :~> [W >: V]
    (vertex: W)
    (using E <:< Unit)
    : DirectedHypergraphBase[W, Unit, M] =
    unlabelledDirectedHypergraph.join(hypergraphs.vertex(vertex), ())

  /**
    * @return
    *   The cone `vertex ~>: this`, with an unlabelled edge from [[vertex]] to
    *   every vertex of this hypergraph (see [[join]]).
    */
  infix def ~>: [W >: V]
    (vertex: W)
    (using E <:< Unit)
    : DirectedHypergraphBase[W, Unit, M] = hypergraphs
    .vertex(vertex)
    .join(unlabelledDirectedHypergraph, ())

  /**
    * The undirected hypergraph obtained by forgetting the direction of every
    * edge, in the default representation. A vertex in both the tail and head of
    * an edge occurs twice in its image.
    */
  def underlying: HypergraphBase[V, E, M] = HypergraphBase
    .kind[M]
    .fromMultiplicities(
      vertexValues,
      edgeValues
        .toSeq
        .map: (edge, multiplicity) =>
          Hyperedge.from(edge.tail.toSeq ++ edge.head, edge.label) ->
            multiplicity,
    )

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

  /** The kind of this hypergraph, in its own representation. */
  private def hypergraphs: DirectedHypergraphBase.Kind[M] =
    DirectedHypergraphBase.Kind(empty)

  /** This hypergraph, whose edges are known to be unlabelled. */
  private def unlabelledDirectedHypergraph
    (using unit: E <:< Unit)
    : DirectedHypergraphBase[V, Unit, M] = unit
    .liftCo[[e] =>> DirectedHypergraphBase[V, e, M]](this)

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

  /** The kind of directed hypergraphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: DirectedHypergraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Hyperarc,
      [v, e] =>> DirectedHypergraphBase[v, e, M],
    ]:

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

/** Directed hypergraphs whose edges form a set, in the default representation. */
object DirectedHypergraph
  extends DirectedHypergraphBase.Kind[Boolean](
    IncidenceDirectedHypergraph.empty,
  )

/** Directed hypergraphs with edge counts, in the default representation. */
object DirectedMultihypergraph
  extends DirectedHypergraphBase.Kind[Count](IncidenceDirectedHypergraph.empty)
