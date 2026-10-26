package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceHypergraph

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
  def updated[W >: V, F >: E](edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M]

  /** @return This hypergraph with one more copy of [[edge]]. */
  infix def + [W >: V, F >: E](edge: Hyperedge[W, F]): HypergraphBase[W, F, M] =
    updated(edge, multiplicity(edge) + algebra.one)

  /**
    * @return
    *   The sum of this hypergraph and [[that]], which adds the multiplicities
    *   of their edges. For edge sets, this is the same as their union.
    */
  infix def + [W >: V, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = hypergraphs.merged(this, that)(_ + _)

  /**
    * @return
    *   The union of this hypergraph and [[that]], which takes the join of their
    *   multiplicities. Vertices with equal values coincide.
    */
  infix def | [W >: V, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = hypergraphs.merged(this, that)(_ | _)

  /**
    * @return
    *   The disjoint union of this hypergraph and [[that]], whose vertices are
    *   tagged by which side they came from.
    */
  def disjointUnion[W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[Either[V, W], F, M] = mapVertices[Either[V, W]](Left(_)) |
    that.mapVertices[Either[V, W]](Right(_))

  /** An alias for [[disjointUnion]]. */
  infix def ⊔ [W, F >: E]
    (that: HypergraphBase[W, F, M])
    : HypergraphBase[Either[V, W], F, M] = disjointUnion(that)

  /**
    * @return
    *   The hypergraph in which each vertex `v` is replaced by the hypergraph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  def flatMapVertices[W, F >: E]
    (transform: V => HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = hypergraphs.flatMapVertices(this, transform)

  /**
    * @return
    *   The hypergraph obtained by replacing each vertex with the hypergraph it
    *   holds (see [[flatMapVertices]]).
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
    flatMapVertices(v => transform(v).mapVertices((v, _)))

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
    *   The join of this hypergraph and [[that]], with an extra edge labelled
    *   [[label]] between every vertex of this hypergraph and every vertex of
    *   [[that]]. Vertices in both also gain a loop.
    */
  def join[W >: V, F >: E]
    (that: HypergraphBase[W, F, M], label: F)
    : HypergraphBase[W, F, M] =
    hypergraphs.joined(this, that)(kneiphof.Edge(_, _, label))

  /**
    * @return
    *   The join `that :~: this`, with an unlabelled edge between every vertex
    *   of [[that]] and every vertex of this hypergraph (see [[join]]).
    */
  infix def :~: [W >: V]
    (that: HypergraphBase[W, Unit, M])
    (using E <:< Unit)
    : HypergraphBase[W, Unit, M] = that.join(unlabelledHypergraph, ())

  /**
    * @return
    *   The cone `this :~ vertex`, with an unlabelled edge from every vertex of
    *   this hypergraph to [[vertex]] (see [[join]]).
    */
  infix def :~ [W >: V]
    (vertex: W)
    (using E <:< Unit)
    : HypergraphBase[W, Unit, M] =
    unlabelledHypergraph.join(hypergraphs.vertex(vertex), ())

  /**
    * @return
    *   The cone `vertex ~: this`, which is the same as `this :~ vertex`.
    */
  infix def ~: [W >: V]
    (vertex: W)
    (using E <:< Unit)
    : HypergraphBase[W, Unit, M] = this :~ vertex

  override def vertex(value: Any): Option[Vertex] = vertexValues
    .get(value)
    .map(found =>
      new VertexOps:
        override val value: V = found,
    )

  override def edge(value: Any): Option[Edge] = edgeValues
    .getKey(value)
    .map(found =>
      new EdgeOps:
        override val value: Hyperedge[V, E] = found,
    )

  override protected def kindName: String = "Hypergraph"

  /** The kind of this hypergraph, in its own representation. */
  private def hypergraphs: HypergraphBase.Kind[M] = HypergraphBase.Kind(empty)

  /** This hypergraph, whose edges are known to be unlabelled. */
  private def unlabelledHypergraph
    (using unit: E <:< Unit)
    : HypergraphBase[V, Unit, M] =
    unit.liftCo[[e] =>> HypergraphBase[V, e, M]](this)

/** The companion object for [[HypergraphBase]]. */
object HypergraphBase:

  /** The kind of hypergraphs, in the representation of [[empty]]. */
  class Kind[M : Multiplicity]
    (override val empty: HypergraphBase[Nothing, Nothing, M])
    extends GraphKind[
      M,
      Hyperedge,
      [v, e] =>> HypergraphBase[v, e, M],
    ]:

    override def updated[V, E]
      (
        graph: HypergraphBase[V, E, M],
        edge: Hyperedge[V, E],
        multiplicity: M,
      )
      : HypergraphBase[V, E, M] = graph.updated(edge, multiplicity)

  /** @return The kind of hypergraphs, in the default representation. */
  def kind[M : Multiplicity]: Kind[M] = Kind(IncidenceHypergraph.empty)

/** Hypergraphs whose edges form a set, in the default representation. */
object Hypergraph
  extends HypergraphBase.Kind[Boolean](IncidenceHypergraph.empty)

/** Hypergraphs with edge counts, in the default representation. */
object Multihypergraph
  extends HypergraphBase.Kind[Count](IncidenceHypergraph.empty)
