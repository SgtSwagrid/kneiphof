package com.alecdorrington
package kneiphof
package views

/**
  * The operations of a lazy graph, which answers queries from [[base]] after
  * the changes in [[patch]] without being built, and everything else from the
  * graph [[origin]] builds with [[patch]] applied, which is only built when
  * first needed.
  *
  * Adding or removing single vertices and edges extends the patch rather than
  * nesting another view, so long chains of updates stay shallow, and building
  * the result applies every change at once.
  */
private[kneiphof] trait LazyOps[
  V,
  E,
  M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
] extends Delegating[V, E, M, X, G]:
  this: G[V, E] =>

  /** The queries about this graph before [[patch]]. */
  protected val base: Queries[V, E, M, X[V, E]]

  /** Builds this graph before [[patch]]. */
  protected val origin: () => G[V, E]

  /** The changes made since. */
  protected val patch: Patch[V, X[V, E], M]

  /**
    * @return
    *   The lazy graph of this kind with the given [[base]], [[origin]] and
    *   [[patch]].
    */
  protected def patched[W, F]
    (
      base: Queries[W, F, M, X[W, F]],
      origin: () => G[W, F],
      patch: Patch[W, X[W, F], M],
    )
    : G[W, F]

  /**
    * @return
    *   [[graph]] with the multiplicity of [[edge]] set to [[multiplicity]].
    */
  protected def set
    (
      graph: G[V, E],
      edge: X[V, E],
      multiplicity: M,
    )
    : G[V, E]

  override private[kneiphof] def eager: Boolean = false

  override private[kneiphof] def built: G[V, E] = delegate

  override def view: G[V, E] = this

  override def vertexValues: ValueSet[V] = queries.vertexValues

  override def hasVertex(vertex: Any): Boolean = queries.hasVertex(vertex)

  override def size: Int = queries.size.getOrElse(delegate.size)

  override def degree(vertex: Any): Int = queries
    .degree(vertex)
    .getOrElse(Queries.degree(vertex, queries.incidentEdges(vertex)))

  override def multiplicity(edge: Any): M = queries.multiplicity(edge)

  override def hasEdge(edge: Any): Boolean = multiplicity(edge).nonZero

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E]] = queries
    .incidentEdgeValues(vertex)

  override def addVertex[W >: V](vertex: W): G[W, E] =
    patched(base, origin, patch.withVertex(vertex))

  override def removeVertex(vertex: Any): G[V, E] =
    if !hasVertex(vertex) then this
    else
      patched(
        base,
        origin,
        patch.withoutVertex(vertex),
      )

  override def withMultiplicity(edge: Any, multiplicity: M): G[V, E] = queries
    .find(edge)
    .fold(this: G[V, E]): found =>
      patched(
        base,
        origin,
        patch.withEdge(found, Nil, multiplicity),
      )

  override protected def found(edge: Any): Option[X[V, E]] = queries.find(edge)

  override protected lazy val delegate: G[V, E] = patch
    .overrides
    .foldLeft(origin().removeVertices(patch.detached).addVertices(patch.added)):
      (graph, entry) => set(graph, entry(0), entry(1))

  /**
    * @return
    *   This graph with the multiplicity of [[edge]] set to [[multiplicity]],
    *   adding its vertices unless the edge is now absent.
    */
  protected def updatedEdge[W >: V, F >: E]
    (edge: X[W, F], multiplicity: M)
    : G[W, F] = patched(
    base,
    origin,
    patch.withEdge(
      edge,
      if multiplicity.isZero then Nil else edge.vertexValues,
      multiplicity,
    ),
  )

  /** The queries about this graph, after [[patch]]. */
  private lazy val queries: Queries[V, E, M, X[V, E]] =
    if patch.isEmpty then base else Patching(base, patch)

/** A lazy hypergraph (see [[LazyOps]]). */
private[kneiphof] class LazyHypergraph[V, E, M : Multiplicity]
  (
    override val empty: HypergraphBase[Nothing, Nothing, M],
    override protected val base: Queries[V, E, M, Hyperedge[V, E]],
    override protected val origin: () => HypergraphBase[V, E, M],
    override protected val patch: Patch[V, Hyperedge[V, E], M] = Patch.empty[M],
  )
  extends HypergraphBase[V, E, M],
          LazyOps[
            V,
            E,
            M,
            Hyperedge,
            [v, e] =>> HypergraphBase[v, e, M],
          ]:

  override def updated[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] = updatedEdge(edge, multiplicity)

  override protected def patched[W, F]
    (
      base: Queries[W, F, M, Hyperedge[W, F]],
      origin: () => HypergraphBase[W, F, M],
      patch: Patch[W, Hyperedge[W, F], M],
    )
    : HypergraphBase[W, F, M] = LazyHypergraph(empty, base, origin, patch)

  override protected def set
    (
      graph: HypergraphBase[V, E, M],
      edge: Hyperedge[V, E],
      multiplicity: M,
    )
    : HypergraphBase[V, E, M] = graph.updated(edge, multiplicity)

/** A lazy graph (see [[LazyOps]]). */
private[kneiphof] class LazyGraph[V, E, M : Multiplicity]
  (
    override val empty: GraphBase[Nothing, Nothing, M],
    override protected val base: Queries[V, E, M, kneiphof.Edge[V, E]],
    override protected val origin: () => GraphBase[V, E, M],
    override protected val patch: Patch[V, kneiphof.Edge[V, E], M] = Patch
      .empty[M],
  )
  extends GraphBase[V, E, M],
          LazyOps[V, E, M, Edge, [v, e] =>> GraphBase[v, e, M]]:

  override def updated[W >: V, F >: E]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: M,
    )
    : GraphBase[W, F, M] = updatedEdge(edge, multiplicity)

  override protected def patched[W, F]
    (
      base: Queries[W, F, M, kneiphof.Edge[W, F]],
      origin: () => GraphBase[W, F, M],
      patch: Patch[W, kneiphof.Edge[W, F], M],
    )
    : GraphBase[W, F, M] = LazyGraph(empty, base, origin, patch)

  override protected def set
    (
      graph: GraphBase[V, E, M],
      edge: kneiphof.Edge[V, E],
      multiplicity: M,
    )
    : GraphBase[V, E, M] = graph.updated(edge, multiplicity)

/** A lazy directed hypergraph (see [[LazyOps]]). */
private[kneiphof] class LazyDirectedHypergraph[V, E, M : Multiplicity]
  (
    override val empty: DirectedHypergraphBase[Nothing, Nothing, M],
    override protected val base: Queries[V, E, M, Hyperarc[V, E]],
    override protected val origin: () => DirectedHypergraphBase[V, E, M],
    override protected val patch: Patch[V, Hyperarc[V, E], M] = Patch.empty[M],
  )
  extends DirectedHypergraphBase[V, E, M],
          LazyOps[
            V,
            E,
            M,
            Hyperarc,
            [v, e] =>> DirectedHypergraphBase[v, e, M],
          ]:

  override def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] = updatedEdge(edge, multiplicity)

  override protected def patched[W, F]
    (
      base: Queries[W, F, M, Hyperarc[W, F]],
      origin: () => DirectedHypergraphBase[W, F, M],
      patch: Patch[W, Hyperarc[W, F], M],
    )
    : DirectedHypergraphBase[W, F, M] =
    LazyDirectedHypergraph(empty, base, origin, patch)

  override protected def set
    (
      graph: DirectedHypergraphBase[V, E, M],
      edge: Hyperarc[V, E],
      multiplicity: M,
    )
    : DirectedHypergraphBase[V, E, M] = graph.updated(edge, multiplicity)

/** A lazy directed graph (see [[LazyOps]]). */
private[kneiphof] class LazyDirectedGraph[V, E, M : Multiplicity]
  (
    override val empty: DirectedGraphBase[Nothing, Nothing, M],
    override protected val base: Queries[V, E, M, Arc[V, E]],
    override protected val origin: () => DirectedGraphBase[V, E, M],
    override protected val patch: Patch[V, Arc[V, E], M] = Patch.empty[M],
  )
  extends DirectedGraphBase[V, E, M],
          LazyOps[
            V,
            E,
            M,
            Arc,
            [v, e] =>> DirectedGraphBase[v, e, M],
          ]:

  override def updated[W >: V, F >: E]
    (edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M] = updatedEdge(edge, multiplicity)

  override protected def patched[W, F]
    (
      base: Queries[W, F, M, Arc[W, F]],
      origin: () => DirectedGraphBase[W, F, M],
      patch: Patch[W, Arc[W, F], M],
    )
    : DirectedGraphBase[W, F, M] = LazyDirectedGraph(empty, base, origin, patch)

  override protected def set
    (
      graph: DirectedGraphBase[V, E, M],
      edge: Arc[V, E],
      multiplicity: M,
    )
    : DirectedGraphBase[V, E, M] = graph.updated(edge, multiplicity)
