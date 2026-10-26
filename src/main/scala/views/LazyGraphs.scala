package com.alecdorrington
package kneiphof
package views

/**
  * A hypergraph answering [[queries]] without being built, and answering
  * everything else with the hypergraph from [[build]], which is only built when
  * first needed. Adding or removing single vertices and edges patches the
  * queries instead of building the hypergraph.
  */
private[kneiphof] class LazyHypergraph[V, E, M : Multiplicity]
  (
    override val empty: HypergraphBase[Nothing, Nothing, M],
    queries: Queries[V, E, M, Hyperedge[V, E]],
    build: () => HypergraphBase[V, E, M],
  )
  extends HypergraphBase[V, E, M],
          Delegating[
            V,
            E,
            M,
            Hyperedge,
            [v, e] =>> HypergraphBase[v, e, M],
          ]:

  override private[kneiphof] def eager: Boolean = false

  override def view: HypergraphBase[V, E, M] = this

  override def vertexValues: ValueSet[V] = queries.vertexValues

  override def hasVertex(vertex: Any): Boolean = queries.hasVertex(vertex)

  override def size: Int = queries.size.getOrElse(delegate.size)

  override def degree(vertex: Any): Int = queries
    .degree(vertex)
    .getOrElse(super.degree(vertex))

  override def multiplicity(edge: Any): M = queries.multiplicity(edge)

  override def hasEdge(edge: Any): Boolean = multiplicity(edge).nonZero

  override def incidentEdgeValues(vertex: Any): ValueSet[Hyperedge[V, E]] =
    queries.incidentEdgeValues(vertex)

  override def addVertex[W >: V](vertex: W): HypergraphBase[W, E, M] =
    patched[W, E](_.withVertex(vertex))(_.addVertex(vertex))

  override def removeVertex(vertex: Any): HypergraphBase[V, E, M] =
    if !hasVertex(vertex) then this
    else patched(_.withoutVertex(vertex))(_.removeVertex(vertex))

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : HypergraphBase[V, E, M] = Queries
    .find(queries, edge)
    .fold(this: HypergraphBase[V, E, M]): found =>
      patched(
        _.withEdge(found, Nil, multiplicity),
      )(_.withMultiplicity(edge, multiplicity))

  override def updated[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] = patched[W, F](
    _.withEdge(edge, edge.vertexValues, multiplicity),
  )(_.updated(edge, multiplicity))

  override protected lazy val delegate: HypergraphBase[V, E, M] = build()

  /**
    * @return
    *   This graph after the change [[change]] to its patch, which [[rebuild]]
    *   makes to the built graph.
    */
  private def patched[W >: V, F >: E]
    (change: Patch[W, Hyperedge[W, F], M] => Patch[W, Hyperedge[W, F], M])
    (rebuild: HypergraphBase[V, E, M] => HypergraphBase[W, F, M])
    : HypergraphBase[W, F, M] = LazyHypergraph(
    empty,
    queries.patched(change),
    () => rebuild(delegate),
  )

/** A graph answering queries like a [[LazyHypergraph]]. */
private[kneiphof] class LazyGraph[V, E, M : Multiplicity]
  (
    override val empty: GraphBase[Nothing, Nothing, M],
    queries: Queries[V, E, M, kneiphof.Edge[V, E]],
    build: () => GraphBase[V, E, M],
  )
  extends GraphBase[V, E, M],
          Delegating[V, E, M, Edge, [v, e] =>> GraphBase[v, e, M]]:

  override private[kneiphof] def eager: Boolean = false

  override def view: GraphBase[V, E, M] = this

  override def vertexValues: ValueSet[V] = queries.vertexValues

  override def hasVertex(vertex: Any): Boolean = queries.hasVertex(vertex)

  override def size: Int = queries.size.getOrElse(delegate.size)

  override def degree(vertex: Any): Int = queries
    .degree(vertex)
    .getOrElse(super.degree(vertex))

  override def multiplicity(edge: Any): M = queries.multiplicity(edge)

  override def hasEdge(edge: Any): Boolean = multiplicity(edge).nonZero

  override def incidentEdgeValues(vertex: Any): ValueSet[kneiphof.Edge[V, E]] =
    queries.incidentEdgeValues(vertex)

  override def addVertex[W >: V](vertex: W): GraphBase[W, E, M] =
    patched[W, E](_.withVertex(vertex))(_.addVertex(vertex))

  override def removeVertex(vertex: Any): GraphBase[V, E, M] =
    if !hasVertex(vertex) then this
    else patched(_.withoutVertex(vertex))(_.removeVertex(vertex))

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : GraphBase[V, E, M] = Queries
    .find(queries, edge)
    .fold(this: GraphBase[V, E, M]): found =>
      patched(
        _.withEdge(found, Nil, multiplicity),
      )(_.withMultiplicity(edge, multiplicity))

  override def updated[W >: V, F >: E]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: M,
    )
    : GraphBase[W, F, M] = patched[W, F](
    _.withEdge(edge, edge.vertexValues, multiplicity),
  )(_.updated(edge, multiplicity))

  override protected lazy val delegate: GraphBase[V, E, M] = build()

  /**
    * @return
    *   This graph after the change [[change]] to its patch, which [[rebuild]]
    *   makes to the built graph.
    */
  private def patched[W >: V, F >: E]
    (
      change: Patch[W, kneiphof.Edge[W, F], M] => Patch[
        W,
        kneiphof.Edge[W, F],
        M,
      ],
    )
    (rebuild: GraphBase[V, E, M] => GraphBase[W, F, M])
    : GraphBase[W, F, M] = LazyGraph(
    empty,
    queries.patched(change),
    () => rebuild(delegate),
  )

/** A directed hypergraph answering queries like a [[LazyHypergraph]]. */
private[kneiphof] class LazyDirectedHypergraph[V, E, M : Multiplicity]
  (
    override val empty: DirectedHypergraphBase[Nothing, Nothing, M],
    queries: Queries[V, E, M, Hyperarc[V, E]],
    build: () => DirectedHypergraphBase[V, E, M],
  )
  extends DirectedHypergraphBase[V, E, M],
          Delegating[
            V,
            E,
            M,
            Hyperarc,
            [v, e] =>> DirectedHypergraphBase[v, e, M],
          ]:

  override private[kneiphof] def eager: Boolean = false

  override def view: DirectedHypergraphBase[V, E, M] = this

  override def vertexValues: ValueSet[V] = queries.vertexValues

  override def hasVertex(vertex: Any): Boolean = queries.hasVertex(vertex)

  override def size: Int = queries.size.getOrElse(delegate.size)

  override def degree(vertex: Any): Int = queries
    .degree(vertex)
    .getOrElse(super.degree(vertex))

  override def multiplicity(edge: Any): M = queries.multiplicity(edge)

  override def hasEdge(edge: Any): Boolean = multiplicity(edge).nonZero

  override def incidentEdgeValues(vertex: Any): ValueSet[Hyperarc[V, E]] =
    queries.incidentEdgeValues(vertex)

  override def addVertex[W >: V](vertex: W): DirectedHypergraphBase[W, E, M] =
    patched[W, E](_.withVertex(vertex))(_.addVertex(vertex))

  override def removeVertex(vertex: Any): DirectedHypergraphBase[V, E, M] =
    if !hasVertex(vertex) then this
    else patched(_.withoutVertex(vertex))(_.removeVertex(vertex))

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : DirectedHypergraphBase[V, E, M] = Queries
    .find(queries, edge)
    .fold(this: DirectedHypergraphBase[V, E, M]): found =>
      patched(
        _.withEdge(found, Nil, multiplicity),
      )(_.withMultiplicity(edge, multiplicity))

  override def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] = patched[W, F](
    _.withEdge(edge, edge.vertexValues, multiplicity),
  )(_.updated(edge, multiplicity))

  override protected lazy val delegate: DirectedHypergraphBase[V, E, M] =
    build()

  /**
    * @return
    *   This graph after the change [[change]] to its patch, which [[rebuild]]
    *   makes to the built graph.
    */
  private def patched[W >: V, F >: E]
    (change: Patch[W, Hyperarc[W, F], M] => Patch[W, Hyperarc[W, F], M])
    (
      rebuild: DirectedHypergraphBase[V, E, M] => DirectedHypergraphBase[
        W,
        F,
        M,
      ],
    )
    : DirectedHypergraphBase[W, F, M] = LazyDirectedHypergraph(
    empty,
    queries.patched(change),
    () => rebuild(delegate),
  )

/** A directed graph answering queries like a [[LazyHypergraph]]. */
private[kneiphof] class LazyDirectedGraph[V, E, M : Multiplicity]
  (
    override val empty: DirectedGraphBase[Nothing, Nothing, M],
    queries: Queries[V, E, M, Arc[V, E]],
    build: () => DirectedGraphBase[V, E, M],
  )
  extends DirectedGraphBase[V, E, M],
          Delegating[
            V,
            E,
            M,
            Arc,
            [v, e] =>> DirectedGraphBase[v, e, M],
          ]:

  override private[kneiphof] def eager: Boolean = false

  override def view: DirectedGraphBase[V, E, M] = this

  override def vertexValues: ValueSet[V] = queries.vertexValues

  override def hasVertex(vertex: Any): Boolean = queries.hasVertex(vertex)

  override def size: Int = queries.size.getOrElse(delegate.size)

  override def degree(vertex: Any): Int = queries
    .degree(vertex)
    .getOrElse(super.degree(vertex))

  override def multiplicity(edge: Any): M = queries.multiplicity(edge)

  override def hasEdge(edge: Any): Boolean = multiplicity(edge).nonZero

  override def incidentEdgeValues(vertex: Any): ValueSet[Arc[V, E]] = queries
    .incidentEdgeValues(vertex)

  override def addVertex[W >: V](vertex: W): DirectedGraphBase[W, E, M] =
    patched[W, E](_.withVertex(vertex))(_.addVertex(vertex))

  override def removeVertex(vertex: Any): DirectedGraphBase[V, E, M] =
    if !hasVertex(vertex) then this
    else patched(_.withoutVertex(vertex))(_.removeVertex(vertex))

  override def withMultiplicity
    (edge: Any, multiplicity: M)
    : DirectedGraphBase[V, E, M] = Queries
    .find(queries, edge)
    .fold(this: DirectedGraphBase[V, E, M]): found =>
      patched(
        _.withEdge(found, Nil, multiplicity),
      )(_.withMultiplicity(edge, multiplicity))

  override def updated[W >: V, F >: E]
    (edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M] = patched[W, F](
    _.withEdge(edge, edge.vertexValues, multiplicity),
  )(_.updated(edge, multiplicity))

  override protected lazy val delegate: DirectedGraphBase[V, E, M] = build()

  /**
    * @return
    *   This graph after the change [[change]] to its patch, which [[rebuild]]
    *   makes to the built graph.
    */
  private def patched[W >: V, F >: E]
    (change: Patch[W, Arc[W, F], M] => Patch[W, Arc[W, F], M])
    (rebuild: DirectedGraphBase[V, E, M] => DirectedGraphBase[W, F, M])
    : DirectedGraphBase[W, F, M] = LazyDirectedGraph(
    empty,
    queries.patched(change),
    () => rebuild(delegate),
  )
