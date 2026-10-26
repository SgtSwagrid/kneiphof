package com.alecdorrington
package kneiphof
package views

/**
  * A hypergraph answering [[queries]] without being built, and answering
  * everything else with the hypergraph from [[build]], which is only built when
  * first needed.
  */
private[kneiphof] final class LazyHypergraph[V, E, M : Multiplicity]
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

  override def updated[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] = delegate.updated(edge, multiplicity)

  override protected lazy val delegate: HypergraphBase[V, E, M] = build()

/** A graph answering queries like a [[LazyHypergraph]]. */
private[kneiphof] final class LazyGraph[V, E, M : Multiplicity]
  (
    override val empty: GraphBase[Nothing, Nothing, M],
    queries: Queries[V, E, M, Edge[V, E]],
    build: () => GraphBase[V, E, M],
  )
  extends GraphBase[V, E, M],
          Delegating[V, E, M, Edge, [v, e] =>> GraphBase[v, e, M]]:

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

  override def updated[W >: V, F >: E]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: M,
    )
    : GraphBase[W, F, M] = delegate.updated(edge, multiplicity)

  override protected lazy val delegate: GraphBase[V, E, M] = build()

/** A directed hypergraph answering queries like a [[LazyHypergraph]]. */
private[kneiphof] final class LazyDirectedHypergraph[V, E, M : Multiplicity]
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

  override def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] = delegate.updated(edge, multiplicity)

  override def transpose: DirectedHypergraphBase[V, E, M] = delegate.transpose

  override protected lazy val delegate: DirectedHypergraphBase[V, E, M] =
    build()

/** A directed graph answering queries like a [[LazyHypergraph]]. */
private[kneiphof] final class LazyDirectedGraph[V, E, M : Multiplicity]
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

  override def updated[W >: V, F >: E]
    (edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M] = delegate.updated(edge, multiplicity)

  override def transpose: DirectedGraphBase[V, E, M] = delegate.transpose

  override protected lazy val delegate: DirectedGraphBase[V, E, M] = build()
