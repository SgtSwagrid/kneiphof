package com.alecdorrington
package kneiphof
package incidence

/** A hypergraph held in an [[Incidence]] store. */
private[kneiphof] final class IncidenceHypergraph[V, E, M : Multiplicity]
  (override protected val store: Incidence[V, Hyperedge[V, E], M])
  extends HypergraphBase[V, E, M],
          IncidenceOps[
            V,
            E,
            M,
            Hyperedge,
            [v, e] =>> HypergraphBase[v, e, M],
          ]:

  override protected def build[W, F]
    (store: Incidence[W, Hyperedge[W, F], M])
    : HypergraphBase[W, F, M] = IncidenceHypergraph(store)

  override def updated[W >: V, F >: E]
    (edge: Hyperedge[W, F], multiplicity: M)
    : HypergraphBase[W, F, M] = set(edge, multiplicity)

/** The companion object for [[IncidenceHypergraph]]. */
private[kneiphof] object IncidenceHypergraph:

  /** @return The empty hypergraph in this representation. */
  def empty[M : Multiplicity]: HypergraphBase[Nothing, Nothing, M] =
    IncidenceHypergraph(Incidence.empty)

/** A graph held in an [[Incidence]] store. */
private[kneiphof] final class IncidenceGraph[V, E, M : Multiplicity]
  (override protected val store: Incidence[V, kneiphof.Edge[V, E], M])
  extends GraphBase[V, E, M],
          IncidenceOps[
            V,
            E,
            M,
            kneiphof.Edge,
            [v, e] =>> GraphBase[v, e, M],
          ]:

  override protected def build[W, F]
    (store: Incidence[W, kneiphof.Edge[W, F], M])
    : GraphBase[W, F, M] = IncidenceGraph(store)

  override def updated[W >: V, F >: E]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: M,
    )
    : GraphBase[W, F, M] = set(edge, multiplicity)

/** The companion object for [[IncidenceGraph]]. */
private[kneiphof] object IncidenceGraph:

  /** @return The empty graph in this representation. */
  def empty[M : Multiplicity]: GraphBase[Nothing, Nothing, M] =
    IncidenceGraph(Incidence.empty)

/** A directed hypergraph held in an [[Incidence]] store. */
private[kneiphof] final class IncidenceDirectedHypergraph[
  V,
  E,
  M : Multiplicity,
]
  (override protected val store: Incidence[V, Hyperarc[V, E], M])
  extends DirectedHypergraphBase[V, E, M],
          IncidenceOps[
            V,
            E,
            M,
            Hyperarc,
            [v, e] =>> DirectedHypergraphBase[v, e, M],
          ]:

  override protected def build[W, F]
    (store: Incidence[W, Hyperarc[W, F], M])
    : DirectedHypergraphBase[W, F, M] = IncidenceDirectedHypergraph(store)

  override def updated[W >: V, F >: E]
    (edge: Hyperarc[W, F], multiplicity: M)
    : DirectedHypergraphBase[W, F, M] = set(edge, multiplicity)

  override def transpose: DirectedHypergraphBase[V, E, M] =
    rebuild(identity, _.reverse)

/** The companion object for [[IncidenceDirectedHypergraph]]. */
private[kneiphof] object IncidenceDirectedHypergraph:

  /** @return The empty directed hypergraph in this representation. */
  def empty[M : Multiplicity]: DirectedHypergraphBase[Nothing, Nothing, M] =
    IncidenceDirectedHypergraph(Incidence.empty)

/** A directed graph held in an [[Incidence]] store. */
private[kneiphof] final class IncidenceDirectedGraph[V, E, M : Multiplicity]
  (override protected val store: Incidence[V, Arc[V, E], M])
  extends DirectedGraphBase[V, E, M],
          IncidenceOps[
            V,
            E,
            M,
            Arc,
            [v, e] =>> DirectedGraphBase[v, e, M],
          ]:

  override protected def build[W, F]
    (store: Incidence[W, Arc[W, F], M])
    : DirectedGraphBase[W, F, M] = IncidenceDirectedGraph(store)

  override def updated[W >: V, F >: E]
    (edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M] = set(edge, multiplicity)

  override def transpose: DirectedGraphBase[V, E, M] =
    rebuild(identity, _.reverse)

/** The companion object for [[IncidenceDirectedGraph]]. */
private[kneiphof] object IncidenceDirectedGraph:

  /** @return The empty directed graph in this representation. */
  def empty[M : Multiplicity]: DirectedGraphBase[Nothing, Nothing, M] =
    IncidenceDirectedGraph(Incidence.empty)
