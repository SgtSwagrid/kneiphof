package com.alecdorrington
package kneiphof

/**
  * A graph which forwards its primitive operations to the graph [[delegate]],
  * for values which are graphs without storing one, such as edges, chains and
  * joins.
  */
private[kneiphof] trait Delegating[
  +V,
  +E,
  M,
  +X[+v, +e] <: AnyEdge[v, e, X],
  +G[+v, +e] <: GraphOps[v, e, M, X, G],
] extends GraphOps[V, E, M, X, G]:
  this: G[V, E] =>

  /** The graph to forward to. */
  protected def delegate: G[V, E]

  override def vertexValues: ValueSet[V] = delegate.vertexValues

  override def edgeValues: ValueMap[X[V, E], M] = delegate.edgeValues

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E]] = delegate
    .incidentEdgeValues(vertex)

  override def empty: G[Nothing, Nothing] = delegate.empty

  override def addVertex[W >: V](vertex: W): G[W, E] =
    delegate.addVertex(vertex)

  override def removeVertex(vertex: Any): G[V, E] =
    delegate.removeVertex(vertex)

  override def withMultiplicity(edge: Any, multiplicity: M): G[V, E] = delegate
    .withMultiplicity(edge, multiplicity)
