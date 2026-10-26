package com.alecdorrington
package kneiphof

/**
  * The graph operations of an edge of kind [[X]], which is the multigraph of
  * kind [[G]] containing just itself, once. Its edges and incidence are
  * answered by the edge itself, and everything else by the graph it builds.
  */
private[kneiphof] trait SingleEdge[
  +V,
  +E,
  +X[+v, +e] <: AnyEdge[v, e, X],
  +G[+v, +e] <: GraphOps[v, e, Count, X, G],
] extends Delegating[V, E, Count, X, G]:
  this: X[V, E] & G[V, E] =>

  override def edgeValues: ValueMap[X[V, E], Count] = ValueMap
    .empty
    .updated(this, Count(1))

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E]] =
    if vertexValues.contains(vertex) then ValueSet(this) else ValueSet.empty

  override final def hashCode: Int =
    AnyGraph.hash(vertexValues, Seq(shapeHash -> Count(1)))
