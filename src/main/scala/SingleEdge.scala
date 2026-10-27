package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{Explicit, IncidenceStructure}

/**
  * The graph operations of an edge of kind [[X]], which is the multigraph of
  * kind [[G]] containing just itself, once. Its edges and incidence are
  * answered by the edge itself.
  */
private[kneiphof] trait SingleEdge[
  +V,
  +E,
  +X[+v, +e] <: AnyEdge[v, e, X],
  +G[+v, +e] <: GraphOps[v, e, Count, X, G],
] extends GraphOps[V, E, Count, X, G]:
  this: X[V, E] & G[V, E] =>

  override private[kneiphof] lazy val structure
    : IncidenceStructure[V, Count, X[V, E]] = Explicit
    .empty[Count]
    .increase(this, vertexValues, Count(1))

  override lazy val edgeValues: ValueMap[X[V, E], Count] = ValueMap
    .empty
    .updated(this, Count(1))

  override def incidentEdgeValues[A : NotHandle](vertex: A): ValueSet[X[V, E]] =
    if vertexValues.contains(vertex) then ValueSet(this) else ValueSet.empty

  override final def hashCode: Int =
    AnyGraph.hash(vertexValues, Seq(shapeHash -> Count(1)))
