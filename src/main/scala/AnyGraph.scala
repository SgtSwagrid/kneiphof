package com.alecdorrington
package kneiphof

import scala.compiletime.deferred

/**
  * A graph of any kind, with vertices of type [[V]], edge labels of type [[E]]
  * and edge multiplicities in [[M]].
  *
  * Each edge value has a multiplicity, and is present exactly when its
  * multiplicity isn't `zero` (see [[Multiplicity]]). Two graphs are equal when
  * they have the same vertices and the same multiplicity for every edge.
  */
trait AnyGraph[+V, +E, M]:

  /** The algebra of edge multiplicities. */
  protected given algebra: Multiplicity[M] = deferred

  /** The values of the vertices of this graph. */
  def vertexValues: ValueSet[V]

  /** The multiplicity of each edge value present in this graph. */
  def edgeValues: ValueMap[Any, M]

  /** @return `true` if and only if [[vertex]] is a vertex of this graph. */
  def hasVertex(vertex: Any): Boolean = vertexValues.contains(vertex)

  /** @return `true` if and only if [[edge]] is an edge of this graph. */
  def hasEdge(edge: Any): Boolean = edgeValues.contains(edge)

  /** @return The multiplicity of [[edge]], which is `zero` if it's absent. */
  def multiplicity(edge: Any): M = edgeValues.getOrElse(edge, algebra.zero)

  /** The number of vertices in this graph. */
  def order: Int = vertexValues.size

  /** The number of edges in this graph, counting every copy of each edge. */
  def size: Int = edgeValues.values.map(_.count).sum

  override def equals(that: Any): Boolean = that match
    case that: AnyGraph[?, ?, ?] => vertexValues == that.vertexValues &&
      edgeValues == that.edgeValues
    case _ => false

  override def hashCode: Int = (vertexValues, edgeValues).##
