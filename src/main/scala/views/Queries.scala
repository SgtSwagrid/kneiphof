package com.alecdorrington
package kneiphof
package views

/**
  * Answers to the queries about a graph which can be computed without building
  * it, for graphs which are views of other graphs.
  *
  * @tparam X
  *   The type of the graph's edges.
  */
private[kneiphof] trait Queries[V, E, M, X]:

  /** The values of the vertices of the graph. */
  def vertexValues: ValueSet[V]

  /** @return `true` if and only if [[vertex]] is a vertex of the graph. */
  def hasVertex(vertex: Any): Boolean

  /** @return The multiplicity of [[edge]] in the graph. */
  def multiplicity(edge: Any): M

  /** @return The values of the edges incident to [[vertex]]. */
  def incidentEdgeValues(vertex: Any): ValueSet[X]

  /** The number of edges, if it can be counted without building the graph. */
  def size: Option[Int]

  /**
    * @return
    *   The degree of [[vertex]], if it can be counted without looking up every
    *   incident edge.
    */
  def degree(vertex: Any): Option[Int]
