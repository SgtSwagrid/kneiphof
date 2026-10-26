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
private[kneiphof] trait Queries[+V, +E, M, +X]:

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

  /**
    * @return
    *   These queries, after the changes made by [[change]] to the graph.
    *   Changes to a patched graph change its patch, rather than patching it
    *   again.
    */
  def patched[W >: V, Y >: X]
    (change: Patch[W, Y, M] => Patch[W, Y, M])
    (using Multiplicity[M])
    : Queries[W, E, M, Y] = Patching(this, change(Patch.empty))

  /**
    * @return
    *   The number of edges gained since [[before]], counting every copy of
    *   each, where [[edges]] includes every edge whose multiplicity differs.
    */
  protected def changes
    (
      before: Queries[?, ?, M, ?],
      edges: Iterable[Any],
    )
    (using Multiplicity[M])
    : Int = ValueSet
    .from(edges)
    .toSeq
    .map(edge => multiplicity(edge).count - before.multiplicity(edge).count)
    .sum

/** The companion object for [[Queries]]. */
private[kneiphof] object Queries:

  /** @return The distinct vertices of [[edge]], if it's an edge. */
  def ends(edge: Any): ValueSet[Any] = edge match
    case edge: AnyEdge[?, ?, ?] => edge.vertexValues
    case _                      => ValueSet.empty

  /**
    * @return
    *   The edge of the graph described by [[queries]] equal to [[edge]], if
    *   there is one, found among the edges at one of its ends.
    */
  def find[X](queries: Queries[?, ?, ?, X], edge: Any): Option[X] = ends(edge)
    .headOption
    .flatMap(queries.incidentEdgeValues(_).get(edge))
