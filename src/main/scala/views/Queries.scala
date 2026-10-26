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
    *   The degree of [[vertex]], if it can be counted without looking at every
    *   incident edge.
    */
  def degree(vertex: Any): Option[Int]

  /**
    * @return
    *   The edges incident to [[vertex]], each with its multiplicity, so that
    *   degrees can be counted without looking each edge up again.
    */
  def incidentEdges(vertex: Any): Iterable[(X, M)] = incidentEdgeValues(vertex)
    .toSeq
    .map(edge => edge -> multiplicity(edge))

  /**
    * @return
    *   The edge of the graph equal to [[edge]], if there is one, found among
    *   the edges at one of its ends.
    */
  def find(edge: Any): Option[X] = Queries
    .ends(edge)
    .headOption
    .flatMap(incidentEdgeValues(_).get(edge))

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
    : Int = Queries.total:
    ValueSet
      .from(edges)
      .iterator
      .map(edge => multiplicity(edge).count - before.multiplicity(edge).count)

/** The companion object for [[Queries]]. */
private[kneiphof] object Queries:

  /**
    * Matches any undirected edge with two ends, as its ends and label, whatever
    * class it is, since equal edges must be found alike.
    */
  object Pair:

    def unapply(edge: Any): Option[(Any, Any, Any)] = edge match
      case edge: Hyperedge[?, ?] => edge.ends match
          case Seq(first, second) => Some((first, second, edge.label))
          case _                  => None
      case _ => None

  /**
    * Matches any directed edge with one source and one target, as its source,
    * target and label, whatever class it is.
    */
  object Arrow:

    def unapply(edge: Any): Option[(Any, Any, Any)] = edge match
      case edge: DirectedHyperedge[?, ?, ?] =>
        (edge.tail.toSeq, edge.head.toSeq) match
          case (Seq(source), Seq(target)) => Some((source, target, edge.label))
          case _                          => None
      case _ => None

  /** @return The distinct vertices of [[edge]], if it's an edge. */
  def ends(edge: Any): ValueSet[Any] = edge match
    case edge: AnyEdge[?, ?, ?] => edge.vertexValues
    case _                      => ValueSet.empty

  /** @return The sum of [[counts]], failing rather than overflowing. */
  def total(counts: IterableOnce[Int]): Int = counts
    .iterator
    .foldLeft(0)(Math.addExact)

  /**
    * @return
    *   The degree of [[vertex]] in a graph whose edges at [[vertex]] are
    *   [[incident]], counting every copy of each edge, and loops twice.
    */
  def degree[M : Multiplicity]
    (
      vertex: Any,
      incident: Iterable[(AnyEdge[?, ?, ?], M)],
    )
    : Int = total:
    incident
      .iterator
      .map((edge, multiplicity) =>
        Math.multiplyExact(
          edge.occurrences(vertex),
          multiplicity.count,
        ),
      )
