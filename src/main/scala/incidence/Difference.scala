package com.alecdorrington
package kneiphof
package incidence

/**
  * The difference of the graph whose incidence structure is [[base]] and the
  * graphs whose structures are [[others]], with the vertices of [[base]], and
  * the multiplicity of each edge in each of [[others]] subtracted. Subtracting
  * one graph after another is subtracting their sum, so a chain of differences
  * is one difference.
  */
private[kneiphof] final class Difference[
  +V,
  M : Multiplicity as M,
  +X <: AnyEdge[?, ?, ?],
]
  (
    val base: IncidenceStructure[V, M, X],
    val others: Seq[IncidenceStructure[Any, M, AnyEdge[?, ?, ?]]],
  )
  extends IncidenceStructure[V, M, X]:

  override def vertexValues: ValueSet[V] = base.vertexValues

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex)

  override def multiplicity(edge: Any): M =
    subtracted(edge, base.multiplicity(edge))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = base
    .incidentEdges(vertex)
    .map((edge, multiplicity) => edge -> subtracted(edge, multiplicity))
    .filter(_(1).nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  /** The number of edges, counted from those of [[others]], which are lost. */
  override lazy val size: Int = Math.addExact(
    base.size,
    IncidenceStructure.changes(
      this,
      base,
      ValueSet.from(others.iterator.flatMap(_.edges.map(_(0)))),
    ),
  )

  /**
    * @return
    *   The multiplicity [[multiplicity]], less the multiplicities of [[edge]]
    *   in each of [[others]].
    */
  private def subtracted(edge: Any, multiplicity: M): M = IncidenceStructure
    .ends(edge)
    .headOption
    .fold(multiplicity): end =>
      owners
        .getOrElse(end, Nil)
        .foldLeft(multiplicity)(_ - _.multiplicity(edge))

  /** The graphs of [[others]] containing each vertex. */
  private lazy val owners
    : Map[
      Any,
      Seq[IncidenceStructure[Any, M, AnyEdge[?, ?, ?]]],
    ] = IncidenceStructure.owners(others)

/** The companion object for [[Difference]]. */
private[kneiphof] object Difference:

  /**
    * @return
    *   The difference of the graphs whose incidence structures are [[base]] and
    *   [[other]], subtracting [[other]] too if [[base]] is already a
    *   difference.
    */
  def of[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      base: IncidenceStructure[V, M, X],
      other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
    )
    : Difference[V, M, X] = base match
    case base: Difference[V, M, X] =>
      Difference(base.base, base.others :+ other)
    case _ => Difference(base, Seq(other))
