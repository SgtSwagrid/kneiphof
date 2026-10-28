package com.alecdorrington
package kneiphof
package incidence

/**
  * The difference of the graph whose incidence structure is [[base]] and the
  * graphs whose structures are [[others]] and [[gathered]], with the vertices
  * of [[base]], and the multiplicity of each edge in each of the others
  * subtracted. Subtracting one graph after another is subtracting their sum, so
  * a chain of differences is one difference, and a few stored edges at a time
  * are gathered into [[gathered]], keeping [[others]] and their index.
  */
private[kneiphof] final class Difference[
  +V,
  M : Multiplicity,
  +X <: AnyEdge[?, ?, ?],
]
  (
    val base: IncidenceStructure[V, M, X],
    val others: Operands[Any, M, AnyEdge[?, ?, ?]],
    val gathered: Patch[Any, M, AnyEdge[?, ?, ?]],
  )
  extends IncidenceStructure[V, M, X]:

  override def vertexValues: ValueSet[V] = base.vertexValues

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex)

  override def vertexValue(vertex: Any): Option[V] = base.vertexValue(vertex)

  override def order: Int = base.order

  override def knownOrder: Option[Long] = base.knownOrder

  override def maxOrder: Option[Long] = base.maxOrder

  override def vertexIterator: Iterator[V] = base.vertexIterator

  override def multiplicity(edge: Any): M =
    subtract(edge, base.multiplicity(edge))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = base
    .incidentEdges(vertex)
    .map((edge, multiplicity) => edge -> subtract(edge, multiplicity))
    .filter((_, multiplicity) => multiplicity.nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  /**
    * The number of edges, counted from the edges of [[base]] if there are no
    * more of those than of the others, and otherwise from those of the others,
    * which are the only ones that can be lost.
    */
  override lazy val size: Int =
    if IncidenceStructure.noLonger(base.incidences, subtrahendIncidences) then
      super.size
    else
      Math.addExact(
        base.size,
        IncidenceStructure.changes(
          this,
          base,
          ValueSet.from(subtrahendEdges),
        ),
      )

  /**
    * @return
    *   This difference, with its gathered edges replaced by [[gathered]], which
    *   includes them.
    */
  def withGathered
    (gathered: Patch[Any, M, AnyEdge[?, ?, ?]])
    : Difference[V, M, X] = Difference(base, others, gathered)

  /** The edges of the graphs subtracted, with repeats. */
  private def subtrahendEdges: Iterator[Any] = (others.all :+ gathered)
    .iterator
    .flatMap(_.edges.map((edge, _) => edge))

  /** The incidences of the graphs subtracted (see [[incidences]]). */
  private def subtrahendIncidences: Iterator[Any] = (others.all :+ gathered)
    .iterator
    .flatMap(_.incidences)

  /**
    * @return
    *   [[multiplicity]], less the multiplicities of [[edge]] in each of the
    *   graphs subtracted which contain its first end.
    */
  private def subtract(edge: Any, multiplicity: M): M = IncidenceStructure
    .ends(edge)
    .headOption
    .fold(multiplicity): end =>
      candidatesAt(end).foldLeft(multiplicity)(_ - _.multiplicity(edge))

  /**
    * @return
    *   The graphs subtracted which may have an edge at [[vertex]] (see
    *   [[Operands.candidates]]).
    */
  private def candidatesAt
    (vertex: Any)
    : Seq[IncidenceStructure[Any, M, AnyEdge[?, ?, ?]]] =
    val candidates = others.candidates(vertex)
    if gathered.hasVertex(vertex) then candidates :+ gathered else candidates

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
    case base: Difference[V, M, X] => Difference(
        base.base,
        base.others.append(other),
        base.gathered,
      )
    case _ => Difference(
        base,
        Operands.of(other),
        Patch(Explicit.empty[M]),
      )
