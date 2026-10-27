package com.alecdorrington
package kneiphof
package incidence

/**
  * The intersection of the graph whose incidence structure is [[base]] with the
  * graphs whose structures are [[others]], with their common vertices and the
  * meet of each edge's multiplicities. Its vertices are found among those of
  * whichever graph has the fewest, and its edges are those of [[base]] at them.
  */
private[kneiphof] final class Intersection[
  +V,
  M : Multiplicity,
  +X <: AnyEdge[?, ?, ?],
]
  (
    val base: IncidenceStructure[V, M, X],
    val others: Seq[IncidenceStructure[Any, M, AnyEdge[?, ?, ?]]],
  )
  extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] =
    val fewest = (base +: others).minBy(IncidenceStructure.orderBound)
    ValueSet.from:
      fewest
        .vertexValues
        .iterator
        .flatMap(base.vertexValue)
        .filter(vertex => others.forall(_.hasVertex(vertex)))

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex) &&
    others.forall(_.hasVertex(vertex))

  override def vertexValue(vertex: Any): Option[V] = base
    .vertexValue(vertex)
    .filter(vertex => others.forall(_.hasVertex(vertex)))

  override def multiplicity(edge: Any): M = meet(edge, base.multiplicity(edge))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    if !hasVertex(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .map((edge, multiplicity) => edge -> meet(edge, multiplicity))
        .filter((_, multiplicity) => multiplicity.nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  override lazy val size: Int = super.size

  /**
    * @return
    *   The meet of [[multiplicity]] with the multiplicities of [[edge]] in each
    *   of [[others]].
    */
  private def meet(edge: Any, multiplicity: M): M =
    others.foldLeft(multiplicity)(_ & _.multiplicity(edge))

/** The companion object for [[Intersection]]. */
private[kneiphof] object Intersection:

  /**
    * @return
    *   The intersection of the graphs whose incidence structures are [[base]]
    *   and [[other]], taking the operands of either which is already an
    *   intersection.
    */
  def of[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      base: IncidenceStructure[V, M, X],
      other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
    )
    : Intersection[V, M, X] =
    val extra = other match
      case other: Intersection[Any, M, AnyEdge[?, ?, ?]] => other.base +:
          other.others
      case _ => Seq(other)
    base match
      case base: Intersection[V, M, X] =>
        Intersection(base.base, base.others ++ extra)
      case _ => Intersection(base, extra)
