package com.alecdorrington
package kneiphof
package incidence

/**
  * The intersection of the graph whose incidence structure is [[base]] with the
  * graphs whose structures are [[others]], with their common vertices and the
  * meet of each edge's multiplicities. Its edges are those of [[base]], found
  * among the edges of the first of [[others]].
  */
private[kneiphof] final class Intersection[
  +V,
  M : Multiplicity as M,
  +X <: AnyEdge[?, ?, ?],
]
  (
    val base: IncidenceStructure[V, M, X],
    val others: Seq[IncidenceStructure[Any, M, AnyEdge[?, ?, ?]]],
  )
  extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] = base
    .vertexValues
    .filter(vertex => others.forall(_.hasVertex(vertex)))

  override def hasVertex(vertex: Any): Boolean = base.hasVertex(vertex) &&
    others.forall(_.hasVertex(vertex))

  override def multiplicity(edge: Any): M = meet(edge, base.multiplicity(edge))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    if !hasVertex(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .map((edge, multiplicity) => edge -> meet(edge, multiplicity))
        .filter(_(1).nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  override def edges: Iterator[(X, M)] = others
    .head
    .edges
    .flatMap((edge, _) => base.find(edge))
    .map(edge => edge -> multiplicity(edge))
    .filter(_(1).nonZero)

  override lazy val size: Int = IncidenceStructure.total(edges.map(_(1).count))

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
