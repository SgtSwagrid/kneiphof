package com.alecdorrington
package kneiphof
package incidence

/**
  * The union of the graphs whose incidence structures are [[operands]], which
  * takes the join of their multiplicities.
  */
private[kneiphof] final class Union[
  +V,
  M : Multiplicity,
  +X <: AnyEdge[?, ?, ?],
]
  (override val operands: Seq[IncidenceStructure[V, M, X]])
  extends Overlay[V, M, X]:

  override protected def combine(x: M, y: M): M = x | y

  /**
    * The number of edges, counted from the size of the largest graph and the
    * edges of the others, which are the only ones whose multiplicities can
    * differ from the largest.
    */
  override lazy val size: Int =
    if disjoint then IncidenceStructure.total(operands.map(_.size))
    else
      val largest = operands.maxBy(_.size)
      val others  = operands.filterNot(_ eq largest)
      Math.addExact(
        largest.size,
        IncidenceStructure.changes(
          this,
          largest,
          ValueSet.from(others.iterator.flatMap(_.edges.map((edge, _) => edge))),
        ),
      )

  override def degree(vertex: Any): Int =
    if !disjoint then super.degree(vertex)
    else ownersOf(vertex).headOption.fold(0)(_.degree(vertex))

/** The companion object for [[Union]]. */
private[kneiphof] object Union:

  /**
    * @return
    *   The union of the graphs whose incidence structures are [[first]] and
    *   [[second]], taking the operands of either which is already a union.
    */
  def of[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      first: IncidenceStructure[V, M, X],
      second: IncidenceStructure[V, M, X],
    )
    : Union[V, M, X] = Union:
    Overlay.merged(first, second):
      case union: Union[V, M, X] => Some(union.operands)
      case _                     => None
