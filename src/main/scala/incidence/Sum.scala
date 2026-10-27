package com.alecdorrington
package kneiphof
package incidence

/**
  * The sum of the graphs whose incidence structures are [[operands]], which
  * adds the multiplicities of their edges. It's only used for additive
  * multiplicities, so its sizes and degrees are those of the graphs added.
  */
private[kneiphof] final class Sum[+V, M : Multiplicity, +X <: AnyEdge[?, ?, ?]]
  (override val operands: Seq[IncidenceStructure[V, M, X]])
  extends Overlay[V, M, X]:

  override protected def combine(x: M, y: M): M = x + y

  override lazy val size: Int = IncidenceStructure.total(operands.map(_.size))

  override def degree(vertex: Any): Int =
    IncidenceStructure.total(ownersOf(vertex).map(_.degree(vertex)))

/** The companion object for [[Sum]]. */
private[kneiphof] object Sum:

  /**
    * @return
    *   The sum of the graphs whose incidence structures are [[first]] and
    *   [[second]], taking the operands of either which is already a sum.
    */
  def of[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      first: IncidenceStructure[V, M, X],
      second: IncidenceStructure[V, M, X],
    )
    : Sum[V, M, X] = Sum:
    Overlay.merged(first, second):
      case sum: Sum[V, M, X] => Some(sum.operands)
      case _                 => None
