package com.alecdorrington
package kneiphof
package incidence

/**
  * The sum of the graphs whose incidence structures are [[parts]] and
  * [[gathered]], which adds the multiplicities of their edges. It's only used
  * for additive multiplicities, so its sizes and degrees are those of the
  * graphs added.
  */
private[kneiphof] final class Sum[+V, M : Multiplicity, +X <: AnyEdge[?, ?, ?]]
  (
    override val parts: Operands[V, M, X],
    override val gathered: Patch[V, M, X],
  )
  extends Overlay[V, M, X]:

  override protected def combine(x: M, y: M): M = x + y

  override lazy val size: Int = IncidenceStructure.total(operands.map(_.size))

  override def degree(vertex: Any): Int =
    IncidenceStructure.total(ownersOf(vertex).map(_.degree(vertex)))

  /**
    * @return
    *   This sum, with its gathered edges replaced by [[gathered]], which
    *   includes them.
    */
  def withGathered[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (gathered: Patch[W, M, Y])
    : Sum[W, M, Y] = Sum(parts, gathered)

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
    : Sum[V, M, X] =
    val (parts, gathered) = Overlay.merge(first, second):
      case sum: Sum[V, M, X] => Some(sum)
      case _                 => None
    Sum(parts, gathered)
