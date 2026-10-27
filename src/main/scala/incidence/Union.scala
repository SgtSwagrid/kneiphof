package com.alecdorrington
package kneiphof
package incidence

/**
  * The union of the graphs whose incidence structures are [[parts]] and
  * [[gathered]], which takes the join of their multiplicities.
  */
private[kneiphof] final class Union[
  +V,
  M : Multiplicity,
  +X <: AnyEdge[?, ?, ?],
]
  (
    override val parts: Operands[V, M, X],
    override val gathered: Patch[V, M, X],
  )
  extends Overlay[V, M, X]:

  override protected def combine(x: M, y: M): M = x | y

  /**
    * The number of edges, counted from the size of the graph with the most
    * vertices and the edges of the others, which are the only ones whose
    * multiplicities can differ from it.
    */
  override lazy val size: Int =
    if disjoint then IncidenceStructure.total(operands.map(_.size))
    else
      val largest = operands.maxBy(IncidenceStructure.orderHint)
      val others  = operands.filterNot(_ eq largest)
      Math.addExact(
        largest.size,
        IncidenceStructure.changes(
          this,
          largest,
          ValueSet.from(others.iterator.flatMap(_.edges.map((edge, _) => edge))),
        ),
      )

  /**
    * The degree of [[vertex]], which is its degree in the only graph containing
    * it, if there's only one.
    */
  override def degree(vertex: Any): Int = ownersOf(vertex) match
    case Seq(only) => only.degree(vertex)
    case _         => super.degree(vertex)

  /**
    * @return
    *   This union, with its gathered edges replaced by [[gathered]], which
    *   includes them.
    */
  def withGathered[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (gathered: Patch[W, M, Y])
    : Union[W, M, Y] = Union(parts, gathered)

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
    : Union[V, M, X] =
    val (parts, gathered) = Overlay.merge(first, second):
      case union: Union[V, M, X] => Some(union)
      case _                     => None
    Union(parts, gathered)
