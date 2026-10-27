package com.alecdorrington
package kneiphof
package incidence

/**
  * The union of the graphs whose incidence structures are [[first]] and
  * [[second]], which takes the join of their multiplicities.
  */
private[kneiphof] final class Union[
  V,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
]
  (
    first: IncidenceStructure[V, M, X],
    second: IncidenceStructure[V, M, X],
  )
  extends Overlay[V, M, X](first, second):

  override protected def combine(x: M, y: M): M = x | y

  /**
    * The number of edges, counted from the sizes of the two graphs and the
    * edges of the smaller one, which are the only ones whose multiplicities can
    * differ from the larger.
    */
  override lazy val size: Int =
    if disjoint then Math.addExact(first.size, second.size)
    else
      val (larger, smaller) =
        if second.size <= first.size then (first, second) else (second, first)
      Math.addExact(
        larger.size,
        IncidenceStructure.changes(this, larger, smaller.edges.map(_(0))),
      )

  override def degree(vertex: Any): Int =
    if !disjoint then super.degree(vertex)
    else if first.hasVertex(vertex) then first.degree(vertex)
    else second.degree(vertex)
