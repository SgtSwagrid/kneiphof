package com.alecdorrington
package kneiphof
package incidence

/**
  * The incidence structures of the graphs which an overlay or a difference
  * combines, in order, indexed by their vertices.
  *
  * The operands with few vertices are indexed, and the others are asked
  * directly, so that a large operand's vertices are never listed just to find
  * out which operands have a vertex. Overlays and differences which differ only
  * in the few stored edges they gather share their operands, and so the index.
  */
private[kneiphof] final class Operands[+V, M, +X <: AnyEdge[?, ?, ?]]
  (val all: Vector[IncidenceStructure[V, M, X]]):

  /** @return The operands containing [[vertex]], in order. */
  def containing(vertex: Any): Seq[IncidenceStructure[V, M, X]] =
    val positions = indexed.getOrElse(vertex, Nil) ++ large.collect:
      case (operand, position) if operand.hasVertex(vertex) => position
    positions.sorted.map(all)

  /** @return These operands, followed by [[next]]. */
  def append[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (next: IncidenceStructure[W, M, Y])
    : Operands[W, M, Y] = Operands(all :+ next)

  /** @return These operands, followed by [[more]]. */
  def concat[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (more: Operands[W, M, Y])
    : Operands[W, M, Y] = Operands(all ++ more.all)

  /** `true` if and only if no two of the operands share a vertex. */
  lazy val disjoint: Boolean = indexed.values.forall(_.size == 1) &&
    indexed
      .keysIterator
      .forall(vertex =>
        !large.exists((operand, _) => operand.hasVertex(vertex)),
      ) &&
    large
      .map((operand, _) => operand)
      .combinations(2)
      .forall:
        case Seq(first, second) =>
          val (smaller, larger) =
            if IncidenceStructure.orderHint(first) <=
                IncidenceStructure.orderHint(second)
            then (first, second)
            else (second, first)
          smaller.vertexValues.forall(!larger.hasVertex(_))
        case _ => true

  /** The operands with few enough vertices to index, and the others. */
  private lazy val (small, large) = all
    .zipWithIndex
    .partition((operand, _) =>
      IncidenceStructure.orderHint(operand) <= IncidenceStructure.indexable,
    )

  /** The positions of the operands with few vertices containing each vertex. */
  private lazy val indexed: Map[Any, Seq[Int]] = small
    .iterator
    .flatMap((operand, position) =>
      operand.vertexValues.iterator.map(vertex => (vertex: Any) -> position),
    )
    .toSeq
    .groupMap((vertex, _) => vertex)((_, position) => position)

/** The companion object for [[Operands]]. */
private[kneiphof] object Operands:

  /** @return The operands [[operands]], in order. */
  def of[V, M, X <: AnyEdge[?, ?, ?]]
    (operands: IncidenceStructure[V, M, X]*)
    : Operands[V, M, X] = Operands(operands.toVector)
