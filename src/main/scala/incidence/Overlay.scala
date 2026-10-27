package com.alecdorrington
package kneiphof
package incidence

/**
  * The overlay of the graphs whose incidence structures are [[operands]], with
  * every vertex and edge of any of them, and each edge's multiplicities
  * combined by [[combine]]: a [[Sum]] or a [[Union]]. Overlays of overlays of
  * the same operation merge into one, so that long chains of them stay shallow.
  */
private[kneiphof] trait Overlay[+V, M, +X <: AnyEdge[?, ?, ?]]
  extends IncidenceStructure[V, M, X]:

  /** The incidence structures of the graphs overlaid. */
  def operands: Seq[IncidenceStructure[V, M, X]]

  /**
    * @return
    *   The multiplicity of an edge with multiplicities [[x]] and [[y]].
    */
  protected def combine(x: M, y: M): M

  override lazy val vertexValues: ValueSet[V] =
    operands.foldLeft(ValueSet.empty[V])(_ | _.vertexValues)

  override def hasVertex(vertex: Any): Boolean = owners.contains(vertex)

  override def multiplicity(edge: Any): M = IncidenceStructure
    .ends(edge)
    .headOption
    .fold(algebra.zero): end =>
      ownersOf(end).foldLeft(algebra.zero): (sum, operand) =>
        combine(sum, operand.multiplicity(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = ownersOf(vertex)
    .foldLeft(ValueSet.empty[X])(_ | _.incidentEdgeValues(vertex))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = ownersOf(vertex)
    .foldLeft(ValueMap.empty[X, M]): (sums, operand) =>
      operand
        .incidentEdges(vertex)
        .foldLeft(sums):
          case (sums, (edge, multiplicity)) => sums.updated(
              edge,
              combine(
                sums.getOrElse(edge, algebra.zero),
                multiplicity,
              ),
            )

  override def find(edge: Any): Option[X] = IncidenceStructure
    .ends(edge)
    .headOption
    .flatMap(ownersOf(_).iterator.flatMap(_.find(edge)).nextOption())

  /** `true` if and only if no two of the graphs share a vertex. */
  protected lazy val disjoint: Boolean = owners.values.forall(_.size == 1)

  /** @return The operands containing [[vertex]]. */
  protected def ownersOf(vertex: Any): Seq[IncidenceStructure[V, M, X]] = owners
    .getOrElse(vertex, Nil)

  /** The operands containing each vertex. */
  private lazy val owners: Map[Any, Seq[IncidenceStructure[V, M, X]]] =
    IncidenceStructure.owners(operands)
