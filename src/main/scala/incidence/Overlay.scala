package com.alecdorrington
package kneiphof
package incidence

/**
  * The overlay of the graphs whose incidence structures are [[operands]], with
  * every vertex and edge of any of them, and each edge's multiplicities
  * combined by [[combine]]: a [[Sum]] or a [[Union]]. Overlays of overlays of
  * the same operation merge into one, so that long chains of them stay shallow.
  *
  * Each query only asks the operands containing the vertices it's about. The
  * operands with few vertices are indexed by them, and the others are asked
  * directly, so that a large operand's vertices are never listed just to find
  * out which operands have a vertex.
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

  override def hasVertex(vertex: Any): Boolean = ownersOf(vertex).nonEmpty

  override def vertexValue(vertex: Any): Option[V] = ownersOf(vertex)
    .iterator
    .flatMap(_.vertexValue(vertex))
    .nextOption()

  override def order: Int =
    if disjoint then IncidenceStructure.total(operands.map(_.order))
    else vertexValues.size

  override def multiplicity(edge: Any): M = IncidenceStructure
    .ends(edge)
    .headOption
    .fold(algebra.zero): end =>
      ownersOf(end).foldLeft(algebra.zero): (sum, operand) =>
        combine(sum, operand.multiplicity(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = ownersOf(vertex)
    .foldLeft(ValueSet.empty[X])(_ | _.incidentEdgeValues(vertex))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = IncidenceStructure
    .combined(
      ownersOf(vertex).iterator.flatMap(_.incidentEdges(vertex)),
    )(combine)

  override def find(edge: Any): Option[X] = IncidenceStructure
    .ends(edge)
    .headOption
    .flatMap(ownersOf(_).iterator.flatMap(_.find(edge)).nextOption())

  /** `true` if and only if no two of the graphs share a vertex. */
  protected lazy val disjoint: Boolean = indexed.values.forall(_.size == 1) &&
    indexed.keysIterator.forall(vertex => !large.exists(_.hasVertex(vertex))) &&
    large
      .combinations(2)
      .forall:
        case Seq(first, second) =>
          val (smaller, larger) =
            if first.order <= second.order then (first, second)
            else (second, first)
          smaller.vertexValues.forall(!larger.hasVertex(_))
        case _ => true

  /** @return The operands containing [[vertex]]. */
  protected def ownersOf(vertex: Any): Seq[IncidenceStructure[V, M, X]] =
    indexed.getOrElse(vertex, Nil) ++ large.filter(_.hasVertex(vertex))

  /** The operands with few enough vertices to index, and the others. */
  private lazy val (small, large) =
    operands.partition(_.order <= IncidenceStructure.indexable)

  /** The operands with few vertices containing each vertex. */
  private lazy val indexed: Map[Any, Seq[IncidenceStructure[V, M, X]]] =
    IncidenceStructure.owners(small)

/** The companion object for [[Overlay]]. */
private[kneiphof] object Overlay:

  /**
    * @return
    *   The operands of an overlay of [[first]] and [[second]], taking the
    *   operands of either which is already an overlay of the same kind, as
    *   given by [[operandsOf]].
    */
  def merged[S](first: S, second: S)(operandsOf: S => Option[Seq[S]]): Seq[S] =
    Seq(first, second).flatMap(structure =>
      operandsOf(structure).getOrElse(Seq(structure)),
    )
