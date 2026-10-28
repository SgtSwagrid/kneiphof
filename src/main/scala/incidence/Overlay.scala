package com.alecdorrington
package kneiphof
package incidence

/**
  * The overlay of the graphs whose incidence structures are [[operands]], with
  * every vertex and edge of any of them, and each edge's multiplicities
  * combined by [[combine]]: a [[Sum]] or a [[Union]]. Overlays of overlays of
  * the same operation merge into one, so that long chains of them stay shallow.
  *
  * Each query only asks the operands containing the vertices it's about (see
  * [[Operands]]). A few stored edges at a time are gathered into [[gathered]],
  * so that adding them one at a time keeps the other operands, and their index.
  * Another operand comes after them, so of equal vertices, the overlay keeps
  * the last operand's.
  */
private[kneiphof] trait Overlay[+V, M, +X <: AnyEdge[?, ?, ?]]
  extends IncidenceStructure[V, M, X]:

  /** The graphs overlaid, other than those gathered into [[gathered]]. */
  def parts: Operands[V, M, X]

  /**
    * The few stored graphs overlaid, gathered into one structure, which counts
    * as the last operand.
    */
  def gathered: Patch[V, M, X]

  /**
    * @return
    *   This overlay, with its gathered edges replaced by [[gathered]], which
    *   includes them.
    */
  def withGathered[W >: V, Y >: X <: AnyEdge[?, ?, ?]](gathered: Patch[W, M, Y])
    : Overlay[W, M, Y]

  /** The incidence structures of the graphs overlaid, in order. */
  def operands: Seq[IncidenceStructure[V, M, X]] =
    if gathered.vertexValues.isEmpty then parts.all else parts.all :+ gathered

  /**
    * @return
    *   The multiplicity of an edge with multiplicities [[x]] and [[y]].
    */
  protected def combine(x: M, y: M): M

  override lazy val vertexValues: ValueSet[V] =
    operands.foldLeft(ValueSet.empty[V])(_ | _.vertexValues)

  override def hasVertex(vertex: Any): Boolean = gathered.hasVertex(vertex) ||
    parts.contain(vertex)

  override def vertexValue(vertex: Any): Option[V] = ownersOf(vertex)
    .lastOption
    .flatMap(_.vertexValue(vertex))

  override def order: Int =
    if disjoint then IncidenceStructure.total(operands.map(_.order))
    else vertexValues.size

  override def maxOrder: Option[Long] =
    IncidenceStructure.knownTotal(operands.map(_.maxOrder))

  override def multiplicity(edge: Any): M = IncidenceStructure
    .ends(edge)
    .headOption
    .fold(algebra.zero): end =>
      candidatesAt(end).foldLeft(algebra.zero): (sum, operand) =>
        combine(sum, operand.multiplicity(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = ownersOf(vertex)
    .foldLeft(ValueSet.empty[X])(_ | _.incidentEdgeValues(vertex))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] = IncidenceStructure
    .combine(
      ownersOf(vertex).iterator.flatMap(_.incidentEdges(vertex)),
    )(combine)

  override def find(edge: Any): Option[X] = IncidenceStructure
    .ends(edge)
    .headOption
    .flatMap(candidatesAt(_).reverseIterator.flatMap(_.find(edge)).nextOption())

  /** `true` if and only if no two of the graphs share a vertex. */
  protected lazy val disjoint: Boolean = parts.disjoint &&
    gathered.vertexValues.forall(!parts.contain(_))

  /** @return The operands containing [[vertex]], in order. */
  protected def ownersOf(vertex: Any): Seq[IncidenceStructure[V, M, X]] =
    val containing = parts.containing(vertex)
    if gathered.hasVertex(vertex) then containing :+ gathered else containing

  /**
    * @return
    *   The operands which may have an edge at [[vertex]], in order (see
    *   [[Operands.candidates]]).
    */
  protected def candidatesAt(vertex: Any): Seq[IncidenceStructure[V, M, X]] =
    val candidates = parts.candidates(vertex)
    if gathered.hasVertex(vertex) then candidates :+ gathered else candidates

/** The companion object for [[Overlay]]. */
private[kneiphof] object Overlay:

  /**
    * @return
    *   The parts and gathered edges of an overlay of [[first]] and [[second]],
    *   taking the operands of either which is already an overlay of the same
    *   kind, as [[overlayOf]] tells, in order. So the gathered edges of each
    *   become a part, before the operands which follow them.
    */
  def merge[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (
      first: IncidenceStructure[V, M, X],
      second: IncidenceStructure[V, M, X],
    )
    (overlayOf: IncidenceStructure[V, M, X] => Option[Overlay[V, M, X]])
    : (Operands[V, M, X], Patch[V, M, X]) =
    def partsOf(structure: IncidenceStructure[V, M, X]): Operands[V, M, X] =
      overlayOf(structure).fold(Operands.of(structure))(overlay =>
        if overlay.gathered.vertexValues.isEmpty then overlay.parts
        else overlay.parts.append(overlay.gathered),
      )
    (partsOf(first).concat(partsOf(second)), Patch(Explicit.empty[M]))
