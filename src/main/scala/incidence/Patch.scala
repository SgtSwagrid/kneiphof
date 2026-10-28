package com.alecdorrington
package kneiphof
package incidence

/**
  * The incidence structure of [[base]] after single vertices and edges are
  * added and removed, which records the changes rather than nesting a structure
  * for each, so that long chains of updates stay shallow.
  *
  * @param added
  *   The vertices added since.
  * @param removed
  *   The vertices removed since, which are absent unless added again.
  * @param detached
  *   The vertices removed at some point, whose original edges are gone.
  * @param overrides
  *   The new multiplicity of each edge changed since.
  * @param touching
  *   The edges in [[overrides]] at each vertex.
  */
private[kneiphof] final class Patch[
  +V,
  M : Multiplicity as M,
  +X <: AnyEdge[?, ?, ?],
] private (
  base: IncidenceStructure[V, M, X],
  added: ValueSet[V],
  removed: ValueSet[Any],
  detached: ValueSet[Any],
  overrides: ValueMap[X, M],
  touching: Map[Any, ValueSet[X]],
) extends IncidenceStructure[V, M, X]:

  override lazy val vertexValues: ValueSet[V] = base
    .vertexValues
    .filter(!removed.contains(_)) | added

  override def hasVertex(vertex: Any): Boolean = added.contains(vertex) ||
    base.hasVertex(vertex) && !removed.contains(vertex)

  override def vertexValue(vertex: Any): Option[V] = added
    .get(vertex)
    .orElse(
      Option.unless(removed.contains(vertex))(base.vertexValue(vertex)).flatten,
    )

  override def knownOrder: Option[Long] = base
    .knownOrder
    .map(_ + added.count(!base.hasVertex(_)) - removed.count(base.hasVertex))

  override def maxOrder: Option[Long] =
    IncidenceStructure.knownTotal(Seq(base.maxOrder, Some(added.size)))

  override def vertexIterator: Iterator[V] = base
    .vertexIterator
    .filter(vertex => !removed.contains(vertex) && !added.contains(vertex)) ++
    added.iterator

  override def order: Int = IncidenceStructure.total:
    Iterator(
      base.order,
      added.count(!base.hasVertex(_)),
      -removed.count(base.hasVertex),
    )

  override def multiplicity(edge: Any): M = overrides.getOrElse(
    edge,
    if isDetached(edge) then M.zero else base.multiplicity(edge),
  )

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    original(vertex) ++ changed(vertex)

  override def find(edge: Any): Option[X] = overrides.getKey(edge) match
    case Some(changed) => Option.when(multiplicity(changed).nonZero)(changed)
    case None          => if isDetached(edge) then None else base.find(edge)

  override def size: Int = Math.addExact(
    base.size,
    IncidenceStructure.changes(this, base, affected),
  )

  override def listable: Boolean = base.listable

  override def patch: Patch[V, M, X] = this

  /** @return This structure with [[vertex]] added. */
  def withVertex[W >: V](vertex: W): Patch[W, M, X] = new Patch(
    base,
    added + vertex,
    removed - vertex,
    detached,
    overrides,
    touching,
  )

  /** @return This structure without [[vertex]] and its edges. */
  def withoutVertex(vertex: Any): Patch[V, M, X] =
    val dropped = touching.getOrElse(vertex, ValueSet.empty)
    new Patch(
      base,
      added - vertex,
      removed + vertex,
      detached + vertex,
      dropped.foldLeft(overrides)(_.without(_)),
      dropped.foldLeft(touching - vertex)(Patch.untouch),
    )

  /**
    * @return
    *   This structure with the multiplicity of [[edge]] set to
    *   [[multiplicity]], and the vertices [[ends]] added.
    */
  def withEdge[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (
      edge: Y,
      ends: Iterable[W],
      multiplicity: M,
    )
    : Patch[W, M, Y] = new Patch(
    base,
    added | ValueSet.from(ends),
    ends.foldLeft(removed)(_ - _),
    detached,
    overrides.withEntry(edge, multiplicity),
    Patch.touch(touching, edge),
  )

  /** @return The original edges at [[vertex]] which this patch left alone. */
  private def original(vertex: Any): Iterable[(X, M)] =
    if detached.contains(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .filter((edge, _) => !isDetached(edge) && !overrides.contains(edge))

  /** @return The edges at [[vertex]] which this patch changed, if present. */
  private def changed(vertex: Any): Iterable[(X, M)] = touching
    .getOrElse(vertex, ValueSet.empty)
    .toSeq
    .map(edge => edge -> overrides.getOrElse(edge, M.zero))
    .filter((_, multiplicity) => multiplicity.nonZero)

  /** The edges whose multiplicities this patch might have changed. */
  private def affected: ValueSet[Any] = ValueSet.from(overrides.keys) |
    ValueSet.from(detached.flatMap(base.incidentEdgeValues))

  /** @return `true` if and only if an end of [[edge]] has been detached. */
  private def isDetached(edge: Any): Boolean = IncidenceStructure
    .ends(edge)
    .exists(detached.contains)

/** The companion object for [[Patch]]. */
private[kneiphof] object Patch:

  /** @return The patch of [[base]] which changes nothing. */
  def apply[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (base: IncidenceStructure[V, M, X])
    : Patch[V, M, X] = new Patch(
    base,
    ValueSet.empty,
    ValueSet.empty,
    ValueSet.empty,
    ValueMap.empty,
    Map.empty,
  )

  /** @return The map [[touching]], with [[edge]] at each of its ends. */
  private def touch[X]
    (touching: Map[Any, ValueSet[X]], edge: X)
    : Map[Any, ValueSet[X]] = IncidenceStructure
    .ends(edge)
    .foldLeft(touching): (touching, end) =>
      touching.updated(
        end,
        touching.getOrElse(end, ValueSet.empty) + edge,
      )

  /** @return The map [[touching]], without [[edge]] at any of its ends. */
  private def untouch[X]
    (
      touching: Map[Any, ValueSet[X]],
      edge: Any,
    )
    : Map[Any, ValueSet[X]] = IncidenceStructure
    .ends(edge)
    .foldLeft(touching): (touching, end) =>
      touching.updatedWith(end)(_.map(_ - edge))
