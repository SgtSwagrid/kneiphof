package com.alecdorrington
package kneiphof
package views

/**
  * The changes made to a lazy graph by adding and removing single vertices and
  * edges, stored so that the graph needn't be built.
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
private[kneiphof] final case class Patch[+V, +X, M]
  (
    added: ValueSet[V],
    removed: ValueSet[Any],
    detached: ValueSet[Any],
    overrides: ValueMap[X, M],
    touching: Map[Any, ValueSet[X]],
  ):

  /** `true` if and only if this patch changes nothing. */
  def isEmpty: Boolean = added.isEmpty && detached.isEmpty && overrides.isEmpty

  /** @return This patch, with [[vertex]] added. */
  def withVertex[W >: V](vertex: W): Patch[W, X, M] = copy(
    added = added + vertex,
    removed = removed - vertex,
  )

  /** @return This patch, with [[vertex]] and its edges removed. */
  def withoutVertex(vertex: Any): Patch[V, X, M] =
    val dropped = touching.getOrElse(vertex, ValueSet.empty)
    copy(
      added = added - vertex,
      removed = removed + vertex,
      detached = detached + vertex,
      overrides = dropped.foldLeft(overrides)(_.removed(_)),
      touching = dropped.foldLeft(touching - vertex)(Patch.untouched),
    )

  /**
    * @return
    *   This patch, with the multiplicity of [[edge]] set to [[multiplicity]],
    *   and the vertices [[ends]] added.
    */
  def withEdge[W >: V, Y >: X]
    (
      edge: Y,
      ends: Iterable[W],
      multiplicity: M,
    )
    : Patch[W, Y, M] = ends
    .foldLeft(this: Patch[W, Y, M])(_.withVertex(_))
    .copy(
      overrides = overrides.updated(edge, multiplicity),
      touching = Patch.touched(touching, edge),
    )

/** The companion object for [[Patch]]. */
private[kneiphof] object Patch:

  /** @return The patch which changes nothing. */
  def empty[M]: Patch[Nothing, Nothing, M] = Patch(
    ValueSet.empty,
    ValueSet.empty,
    ValueSet.empty,
    ValueMap.empty,
    Map.empty,
  )

  /** @return [[touching]], with [[edge]] at each of its ends. */
  private def touched[X]
    (touching: Map[Any, ValueSet[X]], edge: X)
    : Map[Any, ValueSet[X]] = Queries
    .ends(edge)
    .foldLeft(touching): (touching, end) =>
      touching.updated(
        end,
        touching.getOrElse(end, ValueSet.empty) + edge,
      )

  /** @return [[touching]], without [[edge]] at any of its ends. */
  private def untouched[X]
    (
      touching: Map[Any, ValueSet[X]],
      edge: Any,
    )
    : Map[Any, ValueSet[X]] = Queries
    .ends(edge)
    .foldLeft(touching): (touching, end) =>
      touching.updatedWith(end)(_.map(_ - edge))

/** The queries about [[base]] after the changes in [[patch]]. */
private[kneiphof] final class Patching[V, E, M : Multiplicity as M, X]
  (
    base: Queries[V, E, M, X],
    patch: Patch[V, X, M],
  )
  extends Queries[V, E, M, X]:

  override lazy val vertexValues: ValueSet[V] = base
    .vertexValues
    .filter(!patch.removed.contains(_)) | patch.added

  override def hasVertex(vertex: Any): Boolean = patch.added.contains(vertex) ||
    base.hasVertex(vertex) && !patch.removed.contains(vertex)

  override def multiplicity(edge: Any): M = patch
    .overrides
    .getOrElse(
      edge,
      if detached(edge) then M.zero else base.multiplicity(edge),
    )

  override def incidentEdgeValues(vertex: Any): ValueSet[X] =
    ValueSet.from(incidentEdges(vertex).map(_(0)))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    original(vertex) ++ changed(vertex)

  override def find(edge: Any): Option[X] = patch.overrides.getKey(edge) match
    case Some(changed) => Option.when(multiplicity(changed).nonZero)(changed)
    case None          => if detached(edge) then None else base.find(edge)

  override def size: Option[Int] = base
    .size
    .map(size => Math.addExact(size, changes(base, affected)))

  override def degree(vertex: Any): Option[Int] = None

  /** @return The original edges at [[vertex]] which the patch left alone. */
  private def original(vertex: Any): Iterable[(X, M)] =
    if patch.detached.contains(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .filter((edge, _) => !detached(edge) && !patch.overrides.contains(edge))

  /** @return The edges at [[vertex]] which the patch changed, if present. */
  private def changed(vertex: Any): Iterable[(X, M)] = patch
    .touching
    .getOrElse(vertex, ValueSet.empty)
    .toSeq
    .map(edge => edge -> patch.overrides.getOrElse(edge, M.zero))
    .filter(_(1).nonZero)

  /** The edges whose multiplicities the patch might have changed. */
  private def affected: ValueSet[Any] = ValueSet.from(patch.overrides.keys) |
    ValueSet.from(patch.detached.flatMap(base.incidentEdgeValues))

  /** @return `true` if and only if an end of [[edge]] has been detached. */
  private def detached(edge: Any): Boolean = Queries
    .ends(edge)
    .exists(patch.detached.contains)
