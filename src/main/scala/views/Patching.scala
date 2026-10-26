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
  */
private[kneiphof] final case class Patch[+V, +X, M]
  (
    added: ValueSet[V],
    removed: ValueSet[Any],
    detached: ValueSet[Any],
    overrides: ValueMap[X, M],
  ):

  /** @return This patch, with [[vertex]] added. */
  def withVertex[W >: V](vertex: W): Patch[W, X, M] = copy(
    added = added + vertex,
    removed = removed - vertex,
  )

  /** @return This patch, with [[vertex]] and its edges removed. */
  def withoutVertex(vertex: Any): Patch[V, X, M] = copy(
    added = added - vertex,
    removed = removed + vertex,
    detached = detached + vertex,
    overrides = overrides
      .keys
      .filter(Queries.ends(_).contains(vertex))
      .foldLeft(overrides)(_.removed(_)),
  )

  /**
    * @return
    *   This patch, with the multiplicity of [[edge]] set to [[multiplicity]],
    *   and its ends added.
    */
  def withEdge[W >: V, Y >: X]
    (
      edge: Y,
      ends: Iterable[W],
      multiplicity: M,
    )
    : Patch[W, Y, M] = ends
    .foldLeft(this: Patch[W, Y, M])(_.withVertex(_))
    .copy(overrides = overrides.updated(edge, multiplicity))

/** The companion object for [[Patch]]. */
private[kneiphof] object Patch:

  /** @return The patch which changes nothing. */
  def empty[M]: Patch[Nothing, Nothing, M] = Patch(
    ValueSet.empty,
    ValueSet.empty,
    ValueSet.empty,
    ValueMap.empty,
  )

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
    val original =
      if patch.detached.contains(vertex) then ValueSet.empty
      else
        base
          .incidentEdgeValues(vertex)
          .filter(edge => !detached(edge) && !patch.overrides.contains(edge))
    val changed = patch
      .overrides
      .collect:
        case (edge, multiplicity)
          if multiplicity.nonZero && Queries.ends(edge).contains(vertex) => edge
    original | ValueSet.from(changed)

  override def size: Option[Int] = base.size.map(_ + changes(base, affected))

  override def degree(vertex: Any): Option[Int] = None

  override def patched[W >: V, Y >: X]
    (change: Patch[W, Y, M] => Patch[W, Y, M])
    (using Multiplicity[M])
    : Queries[W, E, M, Y] = Patching(base, change(patch))

  /** The edges whose multiplicities the patch might have changed. */
  private def affected: ValueSet[X] = ValueSet.from(patch.overrides.keys) |
    ValueSet.from(patch.detached.flatMap(base.incidentEdgeValues))

  /** @return `true` if and only if an end of [[edge]] has been detached. */
  private def detached(edge: Any): Boolean = Queries
    .ends(edge)
    .exists(patch.detached.contains)
