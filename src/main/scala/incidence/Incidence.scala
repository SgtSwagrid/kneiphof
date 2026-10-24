package com.alecdorrington
package kneiphof
package incidence

/**
  * A persistent store of the vertices [[V]], the edges [[X]] with their
  * multiplicities, and the edges incident to each vertex.
  *
  * Everything is stored in hash tries, so each update touches only the edge and
  * its ends, in effectively constant time per end.
  */
private[kneiphof] final case class Incidence[+V, +X, M]
  (
    vertices: ValueSet[V],
    incident: Map[Any, ValueSet[X]],
    multiplicities: ValueMap[X, M],
  ):

  /** @return This store with [[vertex]] added, if it's not already present. */
  def withVertex[W >: V](vertex: W): Incidence[W, X, M] =
    copy(vertices = vertices + vertex)

  /** @return This store with all of [[vertices]] added. */
  def withVertices[W >: V](vertices: Iterable[W]): Incidence[W, X, M] = vertices
    .foldLeft(this: Incidence[W, X, M])(_.withVertex(_))

  /**
    * @return
    *   This store without [[vertex]] and its incident edges, whose ends are
    *   given by [[ends]].
    */
  def withoutVertex(vertex: Any)(ends: X => Iterable[Any]): Incidence[V, X, M] =
    val detached = incident
      .getOrElse(vertex, ValueSet.empty)
      .foldLeft(this)((store, edge) => store.withoutEdge(edge, ends(edge)))
    detached.copy(vertices = detached.vertices - vertex)

  /**
    * @return
    *   This store with [[edge]] at the given [[multiplicity]], incident to
    *   [[ends]], which are added as vertices if necessary.
    */
  def withEdge[W >: V, Y >: X]
    (
      edge: Y,
      ends: Iterable[W],
      multiplicity: M,
    )
    : Incidence[W, Y, M] = Incidence(
    ends.foldLeft(vertices: ValueSet[W])(_ + _),
    ends.foldLeft(incident: Map[Any, ValueSet[Y]]): (incident, vertex) =>
      incident.updated(
        vertex,
        incident.getOrElse(vertex, ValueSet.empty) + edge,
      ),
    multiplicities.updated(edge, multiplicity),
  )

  /** @return This store without [[edge]], which is incident to [[ends]]. */
  def withoutEdge(edge: Any, ends: Iterable[Any]): Incidence[V, X, M] = copy(
    incident = ends.foldLeft(incident): (incident, vertex) =>
      incident.updatedWith(vertex)(_.map(_ - edge).filter(_.nonEmpty)),
    multiplicities = multiplicities.removed(edge),
  )

/** The companion object for [[Incidence]]. */
private[kneiphof] object Incidence:

  /** @return The store with no vertices or edges. */
  def empty[M]: Incidence[Nothing, Nothing, M] = Incidence(
    ValueSet.empty,
    Map.empty,
    ValueMap.empty,
  )
