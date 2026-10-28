package com.alecdorrington
package kneiphof
package incidence

/**
  * An incidence structure which stores its vertices, its edges with their
  * multiplicities, and the edges incident to each vertex. Graphs built from
  * their edges and materialised graphs are explicit.
  *
  * Everything is stored in hash tries, so each update touches only the edge and
  * its ends, in effectively constant time per end.
  */
private[kneiphof] final class Explicit[
  +V,
  M : Multiplicity as M,
  +X <: AnyEdge[?, ?, ?],
] private (
  override val vertexValues: ValueSet[V],
  incident: Map[Any, ValueSet[X]],
  multiplicities: ValueMap[X, M],
) extends IncidenceStructure[V, M, X]:

  override def hasVertex(vertex: Any): Boolean = vertexValues.contains(vertex)

  override def knownOrder: Option[Long] = Some(vertexValues.size)

  override def multiplicity(edge: Any): M =
    multiplicities.getOrElse(edge, M.zero)

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    incidentEdgeValues(vertex).toSeq.map(edge => edge -> multiplicity(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = incident
    .getOrElse(vertex, ValueSet.empty)

  override def find(edge: Any): Option[X] = multiplicities.getKey(edge)

  override def edges: Iterator[(X, M)] = multiplicities.iterator

  override lazy val size: Int =
    IncidenceStructure.total(multiplicities.values.map(_.count))

  override def stored: Boolean = true

  override def explicit: Explicit[V, M, X] = this

  /**
    * @return
    *   This structure with [[vertex]] added, if it's not already there.
    */
  def withVertex[W >: V](vertex: W): Explicit[W, M, X] = Explicit(
    vertexValues + vertex,
    incident,
    multiplicities,
  )

  /** @return This structure with all of [[vertices]] added. */
  def withVertices[W >: V](vertices: Iterable[W]): Explicit[W, M, X] = vertices
    .foldLeft(this: Explicit[W, M, X])(_.withVertex(_))

  /**
    * @return
    *   This structure with the multiplicity of [[edge]] set to
    *   [[multiplicity]], adding its [[ends]] as vertices unless it's now
    *   absent.
    */
  def withEdge[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (
      edge: Y,
      ends: Iterable[W],
      multiplicity: M,
    )
    : Explicit[W, M, Y] =
    if multiplicity.isZero then withoutEdge(edge)
    else
      Explicit(
        ends.foldLeft(vertexValues: ValueSet[W])(_ + _),
        ends.foldLeft(incident: Map[Any, ValueSet[Y]]): (incident, vertex) =>
          incident.updated(
            vertex,
            incident.getOrElse(vertex, ValueSet.empty) + edge,
          ),
        multiplicities.withEntry(edge, multiplicity),
      )

  /**
    * @return
    *   This structure with [[multiplicity]] added to that of [[edge]], adding
    *   its [[ends]] as vertices.
    */
  def increase[W >: V, Y >: X <: AnyEdge[?, ?, ?]]
    (
      edge: Y,
      ends: Iterable[W],
      multiplicity: M,
    )
    : Explicit[W, M, Y] = withEdge(
    edge,
    ends,
    this.multiplicity(edge) + multiplicity,
  )

  /** @return This structure without [[edge]]. */
  private def withoutEdge(edge: Any): Explicit[V, M, X] = Explicit(
    vertexValues,
    IncidenceStructure
      .ends(edge)
      .foldLeft(incident): (incident, vertex) =>
        incident.updatedWith(vertex)(_.map(_ - edge).filter(_.nonEmpty)),
    multiplicities.without(edge),
  )

/** The companion object for [[Explicit]]. */
private[kneiphof] object Explicit:

  /** @return The structure with no vertices or edges. */
  def empty[M : Multiplicity]: Explicit[Nothing, M, Nothing] = Explicit(
    ValueSet.empty,
    Map.empty,
    ValueMap.empty,
  )

  /** @return The structure with every vertex and edge of [[structure]]. */
  def from[V, M : Multiplicity, X <: AnyEdge[?, ?, ?]]
    (structure: IncidenceStructure[V, M, X])
    : Explicit[V, M, X] = Explicit(
    structure.vertexValues,
    structure
      .vertexValues
      .iterator
      .map(vertex => (vertex: Any) -> structure.incidentEdgeValues(vertex))
      .filter((_, edges) => edges.nonEmpty)
      .toMap,
    ValueMap.from(structure.edges),
  )
