package com.alecdorrington
package kneiphof
package incidence

/**
  * The image of the graph whose incidence structure is [[base]], with each
  * vertex mapped by [[vertexMap]] and each edge replaced by its [[edgeImages]],
  * whose vertices are the images of its own, summing the multiplicities of
  * equal images.
  *
  * @param kept
  *   `true` if and only if every copy of each edge has exactly one image, and
  *   the multiplicities of equal images add their counts, so that sizes and the
  *   sums of degrees are kept.
  */
private[kneiphof] final class Image[
  V,
  W,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
  Y <: AnyEdge[?, ?, ?],
]
  (
    base: IncidenceStructure[V, M, X],
    vertexMap: V => W,
    edgeImages: X => Iterable[Y],
    kept: Boolean,
  )
  extends IncidenceStructure[W, M, Y]:

  override lazy val vertexValues: ValueSet[W] =
    ValueSet.from(base.vertexValues.map(vertexMap))

  override def hasVertex(vertex: Any): Boolean = preimages.contains(vertex)

  override def multiplicity(edge: Any): M = candidates(edge).foldLeft(M.zero):
    (sum, original) =>
      sum +
        base.multiplicity(original).scale(edgeImages(original).count(_ == edge))

  override def incidentEdges(vertex: Any): Iterable[(Y, M)] = ValueMap
    .from(preimagesOf(vertex).iterator.flatMap(base.incidentEdges))
    .iterator
    .flatMap: (original, multiplicity) =>
      edgeImages(original)
        .iterator
        .filter(_.vertexValues.contains(vertex))
        .map(_ -> multiplicity)
    .foldLeft(ValueMap.empty[Y, M]):
      case (sums, (image, multiplicity)) => sums.updated(
          image,
          sums.getOrElse(image, M.zero) + multiplicity,
        )

  override lazy val size: Int = if kept then base.size else super.size

  override def degree(vertex: Any): Int =
    if !kept then super.degree(vertex)
    else IncidenceStructure.total(preimagesOf(vertex).iterator.map(base.degree))

  /** The vertices of [[base]] mapped to each vertex. */
  private lazy val preimages: Map[Any, ValueSet[V]] = base
    .vertexValues
    .toSeq
    .groupMapReduce[Any, ValueSet[V]](vertexMap)(ValueSet(_))(_ | _)

  /** @return The vertices of [[base]] mapped to [[vertex]]. */
  private def preimagesOf(vertex: Any): ValueSet[V] =
    preimages.getOrElse(vertex, ValueSet.empty)

  /** @return The edges of [[base]] whose images might include [[edge]]. */
  private def candidates(edge: Any): Iterable[X] =
    val sources =
      ValueSet.from(IncidenceStructure.ends(edge).flatMap(preimagesOf))
    ValueSet.from:
      sources
        .iterator
        .flatMap(base.incidentEdgeValues)
        .filter(_.vertexValues.forall(sources.contains))
