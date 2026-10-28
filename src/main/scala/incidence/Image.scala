package com.alecdorrington
package kneiphof
package incidence

import scala.util.control.TailCalls.{done, tailcall, TailRec}

/**
  * The image of the graph whose incidence structure is [[base]], with each
  * vertex mapped by [[vertexMap]] and each edge replaced by its [[edgeImages]],
  * whose vertices are the images of its own, summing the multiplicities of
  * equal images. An image of an image is one image, of the composed maps, which
  * are applied in a loop (with [[scala.util.control.TailCalls]]) rather than
  * nested, so that long chains of maps stay shallow.
  *
  * @param kept
  *   `true` if and only if every copy of each edge has exactly one image, and
  *   the multiplicities of equal images add their counts, so that sizes and the
  *   sums of degrees are kept.
  */
private[kneiphof] final class Image[
  V,
  +W,
  M : Multiplicity as M,
  X <: AnyEdge[?, ?, ?],
  +Y <: AnyEdge[?, ?, ?],
] private (
  base: IncidenceStructure[V, M, X],
  vertexMap: V => TailRec[W],
  edgeImages: X => TailRec[Iterable[Y]],
  kept: Boolean,
) extends IncidenceStructure[W, M, Y]:

  override lazy val vertexValues: ValueSet[W] =
    ValueSet.from(base.vertexValues.map(image))

  override def hasVertex(vertex: Any): Boolean = preimages.contains(vertex)

  override def maxOrder: Option[Long] = base.maxOrder

  override def multiplicity(edge: Any): M = candidates(edge).foldLeft(M.zero):
    (sum, original) =>
      sum + base.multiplicity(original).scale(images(original).count(_ == edge))

  override def incidentEdges(vertex: Any): Iterable[(Y, M)] = IncidenceStructure
    .combine(
      ValueMap
        .from(preimagesOf(vertex).iterator.flatMap(base.incidentEdges))
        .iterator
        .flatMap: (original, multiplicity) =>
          images(original)
            .iterator
            .filter(_.vertexValues.contains(vertex))
            .map(_ -> multiplicity),
    )(_ + _)

  override lazy val size: Int = if kept then base.size else super.size

  override def degree(vertex: Any): Int =
    if !kept then super.degree(vertex)
    else IncidenceStructure.total(preimagesOf(vertex).iterator.map(base.degree))

  /**
    * @return
    *   This image, with each vertex then mapped by [[nextVertexMap]], and each
    *   edge then replaced by its [[nextEdgeImages]].
    */
  def compose[U, Z <: AnyEdge[?, ?, ?]]
    (
      nextVertexMap: W => U,
      nextEdgeImages: Y => Iterable[Z],
      nextKept: Boolean,
    )
    : Image[V, U, M, X, Z] = Image(
    base,
    vertex => tailcall(vertexMap(vertex)).map(nextVertexMap),
    edge => tailcall(edgeImages(edge)).map(_.flatMap(nextEdgeImages)),
    kept && nextKept,
  )

  /** @return The image of [[vertex]]. */
  private def image(vertex: V): W = vertexMap(vertex).result

  /** @return The images of [[edge]]. */
  private def images(edge: X): Iterable[Y] = edgeImages(edge).result

  /** The vertices of [[base]] mapped to each vertex. */
  private lazy val preimages: Map[Any, ValueSet[V]] = base
    .vertexValues
    .toSeq
    .groupMapReduce[Any, ValueSet[V]](image)(ValueSet(_))(_ | _)

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

/** The companion object for [[Image]]. */
private[kneiphof] object Image:

  /**
    * @return
    *   The image of the graph whose incidence structure is [[base]], with each
    *   vertex mapped by [[vertexMap]] and each edge replaced by its
    *   [[edgeImages]], composing the maps if [[base]] is already an image.
    */
  def of[
    V,
    W,
    M : Multiplicity,
    X <: AnyEdge[?, ?, ?],
    Y <: AnyEdge[?, ?, ?],
  ]
    (
      base: IncidenceStructure[V, M, X],
      vertexMap: V => W,
      edgeImages: X => Iterable[Y],
      kept: Boolean,
    )
    : IncidenceStructure[W, M, Y] = base match
    case base: Image[?, V, M, ?, X] => base.compose(vertexMap, edgeImages, kept)
    case _                          => Image(
        base,
        vertex => done(vertexMap(vertex)),
        edge => done(edgeImages(edge)),
        kept,
      )
