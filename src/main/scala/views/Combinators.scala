package com.alecdorrington
package kneiphof
package views

/**
  * The queries about [[graph]], with its multiplicities read in [[M]] by
  * [[embed]], answered by [[graph]] itself.
  */
private[kneiphof] final class Viewing[
  V,
  E,
  N,
  M : Multiplicity,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, N, X, G],
]
  (graph: G[V, E], embed: Embedding[N, M])
  extends Queries[V, E, M, X[V, E]]:

  override def vertexValues: ValueSet[V] = graph.vertexValues

  override def hasVertex(vertex: Any): Boolean = graph.hasVertex(vertex)

  override def multiplicity(edge: Any): M = embed(graph.multiplicity(edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E]] = graph
    .incidentEdgeValues(vertex)

  override def size: Option[Int] = Option.when(embed.exact)(graph.size)

  override def degree(vertex: Any): Option[Int] =
    Option.when(embed.exact)(graph.degree(vertex))

/**
  * The queries about the union or sum of the graphs described by [[first]] and
  * [[second]], whose multiplicities are combined by [[combine]].
  *
  * @param edges
  *   The edges of [[second]], which are the only ones whose multiplicities
  *   differ from those in [[first]].
  */
private[kneiphof] final class Combining[V, E, M : Multiplicity, X]
  (
    first: Queries[V, E, M, X],
    second: Queries[V, E, M, X],
    combine: (M, M) => M,
    edges: => Iterable[Any],
  )
  extends Queries[V, E, M, X]:

  override lazy val vertexValues: ValueSet[V] = first.vertexValues |
    second.vertexValues

  override def hasVertex(vertex: Any): Boolean = first.hasVertex(vertex) ||
    second.hasVertex(vertex)

  override def multiplicity(edge: Any): M = combine(
    first.multiplicity(edge),
    second.multiplicity(edge),
  )

  override def incidentEdgeValues(vertex: Any): ValueSet[X] = first
    .incidentEdgeValues(vertex) | second.incidentEdgeValues(vertex)

  override def size: Option[Int] = (first.size, second.size) match
    case (Some(first), Some(second)) if disjoint => Some(first + second)
    case (Some(size), _) => Some(size + changes(first, edges))
    case _               => None

  override def degree(vertex: Any): Option[Int] =
    if !disjoint then None
    else if first.hasVertex(vertex) then first.degree(vertex)
    else second.degree(vertex)

  /** `true` if and only if the two graphs share no vertex. */
  private lazy val disjoint: Boolean =
    !first.vertexValues.exists(second.hasVertex)

/**
  * The queries about the subgraph of the graph described by [[base]] with only
  * the vertices satisfying [[keep]], and each remaining edge's multiplicity
  * changed by [[reweigh]].
  *
  * @param changed
  *   The edges which [[reweigh]] might change.
  */
private[kneiphof] final class Refining[V, E, M : Multiplicity as M, X]
  (
    base: Queries[V, E, M, X],
    keep: V => Boolean,
    reweigh: (Any, M) => M,
    changed: => Iterable[Any],
  )
  extends Queries[V, E, M, X]:

  override lazy val vertexValues: ValueSet[V] = base.vertexValues.filter(keep)

  override def hasVertex(vertex: Any): Boolean = base
    .vertexValues
    .get(vertex)
    .exists(keep)

  override def multiplicity(edge: Any): M =
    if Queries.ends(edge).forall(hasVertex) then
      reweigh(edge, base.multiplicity(edge))
    else M.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[X] =
    if hasVertex(vertex) then
      base.incidentEdgeValues(vertex).filter(multiplicity(_).nonZero)
    else ValueSet.empty

  /**
    * The number of edges, counted from the edges which might have changed:
    * those [[reweigh]] might change, and those at the removed vertices.
    */
  override def size: Option[Int] = base
    .size
    .map: size =>
      val removed = base.vertexValues.filterNot(keep)
      size + changes(
        base,
        changed ++ removed.flatMap(base.incidentEdgeValues),
      )

  override def degree(vertex: Any): Option[Int] = None

/**
  * The queries about the image of the graph described by [[base]], with each
  * vertex mapped by [[vertexMap]] and each edge replaced by its [[edgeImages]],
  * whose vertices are the images of its own.
  *
  * @param exact
  *   `true` if and only if the images neither merge vertices nor change the
  *   number of copies of any edge, so that sizes and degrees are unchanged.
  */
private[kneiphof] final class Imaging[V, E, W, F, M : Multiplicity as M, X, Y]
  (
    base: Queries[V, E, M, X],
    vertexMap: V => W,
    edgeImages: X => Iterable[Y],
    exact: Boolean,
  )
  extends Queries[W, F, M, Y]:

  override lazy val vertexValues: ValueSet[W] =
    ValueSet.from(base.vertexValues.map(vertexMap))

  override def hasVertex(vertex: Any): Boolean = preimages.contains(vertex)

  override def multiplicity(edge: Any): M = candidates(edge).foldLeft(M.zero):
    (sum, original) =>
      sum +
        base.multiplicity(original).scale(edgeImages(original).count(_ == edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[Y] = ValueSet.from:
    preimagesOf(vertex)
      .flatMap(base.incidentEdgeValues)
      .flatMap(edgeImages)
      .filter(Queries.ends(_).contains(vertex))

  override def size: Option[Int] = if exact then base.size else None

  override def degree(vertex: Any): Option[Int] =
    if exact then preimagesOf(vertex).headOption.flatMap(base.degree) else None

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
    val sources = ValueSet.from(Queries.ends(edge).flatMap(preimagesOf))
    ValueSet.from:
      sources
        .iterator
        .flatMap(base.incidentEdgeValues)
        .filter(Queries.ends(_).forall(sources.contains))
