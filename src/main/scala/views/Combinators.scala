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
  * @param additive
  *   `true` if and only if combining adds the counts of the multiplicities, so
  *   that the sizes add.
  * @param firstEdges
  *   The edges of [[first]].
  * @param secondEdges
  *   The edges of [[second]].
  */
private[kneiphof] final class Combining[V, E, M : Multiplicity as M, X]
  (
    first: Queries[V, E, M, X],
    second: Queries[V, E, M, X],
    combine: (M, M) => M,
    additive: Boolean,
    firstEdges: => Iterable[Any],
    secondEdges: => Iterable[Any],
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

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    val firsts  = ValueMap.from(first.incidentEdges(vertex))
    val seconds = ValueMap.from(second.incidentEdges(vertex))
    (firsts.keys ++ seconds.keys.filterNot(firsts.contains)).map: edge =>
      edge -> combine(
        firsts.getOrElse(edge, M.zero),
        seconds.getOrElse(edge, M.zero),
      )

  override def find(edge: Any): Option[X] = first
    .find(edge)
    .orElse(second.find(edge))

  /**
    * The number of edges, counted from the sizes of the two graphs and the
    * edges of the smaller one, which are the only ones whose multiplicities can
    * differ from the larger.
    */
  override def size: Option[Int] = (first.size, second.size) match
    case (Some(first), Some(second)) if additive || disjoint =>
      Some(Math.addExact(first, second))
    case (Some(firstSize), secondSize) if secondSize.forall(_ <= firstSize) =>
      Some(Math.addExact(firstSize, changes(first, secondEdges)))
    case (_, Some(secondSize)) =>
      Some(Math.addExact(secondSize, changes(second, firstEdges)))
    case _ => None

  override def degree(vertex: Any): Option[Int] =
    if !disjoint then None
    else if first.hasVertex(vertex) then first.degree(vertex)
    else second.degree(vertex)

  /** `true` if and only if the two graphs share no vertex. */
  private lazy val disjoint: Boolean =
    !first.vertexValues.exists(second.hasVertex)

/**
  * The queries about the subgraph of the graph described by [[base]] with only
  * the vertices satisfying [[keep]] and the edges satisfying [[keepEdge]], and
  * each remaining edge's multiplicity changed by [[reweigh]].
  *
  * @param changed
  *   The edges which [[keepEdge]] or [[reweigh]] might change.
  * @param support
  *   The edges which might remain, if it's quicker to count them than the
  *   changes.
  */
private[kneiphof] final class Refining[V, E, M : Multiplicity as M, X]
  (
    base: Queries[V, E, M, X],
    keep: V => Boolean,
    keepEdge: Option[X => Boolean],
    reweigh: (Any, M) => M,
    changed: => Iterable[Any],
    support: Option[() => Iterable[Any]],
  )
  extends Queries[V, E, M, X]:

  override lazy val vertexValues: ValueSet[V] = base.vertexValues.filter(keep)

  override def hasVertex(vertex: Any): Boolean = base
    .vertexValues
    .get(vertex)
    .exists(keep)

  override def multiplicity(edge: Any): M =
    val multiplicity =
      if Queries.ends(edge).forall(hasVertex) then
        reweigh(edge, base.multiplicity(edge))
      else M.zero
    keepEdge match
      case Some(predicate)
        if multiplicity.nonZero && !base.find(edge).exists(predicate) => M.zero
      case _ => multiplicity

  override def incidentEdgeValues(vertex: Any): ValueSet[X] =
    ValueSet.from(incidentEdges(vertex).map(_(0)))

  override def incidentEdges(vertex: Any): Iterable[(X, M)] =
    if !hasVertex(vertex) then Nil
    else
      base
        .incidentEdges(vertex)
        .filter((edge, _) =>
          Queries.ends(edge).forall(hasVertex) && keepEdge.forall(_(edge)),
        )
        .map((edge, multiplicity) => edge -> reweigh(edge, multiplicity))
        .filter(_(1).nonZero)

  override def find(edge: Any): Option[X] = base
    .find(edge)
    .filter(multiplicity(_).nonZero)

  /**
    * The number of edges, counted from [[support]] if there is one, and
    * otherwise from the edges which might have changed: those in [[changed]],
    * and those at the removed vertices.
    */
  override def size: Option[Int] = support match
    case Some(edges) => Some(Queries.total(
        ValueSet.from(edges()).iterator.map(multiplicity(_).count),
      ))
    case None => base
        .size
        .map(size =>
          Queries.total(Iterator(
            size,
            changes(base, reweighed),
            -detachment,
          )),
        )

  override def degree(vertex: Any): Option[Int] = None

  /** The removed vertices, in the order they're counted in. */
  private lazy val removed: IndexedSeq[V] = base
    .vertexValues
    .filterNot(keep)
    .toIndexedSeq

  /** The position of each removed vertex in [[removed]]. */
  private lazy val positions: Map[Any, Int] = removed
    .view
    .zipWithIndex
    .map((vertex, position) => (vertex: Any) -> position)
    .toMap

  /** The edges of [[changed]] which no removed vertex detaches. */
  private def reweighed: Iterable[Any] = ValueSet
    .from(changed)
    .filter(Queries.ends(_).forall(!positions.contains(_)))

  /**
    * The number of edges lost by removing vertices, counting each edge once, at
    * its first removed end, without gathering them.
    */
  private def detachment: Int = Queries.total:
    removed
      .iterator
      .zipWithIndex
      .flatMap: (vertex, position) =>
        base
          .incidentEdges(vertex)
          .iterator
          .collect:
            case (edge, multiplicity) if first(edge, position) =>
              multiplicity.count

  /**
    * @return
    *   `true` if and only if no removed end of [[edge]] comes before the one at
    *   [[position]].
    */
  private def first(edge: Any, position: Int): Boolean = Queries
    .ends(edge)
    .forall(positions.get(_).forall(_ >= position))

/**
  * The queries about the image of the graph described by [[base]], with each
  * vertex mapped by [[vertexMap]] and each edge replaced by its [[edgeImages]],
  * whose vertices are the images of its own.
  *
  * @param kept
  *   `true` if and only if every copy of each edge has exactly one image, and
  *   the multiplicities of equal images add their counts, so that sizes and the
  *   sums of degrees are kept.
  */
private[kneiphof] final class Imaging[V, E, W, F, M : Multiplicity as M, X, Y]
  (
    base: Queries[V, E, M, X],
    vertexMap: V => W,
    edgeImages: X => Iterable[Y],
    kept: Boolean,
  )
  extends Queries[W, F, M, Y]:

  override lazy val vertexValues: ValueSet[W] =
    ValueSet.from(base.vertexValues.map(vertexMap))

  override def hasVertex(vertex: Any): Boolean = preimages.contains(vertex)

  override def multiplicity(edge: Any): M = candidates(edge).foldLeft(M.zero):
    (sum, original) =>
      sum +
        base.multiplicity(original).scale(edgeImages(original).count(_ == edge))

  override def incidentEdgeValues(vertex: Any): ValueSet[Y] =
    ValueSet.from(incidentEdges(vertex).map(_(0)))

  override def incidentEdges(vertex: Any): Iterable[(Y, M)] = ValueMap
    .from(preimagesOf(vertex).iterator.flatMap(base.incidentEdges))
    .iterator
    .flatMap: (original, multiplicity) =>
      edgeImages(original)
        .iterator
        .filter(Queries.ends(_).contains(vertex))
        .map(_ -> multiplicity)
    .foldLeft(ValueMap.empty[Y, M]):
      case (sums, (image, multiplicity)) => sums.updated(
          image,
          sums.getOrElse(image, M.zero) + multiplicity,
        )

  override def size: Option[Int] = if kept then base.size else None

  override def degree(vertex: Any): Option[Int] =
    if !kept then None
    else
      preimagesOf(vertex).foldLeft(Option(0)): (sum, original) =>
        sum
          .zip(base.degree(original))
          .map((sum, degree) => Math.addExact(sum, degree))

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
