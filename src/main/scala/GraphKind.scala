package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{
  CartesianProduct, Copy, Difference, EdgeInduced, Explicit, Image,
  IncidenceStructure, Intersection, Join, Overlay, Patch, Subgraph,
  Substitution, Sum, Union,
}
import scala.compiletime.deferred

/**
  * A kind of graph [[G]], with edges of kind [[X]] and multiplicities in [[M]],
  * which builds the graphs of that kind around their incidence structures.
  *
  * Each kind's companion offers one, such as [[Graph]] or [[GraphBase.kind]].
  *
  * @tparam C
  *   The multigraphs of the same kind, such as edges and chains, from which
  *   graphs of this kind can be built.
  */
trait GraphKind[
  M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
  C[+v, +e] <: GraphOps[v, e, Count, X, C],
]:

  /** The algebra of edge multiplicities. */
  protected given algebra: Multiplicity[M] = deferred

  /** `true` if and only if the edges of this kind are directed. */
  private[kneiphof] def directed: Boolean

  /**
    * @return
    *   The edge of this kind from [[source]] to [[target]], labelled [[label]],
    *   as a join adds.
    */
  private[kneiphof] def link[V, E](source: V, target: V, label: E): X[V, E]

  /**
    * @return
    *   The join of the graphs whose incidence structures are [[left]] and
    *   [[right]], whose new edges are labelled [[label]].
    */
  private[kneiphof] def join[V, E, L]
    (
      left: IncidenceStructure[V, M, X[V, E]],
      right: IncidenceStructure[V, M, X[V, E]],
      label: L,
    )
    : IncidenceStructure[V, M, X[V, E | L]] = Join[V, E, L, M, X](
    left,
    right,
    link(_, _, label),
    label,
    directed,
  )

  /**
    * @return
    *   The graph of this kind whose incidence structure is [[structure]].
    */
  private[kneiphof] def wrap[V, E](structure: IncidenceStructure[V, M, X[V, E]])
    : G[V, E]

  /** Alias of [[vertices]]. */
  def empty: G[Nothing, Nothing] = vertices()

  /**
    * @return
    *   The graph with the vertices and edges of all the given [[parts]], such
    *   as edges (`1 ~ 2`), chains (`1 ~ 2 ~ 3`) or multigraphs, adding the
    *   counts of repeated edges.
    */
  def apply[V, E](parts: C[V, E]*): G[V, E] = wrap:
    parts.foldLeft(Explicit.empty[M]: Explicit[V, M, X[V, E]]): (sum, part) =>
      increase(
        sum.withVertices(part.vertexValues),
        part
          .edgeValues
          .map((edge, count) => edge -> algebra.fromInt(count.toInt)),
      )

  /**
    * @return
    *   The graph with the single vertex [[vertex]] and no edges, which is the
    *   unit of [[HypergraphBase.flatMapVertices]] and
    *   [[DirectedHypergraphBase.flatMapVertices]].
    */
  def vertex[V : NotHandle](vertex: V): G[V, Nothing] =
    wrap(Explicit.empty.withVertex(vertex))

  /** @return The graph with the given [[vertices]] and no edges. */
  def vertices[V : NotHandle](vertices: V*): G[V, Nothing] =
    wrap(Explicit.empty.withVertices(vertices))

  /** @return The graph with the given [[edges]] and their vertices. */
  def from[V, E](edges: Iterable[X[V, E]]): G[V, E] =
    fromMultiplicities(Nil, edges.map(_ -> algebra.one))

  /**
    * @return
    *   The graph with the given [[vertices]], plus the given [[edges]] and
    *   their vertices.
    */
  def from[V : NotHandle, E]
    (
      vertices: Iterable[V],
      edges: Iterable[X[V, E]],
    )
    : G[V, E] = fromMultiplicities(vertices, edges.map(_ -> algebra.one))

  /**
    * @return
    *   The graph with the given [[vertices]], plus the given [[edges]] with
    *   their multiplicities, summed where an edge is repeated.
    */
  def fromMultiplicities[V : NotHandle, E]
    (
      vertices: Iterable[V],
      edges: Iterable[(X[V, E], M)],
    )
    : G[V, E] = wrap:
    increase(
      Explicit.empty[M].withVertices(vertices),
      edges,
    )

  /**
    * @return
    *   The graph [[graph]], with the multiplicity of [[edge]] set to
    *   [[multiplicity]], adding its vertices unless the edge is now absent.
    */
  private[kneiphof] def withEdge[V, E]
    (
      graph: G[V, E],
      edge: X[V, E],
      multiplicity: M,
    )
    : G[V, E] = wrap:
    graph
      .structure
      .patch
      .withEdge(
        edge,
        if multiplicity.isZero then Nil else edge.vertexValues,
        multiplicity,
      )

  /** @return The graph [[graph]], with [[vertex]] added. */
  private[kneiphof] def withVertex[V, E](graph: G[V, E], vertex: V): G[V, E] =
    wrap(graph.structure.patch.withVertex(vertex))

  /** @return The graph [[graph]], without [[vertex]] and its incident edges. */
  private[kneiphof] def withoutVertex[V, E]
    (graph: G[V, E], vertex: Any)
    : G[V, E] =
    if !graph.hasVertex(vertex) then graph
    else wrap(graph.structure.patch.withoutVertex(vertex))

  /**
    * @return
    *   The graph [[graph]], with the multiplicity of [[edge]] set to
    *   [[multiplicity]], or [[graph]] unchanged if it doesn't contain [[edge]].
    */
  private[kneiphof] def withMultiplicity[V, E]
    (graph: G[V, E], edge: Any, multiplicity: M)
    : G[V, E] = graph
    .structure
    .find(edge)
    .fold(graph)(found =>
      wrap(graph.structure.patch.withEdge(found, Nil, multiplicity)),
    )

  /** @return The graph [[graph]], with every edge stored. */
  private[kneiphof] def materialise[V, E](graph: G[V, E]): G[V, E] =
    if graph.structure.stored then graph else wrap(graph.structure.explicit)

  /**
    * @return
    *   The sum of [[first]] and the graph whose incidence structure is
    *   [[second]]. For edge sets, this is their union.
    */
  private[kneiphof] def sum[V, E]
    (
      first: G[V, E],
      second: IncidenceStructure[V, M, X[V, E]],
    )
    : G[V, E] =
    if !algebra.additive then union(first, second)
    else
      overlay(first, second, _ + _)(Sum.of(first.structure, second)):
        case sum: Sum[V, M, X[V, E]] => Some(sum)
        case _                       => None

  /**
    * @return
    *   The union of [[first]] and the graph whose incidence structure is
    *   [[second]].
    */
  private[kneiphof] def union[V, E]
    (
      first: G[V, E],
      second: IncidenceStructure[V, M, X[V, E]],
    )
    : G[V, E] =
    overlay(first, second, _ | _)(Union.of(first.structure, second)):
      case union: Union[V, M, X[V, E]] => Some(union)
      case _                           => None

  /**
    * @return
    *   The intersection of [[graph]] and the graph whose incidence structure is
    *   [[other]].
    */
  private[kneiphof] def intersection[V, E]
    (
      graph: G[V, E],
      other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
    )
    : G[V, E] = wrap(Intersection.of(graph.structure, other))

  /**
    * @return
    *   The difference of [[graph]] and the graph whose incidence structure is
    *   [[other]]. A few listable edges are gathered into a difference [[graph]]
    *   already is, or else subtracted from a patch of it, so that removing
    *   edges one at a time stays cheap. Otherwise, the result is one
    *   difference, which a chain of differences extends.
    */
  private[kneiphof] def difference[V, E]
    (
      graph: G[V, E],
      other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
    )
    : G[V, E] =
    if !GraphKind.few(other) then wrap(Difference.of(graph.structure, other))
    else
      graph.structure match
        case difference: Difference[V, M, X[V, E]] =>
          wrap(difference.withGathered(add(difference.gathered, other)(_ + _)))
        case structure => wrap(subtract(structure.patch, other))

  /**
    * @return
    *   The subgraph of [[graph]] with only the vertices satisfying [[keep]],
    *   and the edges between them satisfying [[keepEdge]], if given.
    */
  private[kneiphof] def subgraph[V, E]
    (
      graph: G[V, E],
      keep: V => Boolean,
      keepEdge: Option[X[V, E] => Boolean],
    )
    : G[V, E] = wrap(Subgraph.of(graph.structure, keep, keepEdge))

  /**
    * @return
    *   The subgraph of [[graph]] induced by the edges satisfying [[keep]], with
    *   only the vertices incident to them.
    */
  private[kneiphof] def edgeInduced[V, E]
    (graph: G[V, E], keep: X[V, E] => Boolean)
    : G[V, E] = wrap(EdgeInduced.of(graph.structure, keep))

  /**
    * @return
    *   The image of the graph whose incidence structure is [[structure]], with
    *   each vertex mapped by [[vertexMap]] and each edge replaced by its
    *   [[edgeImages]], summing the multiplicities of equal images.
    *
    * @param kept
    *   `true` if and only if every copy of each edge has exactly one image, and
    *   the multiplicities of equal images add their counts.
    */
  private[kneiphof] def image[V, Y <: AnyEdge[?, ?, ?], W, F]
    (structure: IncidenceStructure[V, M, Y])
    (
      vertexMap: V => W,
      edgeImages: Y => Iterable[X[W, F]],
      kept: Boolean,
    )
    : G[W, F] = wrap(Image.of(structure, vertexMap, edgeImages, kept))

  /**
    * @return
    *   The graph in which each vertex `v` of [[graph]] is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  private[kneiphof] def flatMapVertices[V, E, W]
    (graph: G[V, E], transform: V => G[W, E])
    : G[W, E] = wrap:
    Substitution[V, W, E, M, X](
      graph.structure,
      transform(_).structure,
      None,
    )

  /**
    * @return
    *   The lexicographic sum, in which each vertex `v` of [[graph]] is replaced
    *   by a copy of `transform(v)` whose vertices are tagged by `v`.
    */
  private[kneiphof] def lexicographicSum[V, E, W]
    (graph: G[V, E], transform: V => G[W, E])
    : G[(V, W), E] =
    val owner: Any => Option[V] =
      case (first, _) => graph.structure.vertexValue(first)
      case _          => None
    wrap:
      Substitution[V, (V, W), E, M, X](
        graph.structure,
        vertex => pair(vertex, transform(vertex).structure),
        Some(owner),
      )

  /**
    * @return
    *   The Cartesian product of [[first]] and [[second]], on the pairs of their
    *   vertices.
    */
  private[kneiphof] def cartesianProduct[V, W, E]
    (first: G[V, E], second: G[W, E])
    : G[(V, W), E] =
    wrap(CartesianProduct[V, W, E, M, X](first.structure, second.structure))

  /**
    * @return
    *   The disjoint union of [[first]] and [[second]], whose vertices are
    *   tagged by which side they came from.
    */
  private[kneiphof] def disjointUnion[V, W, E]
    (first: G[V, E], second: G[W, E])
    : G[Either[V, W], E] = wrap:
    Substitution[Boolean, Either[V, W], E, M, X](
      Explicit.empty[M].withVertices(Seq(false, true)),
      Map(
        false -> tag[V, Either[V, W], E](
          first.structure,
          Left(_),
          { case Left(v) => v },
        ),
        true -> tag[W, Either[V, W], E](
          second.structure,
          Right(_),
          { case Right(w) => w },
        ),
      ),
      Some(GraphKind.side),
    )

  /**
    * @return
    *   The sum or union of [[first]] and the graph whose incidence structure is
    *   [[second]], whose multiplicities are combined by [[combine]]. A few
    *   listable vertices and edges are gathered into an overlay [[first]]
    *   already is, as [[overlayOf]] tells, or else added to a patch of it, so
    *   that adding edges one at a time stays cheap. Otherwise, the result is
    *   [[both]], the overlay of the two, which a chain of overlays extends.
    */
  private def overlay[V, E]
    (
      first: G[V, E],
      second: IncidenceStructure[V, M, X[V, E]],
      combine: (M, M) => M,
    )
    (both: => IncidenceStructure[V, M, X[V, E]])
    (
      overlayOf: IncidenceStructure[V, M, X[V, E]] => Option[
        Overlay[V, M, X[V, E]],
      ],
    )
    : G[V, E] = wrap:
    if !GraphKind.few(second) then both
    else
      overlayOf(first.structure).fold(
        add(first.structure.patch, second)(combine),
      ): existing =>
        existing.withGathered(add(existing.gathered, second)(combine))

  /**
    * @return
    *   [[patch]], with the vertices and edges of [[few]] added, combining the
    *   multiplicities of each edge by [[combine]].
    */
  private def add[V, Y <: AnyEdge[V, ?, ?]]
    (
      patch: Patch[V, M, Y],
      few: IncidenceStructure[V, M, Y],
    )
    (combine: (M, M) => M)
    : Patch[V, M, Y] = few
    .edges
    .foldLeft(few.vertexValues.foldLeft(patch)(_.withVertex(_))):
      case (patch, (edge, multiplicity)) => patch.withEdge(
          edge,
          edge.vertexValues,
          combine(patch.multiplicity(edge), multiplicity),
        )

  /**
    * @return
    *   [[patch]], with the multiplicity of each edge of [[few]] subtracted.
    */
  private def subtract[V, Y <: AnyEdge[?, ?, ?]]
    (
      patch: Patch[V, M, Y],
      few: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
    )
    : Patch[V, M, Y] = few
    .edges
    .foldLeft(patch):
      case (patch, (edge, multiplicity)) => patch
          .find(edge)
          .fold(patch)(found =>
            patch.withEdge(
              found,
              Nil,
              patch.multiplicity(found) - multiplicity,
            ),
          )

  /**
    * @return
    *   The structure of [[part]], with each vertex tagged by [[vertex]].
    */
  private def pair[V, W, E]
    (
      vertex: V,
      part: IncidenceStructure[W, M, X[W, E]],
    )
    : IncidenceStructure[(V, W), M, X[(V, W), E]] = tag(
    part,
    (vertex, _),
    { case (`vertex`, value) => value },
  )

  /**
    * @return
    *   The copy of the graph whose incidence structure is [[structure]], with
    *   each vertex tagged by [[tagOf]], where [[untag]] gives the value tagged
    *   as a vertex.
    */
  private def tag[V, T, E]
    (
      structure: IncidenceStructure[V, M, X[V, E]],
      tagOf: V => T,
      untag: PartialFunction[Any, Any],
    )
    : IncidenceStructure[T, M, X[T, E]] = Copy[V, T, E, M, X](
    structure,
    tagOf,
    vertex => untag.lift(vertex).flatMap(structure.vertexValue),
  )

  /**
    * @return
    *   The structure [[explicit]], with each of [[edges]] added, summing
    *   multiplicities.
    */
  private def increase[V, E]
    (
      explicit: Explicit[V, M, X[V, E]],
      edges: Iterable[(X[V, E], M)],
    )
    : Explicit[V, M, X[V, E]] = edges.foldLeft(explicit):
    case (explicit, (edge, multiplicity)) =>
      explicit.increase(edge, edge.vertexValues, multiplicity)

/** The companion object for [[GraphKind]]. */
private[kneiphof] object GraphKind:

  /**
    * The most vertices and edges an operand can have for them to be copied into
    * a patch or an overlay's gathered edges, rather than kept as an operand.
    */
  val patchable: Int = 64

  /**
    * A few listable vertices and edges are cheap to copy, so they're added to a
    * patch or gathered into an overlay rather than kept as an operand.
    *
    * @return
    *   `true` if and only if [[structure]] is listable, with at most
    *   [[patchable]] vertices and edges, found without counting all of its
    *   edges.
    */
  private def few(structure: IncidenceStructure[?, ?, ?]): Boolean =
    structure.listable &&
    IncidenceStructure.knownAtMost(structure, patchable) &&
    structure.edges.drop(patchable).isEmpty

  /**
    * @return
    *   The side of a disjoint union, `false` for the left and `true` for the
    *   right, which [[vertex]] is tagged with, if any.
    */
  private def side(vertex: Any): Option[Boolean] = vertex match
    case Left(_)  => Some(false)
    case Right(_) => Some(true)
    case _        => None
