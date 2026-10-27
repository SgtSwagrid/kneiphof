package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.{
  CartesianProduct, Copy, Difference, EdgeInduced, Explicit, Image,
  IncidenceStructure, Intersection, Subgraph, Substitution, Sum, Union,
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

  /**
    * @return
    *   The graph of this kind whose incidence structure is [[structure]].
    */
  private[kneiphof] def wrap[V, E](structure: IncidenceStructure[V, M, X[V, E]])
    : G[V, E]

  /** The empty graph of this kind. */
  def empty: G[Nothing, Nothing] = wrap(Explicit.empty)

  /**
    * @return
    *   The graph with the vertices and edges of all the given [[parts]], such
    *   as edges (`1 ~ 2`), chains (`1 ~ 2 ~ 3`) or multigraphs, adding the
    *   counts of repeated edges.
    */
  def apply[V, E](parts: C[V, E]*): G[V, E] = wrap:
    parts.foldLeft(Explicit.empty[M]: Explicit[V, M, X[V, E]]): (sum, part) =>
      increased(
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
    increased(
      Explicit.empty[M].withVertices(vertices),
      edges,
    )

  /**
    * @return
    *   The graph [[graph]], with the multiplicity of [[edge]] set to
    *   [[multiplicity]], adding its vertices unless the edge is now absent.
    */
  private[kneiphof] def updated[V, E]
    (
      graph: G[V, E],
      edge: X[V, E],
      multiplicity: M,
    )
    : G[V, E] = wrap:
    graph
      .structure
      .patched
      .withEdge(
        edge,
        if multiplicity.isZero then Nil else edge.vertexValues,
        multiplicity,
      )

  /** @return The graph [[graph]], with [[vertex]] added. */
  private[kneiphof] def withVertex[V, E](graph: G[V, E], vertex: V): G[V, E] =
    wrap(graph.structure.patched.withVertex(vertex))

  /** @return The graph [[graph]], without [[vertex]] and its incident edges. */
  private[kneiphof] def withoutVertex[V, E]
    (graph: G[V, E], vertex: Any)
    : G[V, E] =
    if !graph.hasVertex(vertex) then graph
    else wrap(graph.structure.patched.withoutVertex(vertex))

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
      wrap(graph.structure.patched.withEdge(found, Nil, multiplicity)),
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
      overlaid(
        first,
        second,
        _.isInstanceOf[Sum[?, ?, ?]],
      )(_ + _):
        Sum.of(first.structure, second)

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
    : G[V, E] = overlaid(
    first,
    second,
    _.isInstanceOf[Union[?, ?, ?]],
  )(_ | _):
    Union.of(first.structure, second)

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
    *   [[other]]. If [[other]] is a few stored edges, and [[graph]] isn't
    *   already a difference, they're subtracted from a patch of [[graph]], so
    *   that removing edges one at a time stays cheap. Otherwise, the result is
    *   one difference, which a chain of differences extends.
    */
  private[kneiphof] def difference[V, E]
    (
      graph: G[V, E],
      other: IncidenceStructure[Any, M, AnyEdge[?, ?, ?]],
    )
    : G[V, E] =
    if graph.structure.isInstanceOf[Difference[?, ?, ?]] || !few(other) then
      wrap(Difference.of(graph.structure, other))
    else
      wrap:
        other
          .edges
          .foldLeft(graph.structure.patched):
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
  private[kneiphof] def imaged[V, Y <: AnyEdge[?, ?, ?], W, F]
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
        vertex => paired(vertex, transform(vertex).structure),
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
        false -> tagged[V, Either[V, W], E](
          first.structure,
          Left(_),
          { case Left(v) => v },
        ),
        true -> tagged[W, Either[V, W], E](
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
    *   [[second]], whose multiplicities are combined by [[combine]]. If
    *   [[second]] is a few stored edges, and [[first]] isn't already such an
    *   overlay, as [[merges]] tells, they're added to a patch of [[first]], so
    *   that adding edges one at a time stays cheap. Otherwise, the result is
    *   [[overlay]], which a chain of overlays extends.
    */
  private def overlaid[V, E]
    (
      first: G[V, E],
      second: IncidenceStructure[V, M, X[V, E]],
      merges: IncidenceStructure[V, M, X[V, E]] => Boolean,
    )
    (combine: (M, M) => M)
    (overlay: => IncidenceStructure[V, M, X[V, E]])
    : G[V, E] =
    if merges(first.structure) || !few(second) then wrap(overlay)
    else
      val extended = second
        .vertexValues
        .foldLeft(first.structure.patched)(_.withVertex(_))
      wrap:
        second
          .edges
          .foldLeft(extended):
            case (patch, (edge, multiplicity)) => patch.withEdge(
                edge,
                edge.vertexValues,
                combine(patch.multiplicity(edge), multiplicity),
              )

  /**
    * @return
    *   `true` if and only if [[structure]] is stored, with only a few edges,
    *   found without counting all of them.
    */
  private def few(structure: IncidenceStructure[?, M, ?]): Boolean =
    structure.stored && structure.edges.drop(GraphKind.patchable).isEmpty

  /**
    * @return
    *   The structure of [[part]], with each vertex tagged by [[vertex]].
    */
  private def paired[V, W, E]
    (
      vertex: V,
      part: IncidenceStructure[W, M, X[W, E]],
    )
    : IncidenceStructure[(V, W), M, X[(V, W), E]] = tagged(
    part,
    (vertex, _),
    { case (`vertex`, value) => value },
  )

  /**
    * @return
    *   The copy of the graph whose incidence structure is [[structure]], with
    *   each vertex tagged by [[tag]], where [[untag]] gives the value tagged as
    *   a vertex.
    */
  private def tagged[V, T, E]
    (
      structure: IncidenceStructure[V, M, X[V, E]],
      tag: V => T,
      untag: PartialFunction[Any, Any],
    )
    : IncidenceStructure[T, M, X[T, E]] = Copy[V, T, E, M, X](
    structure,
    tag,
    vertex => untag.lift(vertex).flatMap(structure.vertexValue),
  )

  /**
    * @return
    *   The structure [[explicit]], with each of [[edges]] added, summing
    *   multiplicities.
    */
  private def increased[V, E]
    (
      explicit: Explicit[V, M, X[V, E]],
      edges: Iterable[(X[V, E], M)],
    )
    : Explicit[V, M, X[V, E]] = edges.foldLeft(explicit):
    case (explicit, (edge, multiplicity)) =>
      explicit.increased(edge, edge.vertexValues, multiplicity)

/** The companion object for [[GraphKind]]. */
private[kneiphof] object GraphKind:

  /**
    * The most edges a stored operand can have for its edges to be added to a
    * patch, rather than kept as an operand.
    */
  val patchable: Int = 64

  /**
    * @return
    *   The side of a disjoint union, `false` for the left and `true` for the
    *   right, which [[vertex]] is tagged with, if any.
    */
  private def side(vertex: Any): Option[Boolean] = vertex match
    case Left(_)  => Some(false)
    case Right(_) => Some(true)
    case _        => None
