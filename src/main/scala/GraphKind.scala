package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.views.{
  Combining, Imaging, Queries, Refining, Substitution, Tagging, Viewing,
}
import scala.compiletime.deferred

/**
  * A kind of graph [[G]], with edges of kind [[X]] and multiplicities in [[M]],
  * together with a representation for building new graphs.
  *
  * Each kind's companion offers one in the default representation, such as
  * [[Graph]] or [[GraphBase.kind]]. Another representation can be selected by
  * constructing a kind from its empty graph, e.g. `GraphBase.Kind(empty)`.
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

  /** The empty graph of this kind, in this kind's representation. */
  def empty: G[Nothing, Nothing]

  /**
    * @return
    *   [[graph]] with the multiplicity of [[edge]] set to [[multiplicity]].
    */
  def updated[V, E]
    (
      graph: G[V, E],
      edge: X[V, E],
      multiplicity: M,
    )
    : G[V, E]

  /**
    * @return
    *   The graph with the vertices and edges of all the given [[parts]], such
    *   as edges (`1 ~ 2`), chains (`1 ~ 2 ~ 3`) or multigraphs, adding the
    *   counts of repeated edges.
    */
  def apply[V, E](parts: C[V, E]*): G[V, E] = parts.foldLeft(empty: G[V, E]):
    (graph, part) =>
      merged(
        graph,
        part.vertexValues,
        part.edgeValues.map(counted),
      )(_ + _)

  /**
    * @return
    *   The graph with the single vertex [[vertex]] and no edges, which is the
    *   unit of `flatMapVertices`.
    */
  def vertex[V](vertex: V): G[V, Nothing] = empty.addVertex(vertex)

  /** @return The graph with the given [[vertices]] and no edges. */
  def vertices[V](vertices: V*): G[V, Nothing] = empty.addVertices(vertices)

  /** @return The graph with the given [[edges]] and their vertices. */
  def from[V, E](edges: Iterable[X[V, E]]): G[V, E] =
    increased(empty, edges.map(_ -> algebra.one))

  /**
    * @return
    *   The graph with the given [[vertices]], plus the given [[edges]] and
    *   their vertices.
    */
  def from[V, E]
    (
      vertices: Iterable[V],
      edges: Iterable[X[V, E]],
    )
    : G[V, E] = from(edges).addVertices(vertices)

  /**
    * @return
    *   The graph with the given [[vertices]], plus the given [[edges]] with
    *   their multiplicities, summed where an edge is repeated.
    */
  def fromMultiplicities[V, E]
    (
      vertices: Iterable[V],
      edges: Iterable[(X[V, E], M)],
    )
    : G[V, E] = increased(empty.addVertices(vertices), edges)

  /** @return [[graph]] with each of [[edges]] added, summing multiplicities. */
  private[kneiphof] def increased[V, E]
    (
      graph: G[V, E],
      edges: Iterable[(X[V, E], M)],
    )
    : G[V, E] = merged(graph, Nil, edges)(_ + _)

  /**
    * @return
    *   [[graph]] with [[vertices]] and [[edges]] added, where the
    *   multiplicities of each edge are combined by [[combine]].
    */
  private[kneiphof] def merged[V, E]
    (
      graph: G[V, E],
      vertices: Iterable[V],
      edges: Iterable[(X[V, E], M)],
    )
    (combine: (M, M) => M)
    : G[V, E] = edges.foldLeft(graph.addVertices(vertices)): (graph, entry) =>
    updated(
      graph,
      entry(0),
      combine(graph.multiplicity(entry(0)), entry(1)),
    )

  /**
    * @return
    *   The lazy graph answering [[queries]] without being built, and anything
    *   else as the graph from [[build]], which is only built when first needed.
    */
  private[kneiphof] def view[V, E]
    (
      queries: Queries[V, E, M, X[V, E]],
      build: => G[V, E],
    )
    : G[V, E]

  /**
    * @return
    *   The result of an operation on [[operands]], which is the graph from
    *   [[build]] if every operand is eager, and otherwise the lazy graph
    *   answering [[queries]] (see [[view]]). Laziness is the meaning of every
    *   operation, and building eager results straight away is an optimisation.
    *   It's only used for operations whose results are no bigger than their
    *   operands; operations which generate structure, such as joins and
    *   products, are always lazy, since chaining them could otherwise build a
    *   result exponential in the length of the expression.
    */
  private[kneiphof] def lazily[V, E]
    (operands: AnyGraph[?, ?, ?]*)
    (
      queries: => Queries[V, E, M, X[V, E]],
      build: => G[V, E],
    )
    : G[V, E] = if operands.forall(_.eager) then build else view(queries, build)

  /**
    * @return
    *   The union or sum of [[first]] and the graph [[second]] describes, whose
    *   vertices and edges are [[vertices]] and [[edges]], combining each edge's
    *   multiplicities by [[combine]].
    */
  private[kneiphof] def combined[V, E]
    (
      first: G[V, E],
      second: Queries[V, E, M, X[V, E]],
      operand: AnyGraph[?, ?, ?],
      vertices: => Iterable[V],
      edges: => Iterable[(X[V, E], M)],
    )
    (combine: (M, M) => M)
    : G[V, E] = lazily(first, operand)(
    Combining(
      viewing(first),
      second,
      combine,
      edges.map(_(0)),
    ),
    merged(first, vertices, edges)(combine),
  )

  /**
    * @return
    *   The subgraph of [[graph]] with only the vertices satisfying [[keep]],
    *   and each remaining edge's multiplicity changed by [[reweigh]], which may
    *   depend on [[operands]]. Only the edges in [[changed]] may have their
    *   multiplicities changed.
    */
  private[kneiphof] def refined[V, E]
    (
      graph: G[V, E],
      keep: V => Boolean,
      reweigh: (Any, M) => M,
      changed: => Iterable[Any],
      operands: Seq[AnyGraph[?, ?, ?]],
    )
    : G[V, E] = lazily(graph +: operands*)(
    Refining(viewing(graph), keep, reweigh, changed),
    changed.foldLeft(graph.removeVertices(graph.vertexValues.filterNot(keep))):
      (refined, edge) =>
        val multiplicity = refined.multiplicity(edge)
        val reweighed    = reweigh(edge, multiplicity)
        if reweighed == multiplicity then refined
        else refined.withMultiplicity(edge, reweighed),
  )

  /**
    * @return
    *   The image of [[graph]], which [[queries]] describes and whose edges are
    *   [[edges]], with each vertex mapped by [[vertexMap]] and each edge
    *   replaced by its [[edgeImages]], summing the multiplicities of equal
    *   images.
    *
    * @param exact
    *   `true` if and only if the images neither merge vertices nor change the
    *   number of copies of any edge.
    */
  private[kneiphof] def imaged[V, E, Y, W, F]
    (
      graph: AnyGraph[V, E, M],
      queries: Queries[V, E, M, Y],
      edges: => Iterable[(Y, M)],
    )
    (
      vertexMap: V => W,
      edgeImages: Y => Iterable[X[W, F]],
      exact: Boolean,
    )
    : G[W, F] = lazily(graph)(
    Imaging(queries, vertexMap, edgeImages, exact),
    fromMultiplicities(
      graph.vertexValues.map(vertexMap),
      edges.flatMap((edge, multiplicity) =>
        edgeImages(edge).map(_ -> multiplicity),
      ),
    ),
  )

  /**
    * @return
    *   The graph in which each vertex `v` of [[graph]] is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  private[kneiphof] def flatMapVertices[V, E, W]
    (graph: G[V, E], transform: V => G[W, E])
    : G[W, E] = view(
    Substitution(graph, transform, None),
    substituted(
      graph,
      graph.vertexValues.map(v => v -> transform(v)).toMap,
    ),
  )

  /**
    * @return
    *   The lexicographic sum, in which each vertex `v` of [[graph]] is replaced
    *   by a copy of `transform(v)` whose vertices are tagged by `v`.
    */
  private[kneiphof] def lexicographicSum[V, E, W]
    (graph: G[V, E], transform: V => G[W, E])
    : G[(V, W), E] =
    val parts = graph
      .vertexValues
      .map(vertex => vertex -> paired(vertex, transform(vertex)))
      .toMap
    val owner: Any => Option[V] =
      case (first, _) => graph.vertexValues.get(first)
      case _          => None
    view(
      Substitution(graph, parts, Some(owner)),
      substituted(graph, parts),
    )

  /**
    * @return
    *   The disjoint union of [[first]] and [[second]], whose vertices are
    *   tagged by which side they came from.
    */
  private[kneiphof] def disjointUnion[V, W, E]
    (first: G[V, E], second: G[W, E])
    : G[Either[V, W], E] =
    val sides = empty.addVertices(Seq(false, true))
    val left  = tagged(
      first,
      Left(_),
      {
        case Left(v) => first.vertexValues.get(v)
        case _       => None
      },
    )
    val right = tagged(
      second,
      Right(_),
      {
        case Right(w) => second.vertexValues.get(w)
        case _        => None
      },
    )
    val parts: Map[Boolean, G[Either[V, W], E]] =
      Map(false -> left, true -> right)
    val owner: Any => Option[Boolean] =
      case Left(_)  => Some(false)
      case Right(_) => Some(true)
      case _        => None
    lazily(first, second)(
      Substitution(sides, parts, Some(owner)),
      substituted(sides, parts),
    )

  /**
    * @return
    *   [[graph]] with each vertex `v` relabelled `tag(v)`, where [[tag]] is
    *   injective with inverse [[untag]]. This is always lazy, since it's only
    *   used within other operations, which decide whether to build the result.
    */
  private[kneiphof] def tagged[V, T, E]
    (
      graph: G[V, E],
      tag: V => T,
      untag: Any => Option[V],
    )
    : G[T, E] = view(
    Tagging(graph, tag, untag),
    graph.mapVertices(tag),
  )

  /** @return The queries about [[graph]], answered by [[graph]] itself. */
  private[kneiphof] def viewing[V, E]
    (graph: G[V, E])
    : Queries[V, E, M, X[V, E]] = Viewing(graph, Embedding.identity[M])

  /** @return [[part]], with each vertex tagged by [[vertex]]. */
  private def paired[V, W, E](vertex: V, part: G[W, E]): G[(V, W), E] = tagged(
    part,
    (vertex, _),
    {
      case (`vertex`, value) => part.vertexValues.get(value)
      case _                 => None
    },
  )

  /** @return The graph from `flatMapVertices`, built from every edge. */
  private def substituted[V, E, W]
    (graph: G[V, E], parts: Map[V, G[W, E]])
    : G[W, E] =
    val outer = graph
      .edgeValues
      .toSeq
      .flatMap: (edge, multiplicity) =>
        edge.substitute(parts(_).vertexValues).map(_ -> multiplicity)
    val replacements = parts
      .values
      .foldLeft(empty: G[W, E]): (sum, part) =>
        merged(sum, part.vertexValues, part.edgeValues)(_ + _)
    increased(replacements, outer)

  /**
    * @return
    *   The join of [[first]] and [[second]], being their sum plus the edge
    *   `link(u, v)` for every vertex `u` of [[first]] and `v` of [[second]].
    */
  private[kneiphof] def joined[V, E]
    (first: G[V, E], second: G[V, E])
    (link: (V, V) => X[V, E])
    : G[V, E] =
    val links =
      for
        u <- first.vertexValues.toSeq
        v <- second.vertexValues.toSeq
      yield link(u, v) -> algebra.one
    val sum = merged(
      first,
      second.vertexValues,
      second.edgeValues,
    )(_ + _)
    increased(sum, links)

  /** @return [[entry]], with its count read as a multiplicity in [[M]]. */
  private def counted[Y](entry: (Y, Count)): (Y, M) = entry(0) ->
    algebra.fromInt(entry(1).toInt)
