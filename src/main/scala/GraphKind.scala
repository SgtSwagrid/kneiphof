package com.alecdorrington
package kneiphof

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
    *   The graph in which each vertex `v` of [[graph]] is replaced by the graph
    *   `transform(v)`, and each edge by its copies between the replacements of
    *   its ends. Equal vertices of different replacements merge.
    */
  private[kneiphof] def flatMapVertices[V, E, W]
    (graph: G[V, E], transform: V => G[W, E])
    : G[W, E] =
    val inner = graph.vertexValues.toSeq.map(v => v -> transform(v)).toMap
    val outer = graph
      .edgeValues
      .toSeq
      .flatMap: (edge, multiplicity) =>
        edge.substitute(inner(_).vertexValues).map(_ -> multiplicity)
    val replacements = inner
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
