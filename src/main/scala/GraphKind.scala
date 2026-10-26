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
  */
trait GraphKind[
  M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
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

  /** @return The graph with the given [[edges]] and their vertices. */
  def apply[V, E](edges: X[V, E]*): G[V, E] = from(edges)

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
    : G[V, E] = edges.foldLeft(graph): (graph, entry) =>
    updated(
      graph,
      entry(0),
      graph.multiplicity(entry(0)) + entry(1),
    )

  /**
    * @return
    *   The graph with the vertices of [[first]] and [[second]], where each
    *   edge's multiplicities are combined by [[combine]].
    */
  private[kneiphof] def merged[V, E]
    (first: G[V, E], second: G[V, E])
    (combine: (M, M) => M)
    : G[V, E] =
    val (large, small) =
      if first.edgeValues.size >= second.edgeValues.size then (first, second)
      else (second, first)
    small
      .edgeValues
      .foldLeft(large.addVertices(small.vertexValues)): (graph, entry) =>
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
        edge.flatMapVertices(inner(_).vertexValues).map(_ -> multiplicity)
    val replacements = inner
      .values
      .foldLeft(empty: G[W, E]): (sum, part) =>
        merged(sum, part)(_ + _)
    increased(replacements, outer)
