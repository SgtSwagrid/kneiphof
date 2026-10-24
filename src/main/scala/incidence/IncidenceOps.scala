package com.alecdorrington
package kneiphof
package incidence

/**
  * The representation of every kind of graph as an [[Incidence]] store, with
  * effectively constant-time updates and incidence queries.
  */
private[kneiphof] trait IncidenceOps[
  V,
  E,
  M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
] extends GraphOps[V, E, M, X, G]:
  this: G[V, E] =>

  /** The store holding this graph. */
  protected def store: Incidence[V, X[V, E], M]

  /** @return The graph of the same kind held by [[store]]. */
  protected def build[W, F](store: Incidence[W, X[W, F], M]): G[W, F]

  override def vertexValues: ValueSet[V] = store.vertices

  override def edgeValues: ValueMap[X[V, E], M] = store.multiplicities

  override def incidentEdgeValues(vertex: Any): ValueSet[X[V, E]] = store
    .incident
    .getOrElse(vertex, ValueSet.empty)

  override def empty: G[Nothing, Nothing] = build(Incidence.empty)

  override def addVertex[W >: V](vertex: W): G[W, E] =
    build(store.withVertex(vertex))

  override def removeVertex(vertex: Any): G[V, E] =
    build(store.withoutVertex(vertex)(_.vertices))

  override def withMultiplicity(edge: Any, multiplicity: M): G[V, E] =
    edgeValues.getKey(edge).fold(this: G[V, E])(set(_, multiplicity))

  override def map[W, F](vertexMap: V => W, labelMap: E => F): G[W, F] =
    rebuild(vertexMap, _.map(vertexMap, labelMap))

  /**
    * @return
    *   This graph with the multiplicity of [[edge]] set to [[multiplicity]],
    *   adding its vertices if necessary.
    */
  protected def set[W >: V, F >: E](edge: X[W, F], multiplicity: M): G[W, F] =
    build[W, F]:
      if multiplicity.isZero then store.withoutEdge(edge, edge.vertices)
      else store.withEdge(edge, edge.vertices, multiplicity)

  /**
    * @return
    *   The image of this graph, with each vertex transformed by [[vertexMap]]
    *   and each edge by [[edgeMap]], summing the multiplicities of edges with
    *   the same image.
    */
  protected def rebuild[W, F]
    (vertexMap: V => W, edgeMap: X[V, E] => X[W, F])
    : G[W, F] =
    val vertices = Incidence.empty[M].withVertices(vertexValues.map(vertexMap))
    build:
      edgeValues.foldLeft(vertices: Incidence[W, X[W, F], M]): (store, entry) =>
        val image = edgeMap(entry(0))
        val total = store.multiplicities.getOrElse(image, algebra.zero) +
          entry(1)
        store.withEdge(image, image.vertices, total)
