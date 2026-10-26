package com.alecdorrington
package kneiphof
package shapes

/**
  * An unlabelled graph whose edges are given by a [[Shape]], answering queries
  * about vertices and edges from the shape alone. Anything which needs every
  * edge, such as iterating over them or adding to the graph, is answered by the
  * graph built from the shape, which is only built when first needed.
  */
private[kneiphof] final class ShapeGraph[V, M : Multiplicity]
  (
    kind: GraphBase.Kind[M],
    indexing: Indexing[V],
    shape: Shape,
  )
  extends GraphBase[V, Unit, M],
          Delegating[
            V,
            Unit,
            M,
            Edge,
            [v, e] =>> GraphBase[v, e, M],
          ]:

  override lazy val vertexValues: ValueSet[V] =
    ValueSet.from((0 until order).map(indexing.vertex))

  override def hasVertex(vertex: Any): Boolean = indexing
    .index(vertex)
    .isDefined

  override def empty: GraphBase[Nothing, Nothing, M] = kind.empty

  override def order: Int = shape.order

  override def size: Int = shape.size

  override def hasEdge(edge: Any): Boolean = edge match
    case kneiphof.Edge(first, second, ()) => linked(first, second) ||
      linked(second, first)
    case _ => false

  override def multiplicity(edge: Any): M =
    if hasEdge(edge) then algebra.one else algebra.zero

  override def incidentEdgeValues
    (vertex: Any)
    : ValueSet[kneiphof.Edge[V, Unit]] = ValueSet.from:
    indexing
      .index(vertex)
      .toSeq
      .flatMap: index =>
        (shape.arcsFrom(index) ++ shape.arcsInto(index)).map(edge(index, _))

  override def updated[W >: V, F >: Unit]
    (
      edge: kneiphof.Edge[W, F],
      multiplicity: M,
    )
    : GraphBase[W, F, M] = delegate.updated(edge, multiplicity)

  override protected lazy val delegate: GraphBase[V, Unit, M] = kind.from(
    vertexValues,
    (0 until order).flatMap(source =>
      shape.arcsFrom(source).map(edge(source, _)),
    ),
  )

  /**
    * @return
    *   The edge between the vertices numbered [[first]] and [[second]].
    */
  private def edge(first: Int, second: Int): kneiphof.Edge[V, Unit] = kneiphof
    .Edge(
      indexing.vertex(first),
      indexing.vertex(second),
    )

  /**
    * @return
    *   `true` if and only if the shape has an arc from [[source]] to
    *   [[target]].
    */
  private def linked(source: Any, target: Any): Boolean =
    (indexing.index(source), indexing.index(target)) match
      case (Some(source), Some(target)) => shape.linked(source, target)
      case _                            => false

/**
  * An unlabelled directed graph whose edges are the arcs of a [[Shape]], which
  * answers queries in the same way as a [[ShapeGraph]].
  */
private[kneiphof] final class DirectedShapeGraph[V, M : Multiplicity]
  (
    kind: DirectedGraphBase.Kind[M],
    indexing: Indexing[V],
    shape: Shape,
  )
  extends DirectedGraphBase[V, Unit, M],
          Delegating[
            V,
            Unit,
            M,
            Arc,
            [v, e] =>> DirectedGraphBase[v, e, M],
          ]:

  override lazy val vertexValues: ValueSet[V] =
    ValueSet.from((0 until order).map(indexing.vertex))

  override def hasVertex(vertex: Any): Boolean = indexing
    .index(vertex)
    .isDefined

  override def empty: DirectedGraphBase[Nothing, Nothing, M] = kind.empty

  override def order: Int = shape.order

  override def size: Int = shape.size

  override def hasEdge(edge: Any): Boolean = edge match
    case DirectedEdge(source, target, ()) =>
      (indexing.index(source), indexing.index(target)) match
        case (Some(source), Some(target)) => shape.linked(source, target)
        case _                            => false
    case _ => false

  override def multiplicity(edge: Any): M =
    if hasEdge(edge) then algebra.one else algebra.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[Arc[V, Unit]] =
    ValueSet.from:
      indexing
        .index(vertex)
        .toSeq
        .flatMap: index =>
          shape.arcsFrom(index).map(arc(index, _)) ++
            shape.arcsInto(index).map(arc(_, index))

  override def updated[W >: V, F >: Unit]
    (edge: Arc[W, F], multiplicity: M)
    : DirectedGraphBase[W, F, M] = delegate.updated(edge, multiplicity)

  override def transpose: DirectedGraphBase[V, Unit, M] = delegate.transpose

  override protected lazy val delegate: DirectedGraphBase[V, Unit, M] = kind
    .from(
      vertexValues,
      (0 until order).flatMap(source =>
        shape.arcsFrom(source).map(arc(source, _)),
      ),
    )

  /** @return The arc from the vertex numbered [[source]] to [[target]]. */
  private def arc(source: Int, target: Int): Arc[V, Unit] = DirectedEdge(
    indexing.vertex(source),
    indexing.vertex(target),
  )
