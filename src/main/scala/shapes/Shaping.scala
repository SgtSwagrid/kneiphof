package com.alecdorrington
package kneiphof
package shapes

import com.alecdorrington.kneiphof.views.Queries

/**
  * The queries about the unlabelled graph whose edges are given by a [[Shape]],
  * answered from the shape alone.
  */
private[kneiphof] final class Shaping[V, M : Multiplicity as M]
  (indexing: Indexing[V], shape: Shape)
  extends Queries[V, Unit, M, Edge[V, Unit]]:

  override lazy val vertexValues: ValueSet[V] =
    ValueSet.from((0 until shape.order).map(indexing.vertex))

  override def hasVertex(vertex: Any): Boolean = indexing
    .index(vertex)
    .isDefined

  override def multiplicity(edge: Any): M = edge match
    case Queries.Pair(first, second, ())
      if linked(first, second) || linked(second, first) => M.one
    case _ => M.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[Edge[V, Unit]] =
    ValueSet.from:
      indexing
        .index(vertex)
        .toSeq
        .flatMap: index =>
          (shape.arcsFrom(index) ++ shape.arcsInto(index)).map(edge(index, _))

  override def size: Option[Int] = Some(shape.size)

  override def find(edge: Any): Option[Edge[V, Unit]] = edge match
    case Queries.Pair(first, second, ()) =>
      (indexing.index(first), indexing.index(second)) match
        case (Some(first), Some(second)) if shape.linked(first, second) =>
          Some(this.edge(first, second))
        case (Some(first), Some(second)) if shape.linked(second, first) =>
          Some(this.edge(second, first))
        case _ => None
    case _ => None

  override def degree(vertex: Any): Option[Int] = Some:
    indexing
      .index(vertex)
      .fold(0)(index => shape.arcsFrom(index).size + shape.arcsInto(index).size)

  /** Every edge of the shape. */
  def edges: Iterable[Edge[V, Unit]] = (0 until shape.order).flatMap: source =>
    shape.arcsFrom(source).map(edge(source, _))

  /**
    * @return
    *   The edge between the vertices numbered [[first]] and [[second]].
    */
  private def edge(first: Int, second: Int): Edge[V, Unit] = Edge(
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
  * The queries about the unlabelled directed graph whose edges are the arcs of
  * a [[Shape]], answered from the shape alone.
  */
private[kneiphof] final class DirectedShaping[V, M : Multiplicity as M]
  (indexing: Indexing[V], shape: Shape)
  extends Queries[V, Unit, M, Arc[V, Unit]]:

  override lazy val vertexValues: ValueSet[V] =
    ValueSet.from((0 until shape.order).map(indexing.vertex))

  override def hasVertex(vertex: Any): Boolean = indexing
    .index(vertex)
    .isDefined

  override def multiplicity(edge: Any): M = edge match
    case Queries.Arrow(source, target, ()) =>
      (indexing.index(source), indexing.index(target)) match
        case (Some(source), Some(target)) if shape.linked(source, target) =>
          M.one
        case _ => M.zero
    case _ => M.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[Arc[V, Unit]] =
    ValueSet.from:
      indexing
        .index(vertex)
        .toSeq
        .flatMap: index =>
          shape.arcsFrom(index).map(arc(index, _)) ++
            shape.arcsInto(index).map(arc(_, index))

  override def size: Option[Int] = Some(shape.size)

  override def find(edge: Any): Option[Arc[V, Unit]] = edge match
    case Queries.Arrow(source, target, ()) =>
      (indexing.index(source), indexing.index(target)) match
        case (Some(source), Some(target)) if shape.linked(source, target) =>
          Some(arc(source, target))
        case _ => None
    case _ => None

  override def degree(vertex: Any): Option[Int] = Some:
    indexing
      .index(vertex)
      .fold(0)(index => shape.arcsFrom(index).size + shape.arcsInto(index).size)

  /** Every arc of the shape. */
  def edges: Iterable[Arc[V, Unit]] = (0 until shape.order).flatMap: source =>
    shape.arcsFrom(source).map(arc(source, _))

  /** @return The arc from the vertex numbered [[source]] to [[target]]. */
  private def arc(source: Int, target: Int): Arc[V, Unit] = DirectedEdge(
    indexing.vertex(source),
    indexing.vertex(target),
  )
