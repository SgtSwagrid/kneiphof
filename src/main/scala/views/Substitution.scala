package com.alecdorrington
package kneiphof
package views

/**
  * The queries about the graph in which each vertex `v` of [[outer]] is
  * replaced by the graph `inner(v)`, and each edge by its copies between the
  * replacements of its ends (see `flatMapVertices`), answered without building
  * the copies.
  *
  * Each query only looks at the replacements containing the vertices it asks
  * about (their owners), and at the edges of [[outer]] incident to those
  * owners.
  *
  * @param owner
  *   The vertex of [[outer]] whose replacement contains a vertex, if this is
  *   known to be unique. Otherwise, each vertex's owners are found from the
  *   replacements themselves.
  */
private[kneiphof] final class Substitution[
  V,
  W,
  E,
  M : Multiplicity as M,
  X[+v, +e] <: AnyEdge[v, e, X],
  G[+v, +e] <: GraphOps[v, e, M, X, G],
]
  (
    outer: G[V, E],
    inner: V => G[W, E],
    owner: Option[Any => Option[V]],
  )
  extends Queries[W, E, M, X[W, E]]:

  override lazy val vertexValues: ValueSet[W] = parts
    .values
    .foldLeft(ValueSet.empty[W])(_ | _.vertexValues)

  override def hasVertex(vertex: Any): Boolean =
    owners(vertex).exists(part(_).hasVertex(vertex))

  override def multiplicity(edge: Any): M = edge match
    case edge: AnyEdge[?, ?, ?] => within(edge) + between(edge)
    case _                      => M.zero

  override def incidentEdgeValues(vertex: Any): ValueSet[X[W, E]] = ValueSet
    .from:
      owners(vertex)
        .toSeq
        .flatMap: owner =>
          part(owner).incidentEdgeValues(vertex) ++
            outer.incidentEdgeValues(owner).flatMap(copiesAt(_, vertex))

  /**
    * The number of edges, which can be counted from [[outer]] when the
    * replacements are disjoint and no edge of [[outer]] repeats a vertex, since
    * then no two copies coincide.
    */
  override def size: Option[Int] =
    Option.when(disjoint && outer.edgeValues.keys.forall(simple)):
      Math.toIntExact:
        parts.values.map(_.size.toLong).sum +
          outer.edgeValues.toSeq.map(copies).sum

  /**
    * The degree of [[vertex]], which can be counted from its own replacement
    * and the outer edges at its owner when the replacements are disjoint and
    * those edges repeat no vertex, since then no two copies coincide.
    */
  override def degree(vertex: Any): Option[Int] =
    if !hasVertex(vertex) then Some(0)
    else
      owners(vertex).toList match
        case List(owner) if disjoint =>
          val outerEdges = outer.incidentEdgeValues(owner)
          Option.when(outerEdges.forall(simple)):
            part(owner).degree(vertex) +
              outerEdges.toSeq.map(copiesThrough(_, owner)).sum
        case _ => None

  /** The replacement of each vertex of [[outer]]. */
  private lazy val parts: Map[V, G[W, E]] = outer
    .vertexValues
    .map(vertex => vertex -> inner(vertex))
    .toMap

  /** The owners of each vertex, if they aren't known from [[owner]]. */
  private lazy val index: Map[Any, ValueSet[V]] = parts
    .toSeq
    .flatMap((owner, part) => part.vertexValues.map((_: Any) -> owner))
    .groupMapReduce(_(0))(entry => ValueSet(entry(1)))(_ | _)

  /** `true` if and only if no two replacements share a vertex. */
  private def disjoint: Boolean = owner.isDefined ||
    index.values.forall(_.size == 1)

  /** @return The replacement of [[vertex]]. */
  private def part(vertex: V): G[W, E] = parts.getOrElse(vertex, inner(vertex))

  /**
    * @return
    *   The vertices of [[outer]] whose replacements contain [[vertex]].
    */
  private def owners(vertex: Any): Iterable[V] =
    owner.fold(index.getOrElse(vertex, ValueSet.empty[V]))(_(vertex).toList)

  /** @return The multiplicity of [[edge]] within single replacements. */
  private def within(edge: AnyEdge[?, ?, ?]): M = commonOwners(edge)
    .map(part(_).multiplicity(edge))
    .foldLeft(M.zero)(_ + _)

  /** @return The vertices whose replacements contain every end of [[edge]]. */
  private def commonOwners(edge: AnyEdge[?, ?, ?]): Iterable[V] =
    edge.vertexValues.toSeq match
      case Seq()         => outer.vertexValues
      case first +: rest =>
        owners(first).filter(owner => rest.forall(part(owner).hasVertex))

  /** @return The multiplicity of [[edge]] among the copies of outer edges. */
  private def between(edge: AnyEdge[?, ?, ?]): M =
    candidates(edge).foldLeft(M.zero): (sum, outerEdge) =>
      sum + outer.multiplicity(outerEdge).scale(copiesOf(outerEdge, edge))

  /** @return The outer edges which might have [[edge]] as a copy. */
  private def candidates(edge: AnyEdge[?, ?, ?]): Iterable[X[V, E]] =
    if edge.vertexValues.isEmpty then
      outer.edgeValues.keys.filter(_.vertexValues.isEmpty)
    else
      val owning = ValueSet.from(edge.vertexValues.flatMap(owners))
      ValueSet.from:
        owning
          .iterator
          .flatMap(outer.incidentEdgeValues)
          .filter(_.vertexValues.forall(owning.contains))

  /** @return The number of copies of [[outerEdge]] which equal [[edge]]. */
  private def copiesOf(outerEdge: X[V, E], edge: AnyEdge[?, ?, ?]): Int =
    outerEdge
      .substitute(owner =>
        part(owner).vertexValues.filter(edge.vertexValues.contains),
      )
      .count(_ == edge)

  /** @return The copies of [[outerEdge]] which have [[vertex]] as an end. */
  private def copiesAt(outerEdge: X[V, E], vertex: Any): Iterable[X[W, E]] =
    if simple(outerEdge) then
      outerEdge
        .vertexValues
        .filter(part(_).hasVertex(vertex))
        .flatMap: fixed =>
          outerEdge.substitute: owner =>
            if owner == fixed then part(owner).vertexValues.get(vertex).toList
            else part(owner).vertexValues
    else
      outerEdge
        .substitute(part(_).vertexValues)
        .filter(_.vertexValues.contains(vertex))

  /**
    * @return
    *   The number of copies of the simple [[outerEdge]] at a vertex of the
    *   replacement of its end [[owner]], counting every copy of each.
    */
  private def copiesThrough(outerEdge: X[V, E], owner: V): Int = outerEdge
    .vertexValues
    .filter(_ != owner)
    .toSeq
    .map(part(_).order)
    .product * outer.multiplicity(outerEdge).count

  /**
    * @return
    *   The number of copies of an outer edge with [[multiplicity]], counting
    *   every copy of each.
    */
  private def copies(entry: (X[V, E], M)): Long = entry(0)
    .vertexValues
    .toSeq
    .map(part(_).order.toLong)
    .product * entry(1).count

  /**
    * @return
    *   `true` if and only if [[edge]] has some end, and no vertex occurs at
    *   more than one of its ends.
    */
  private def simple(edge: X[V, E]): Boolean = edge.vertexValues.nonEmpty &&
    edge.vertexValues.forall(edge.occurrences(_) == 1)
