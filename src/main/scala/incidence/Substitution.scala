package com.alecdorrington
package kneiphof
package incidence

/**
  * The graph in which each vertex `v` of the graph whose incidence structure is
  * [[outer]] is replaced by the graph whose structure is `inner(v)`, and each
  * edge by its copies between the replacements of its ends (see
  * [[HypergraphBase.flatMapVertices]]), answered without building the copies.
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
]
  (
    outer: IncidenceStructure[V, M, X[V, E]],
    inner: V => IncidenceStructure[W, M, X[W, E]],
    owner: Option[Any => Option[V]],
  )
  extends IncidenceStructure[W, M, X[W, E]]:

  override lazy val vertexValues: ValueSet[W] = parts
    .values
    .foldLeft(ValueSet.empty[W])(_ | _.vertexValues)

  override def hasVertex(vertex: Any): Boolean =
    owners(vertex).exists(part(_).hasVertex(vertex))

  override def vertexValue(vertex: Any): Option[W] = owners(vertex)
    .toSeq
    .reverseIterator
    .flatMap(part(_).vertexValue(vertex))
    .nextOption()

  override def order: Int =
    if disjoint then IncidenceStructure.total(parts.values.map(_.order))
    else vertexValues.size

  /**
    * The order, known when the replacements are known to be disjoint, few, and
    * of known orders.
    */
  override def maxOrder: Option[Long] =
    if !IncidenceStructure.knownAtMost(outer, IncidenceStructure.indexable) then
      None
    else IncidenceStructure.knownTotal(parts.values.map(_.maxOrder))

  override def knownOrder: Option[Long] =
    if owner.isEmpty ||
      !outer.knownOrder.exists(_ <= IncidenceStructure.indexable)
    then None
    else IncidenceStructure.knownTotal(parts.values.map(_.knownOrder))

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
    * The edges at [[vertex]] with their multiplicities, counted directly when
    * the replacements are disjoint, since then each copy of an outer edge at
    * [[vertex]] is found exactly once.
    */
  override def incidentEdges(vertex: Any): Iterable[(X[W, E], M)] =
    if !disjoint then
      incidentEdgeValues(vertex).toSeq.map(edge => edge -> multiplicity(edge))
    else
      IncidenceStructure.combine(
        owners(vertex)
          .iterator
          .flatMap: owner =>
            part(owner).incidentEdges(vertex).iterator ++
              outer
                .incidentEdges(owner)
                .iterator
                .flatMap((edge, multiplicity) =>
                  copiesAt(edge, vertex).map(_ -> multiplicity),
                ),
      )(_ + _)

  /**
    * The number of edges, which can be counted from [[outer]] when the
    * replacements are disjoint and no edge of [[outer]] repeats a vertex, since
    * then no two copies coincide. Otherwise, every edge is counted.
    */
  override lazy val size: Int =
    val counted =
      if !disjoint then None
      else
        outer
          .edges
          .foldLeft(Option(IncidenceStructure.total(parts.values.map(_.size)))):
            case (total, (edge, multiplicity)) => total.flatMap: sum =>
                Option.when(simple(edge)):
                  Math.addExact(
                    sum,
                    Math.toIntExact(copies(edge -> multiplicity)),
                  )
    counted.getOrElse(super.size)

  /**
    * The degree of [[vertex]], which can be counted from its own replacement
    * and the outer edges at its owner when the replacements are disjoint and
    * those edges repeat no vertex, since then no two copies coincide.
    */
  override def degree(vertex: Any): Int =
    if !hasVertex(vertex) then 0
    else
      owners(vertex).toList match
        case List(owner) if disjoint =>
          val outerEdges = outer.incidentEdgeValues(owner)
          if !outerEdges.forall(simple) then super.degree(vertex)
          else
            IncidenceStructure.total:
              Iterator(part(owner).degree(vertex)) ++
                outerEdges.iterator.map(copiesThrough(_, owner))
        case _ => super.degree(vertex)

  /** The replacement of each vertex of [[outer]]. */
  private lazy val parts: Map[V, IncidenceStructure[W, M, X[W, E]]] = outer
    .vertexValues
    .map(vertex => vertex -> inner(vertex))
    .toMap

  /**
    * The owners of each vertex, in the order of [[parts]], if they aren't known
    * from [[owner]].
    */
  private lazy val index: Map[Any, Seq[V]] = parts
    .toSeq
    .flatMap((owner, part) => part.vertexValues.map((_: Any) -> owner))
    .groupMap((vertex, _) => vertex)((_, owner) => owner)

  /** `true` if and only if no two replacements share a vertex. */
  private lazy val disjoint: Boolean = owner.isDefined ||
    index.values.forall(_.size == 1)

  /** @return The replacement of [[vertex]]. */
  private def part(vertex: V): IncidenceStructure[W, M, X[W, E]] = parts
    .getOrElse(vertex, inner(vertex))

  /**
    * @return
    *   The vertices of [[outer]] whose replacements contain [[vertex]].
    */
  private def owners(vertex: Any): Iterable[V] =
    owner.fold(index.getOrElse(vertex, Nil))(_(vertex).toList)

  /** @return The multiplicity of [[edge]] within single replacements. */
  private def within(edge: AnyEdge[?, ?, ?]): M = commonOwners(edge)
    .map(part(_).multiplicity(edge))
    .foldLeft(M.zero)(_ + _)

  /** @return The vertices whose replacements contain every end of [[edge]]. */
  private def commonOwners(edge: AnyEdge[?, ?, ?]): Iterable[V] =
    val ends = edge.vertexValues.toSeq
    owners(ends.head).filter(owner => ends.tail.forall(part(owner).hasVertex))

  /** @return The multiplicity of [[edge]] among the copies of outer edges. */
  private def between(edge: AnyEdge[?, ?, ?]): M =
    candidates(edge).foldLeft(M.zero): (sum, outerEdge) =>
      sum + outer.multiplicity(outerEdge).scale(copiesOf(outerEdge, edge))

  /** @return The outer edges which might have [[edge]] as a copy. */
  private def candidates(edge: AnyEdge[?, ?, ?]): Iterable[X[V, E]] =
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
  private def copiesThrough(outerEdge: X[V, E], owner: V): Int = Math
    .toIntExact:
      IncidenceStructure.product:
        outerEdge
          .vertexValues
          .iterator
          .filter(_ != owner)
          .map(part(_).order.toLong) ++
          Iterator(outer.multiplicity(outerEdge).count.toLong)

  /**
    * @return
    *   The number of copies of the outer edge in [[entry]] with its
    *   multiplicity, counting every copy of each.
    */
  private def copies(entry: (X[V, E], M)): Long =
    val (edge, multiplicity) = entry
    IncidenceStructure.product:
      edge.vertexValues.iterator.map(part(_).order.toLong) ++
        Iterator(multiplicity.count.toLong)

  /**
    * @return
    *   `true` if and only if [[edge]] has at least two ends, and no vertex
    *   occurs at more than one of them, so that its copies can't coincide with
    *   each other or with an edge inside a replacement.
    */
  private def simple(edge: X[V, E]): Boolean = edge.vertexValues.size >= 2 &&
    edge.vertexValues.forall(edge.occurrences(_) == 1)
