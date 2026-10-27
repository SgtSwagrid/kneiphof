package com.alecdorrington
package kneiphof
package incidence

import scala.annotation.tailrec
import scala.compiletime.deferred

/**
  * The vertices of a graph, its edges with their multiplicities in [[M]], and
  * the edges incident to each vertex, whatever the graph's kind.
  *
  * Every graph is a kind wrapped around an incidence structure. An [[Explicit]]
  * structure stores its edges, and every other structure computes them from its
  * operands, answering each question without computing the rest.
  *
  * @tparam X
  *   The type of the edges.
  */
private[kneiphof] trait IncidenceStructure[+V, M, +X <: AnyEdge[?, ?, ?]]:

  /** The algebra of edge multiplicities. */
  protected given algebra: Multiplicity[M] = deferred

  /** The values of the vertices. */
  def vertexValues: ValueSet[V]

  /** @return `true` if and only if [[vertex]] is a vertex. */
  def hasVertex(vertex: Any): Boolean

  /**
    * @return
    *   The vertex equal to [[vertex]], if there is one, found without listing
    *   every vertex where the structure can.
    */
  def vertexValue(vertex: Any): Option[V] = vertexValues.get(vertex)

  /** The number of vertices. */
  def order: Int = vertexValues.size

  /**
    * The number of vertices, if it's known without listing them, as for stored
    * graphs, shapes and products. It's counted in a [[Long]], so it never
    * overflows, and it never fails, so it can choose between ways of answering
    * a query.
    */
  def knownOrder: Option[Long] = None

  /** The vertices, listed only as they're needed, where the structure can. */
  def vertexIterator: Iterator[V] = vertexValues.iterator

  /**
    * The edges at each vertex in turn, listed only as they're needed, so that
    * each edge appears once for each of its distinct ends. Where only their
    * number matters, roughly, this stands in for [[edges]], which may first
    * have to number every vertex.
    */
  def incidences: Iterator[Any] =
    vertexIterator.flatMap(incidentEdges(_).iterator.map((edge, _) => edge))

  /**
    * @return
    *   `true` if this structure is known to be the same graph as [[that]] from
    *   how they're built, as for equal shapes, without listing either. If it's
    *   `false`, they may still be the same.
    */
  def sameAs(that: IncidenceStructure[?, ?, ?]): Boolean = this eq that

  /**
    * The structures of the two graphs whose Cartesian product this is, if it is
    * one, so that products can be compared by their factors.
    */
  def factors
    : Option[(IncidenceStructure[?, ?, ?], IncidenceStructure[?, ?, ?])] = None

  /** @return The multiplicity of [[edge]], which is `zero` if it's absent. */
  def multiplicity(edge: Any): M

  /** @return The edges incident to [[vertex]], each with its multiplicity. */
  def incidentEdges(vertex: Any): Iterable[(X, M)]

  /** @return The values of the edges incident to [[vertex]]. */
  def incidentEdgeValues(vertex: Any): ValueSet[X] =
    ValueSet.from(incidentEdges(vertex).map((edge, _) => edge))

  /**
    * @return
    *   The edge equal to [[edge]], if there is one, found among the edges at
    *   one of its ends.
    */
  def find(edge: Any): Option[X] = IncidenceStructure
    .ends(edge)
    .headOption
    .flatMap(incidentEdgeValues(_).get(edge))

  /**
    * Every edge, once, with its multiplicity, each found at whichever of its
    * ends comes first among the vertices, however the edge is written.
    */
  def edges: Iterator[(X, M)] =
    val positions = vertexValues
      .iterator
      .zipWithIndex
      .map((vertex, position) => (vertex: Any) -> position)
      .toMap
    vertexValues
      .iterator
      .zipWithIndex
      .flatMap: (vertex, position) =>
        incidentEdges(vertex)
          .iterator
          .filter((edge, _) =>
            edge.vertexValues.forall(positions.get(_).forall(_ >= position)),
          )

  /** The number of edges, counting every copy of each. */
  def size: Int =
    IncidenceStructure.total(edges.map((_, multiplicity) => multiplicity.count))

  /**
    * @return
    *   The number of edges incident to [[vertex]], counting every copy of each
    *   edge, and counting loops twice.
    */
  def degree(vertex: Any): Int =
    IncidenceStructure.degree(vertex, incidentEdges(vertex))

  /** The edges with only one distinct end, such as loops. */
  def loops: Iterator[X] = edges.collect:
    case (edge, _) if edge.vertexValues.size == 1 => edge

  /** `true` if and only if every edge is stored, rather than computed. */
  def stored: Boolean = false

  /**
    * `true` if and only if the vertices and edges can be listed in time
    * proportional to their number, as for stored graphs and shapes, so that a
    * few of them can be copied cheaply.
    */
  def listable: Boolean = stored

  /** This structure as a patch, to which single changes can be made. */
  def patch: Patch[V, M, X] = Patch(this)

  /** This structure with every edge stored. */
  def explicit: Explicit[V, M, X] = Explicit.from(this)

/** The companion object for [[IncidenceStructure]]. */
private[kneiphof] object IncidenceStructure:

  /**
    * Matches any undirected edge with two ends, as its ends and label, whatever
    * class it is, since equal edges must be found alike.
    */
  object Pair:

    def unapply(edge: Any): Option[(Any, Any, Any)] = edge match
      case edge: Hyperedge[?, ?] => edge.ends match
          case Seq(first, second) => Some((first, second, edge.label))
          case _                  => None
      case _ => None

  /**
    * Matches any directed edge with one source and one target, as its source,
    * target and label, whatever class it is.
    */
  object Arrow:

    def unapply(edge: Any): Option[(Any, Any, Any)] = edge match
      case edge: DirectedHyperedge[?, ?, ?] => (edge.tail, edge.head) match
          case (Seq(source), Seq(target)) => Some((source, target, edge.label))
          case _                          => None
      case _ => None

  /** @return The distinct vertices of [[edge]], if it's an edge. */
  def ends(edge: Any): ValueSet[Any] = edge match
    case edge: AnyEdge[?, ?, ?] => edge.vertexValues
    case _                      => ValueSet.empty

  /**
    * @return
    *   The number of vertices of [[structure]] if it's known without listing
    *   them, and otherwise [[Long.MaxValue]], so that the structures known to
    *   be smaller come first.
    */
  def orderBound(structure: IncidenceStructure[?, ?, ?]): Long = structure
    .knownOrder
    .getOrElse(Long.MaxValue)

  /**
    * @return
    *   `true` if and only if [[structure]] is known, without listing its
    *   vertices, to have at most [[bound]] of them.
    */
  def knownAtMost
    (
      structure: IncidenceStructure[?, ?, ?],
      bound: Int,
    )
    : Boolean = structure.knownOrder.exists(_ <= bound)

  /**
    * @return
    *   `true` if and only if [[first]] has at most as many elements as
    *   [[second]], found by taking one from each until either runs out.
    */
  @tailrec
  def noLonger
    (
      first: Iterator[Any],
      second: Iterator[Any],
    )
    : Boolean =
    if !first.hasNext then true
    else if !second.hasNext then false
    else
      first.next()
      second.next()
      noLonger(first, second)

  /**
    * The most vertices a structure can have for an operation to index its
    * vertices, rather than asking it about each.
    */
  val indexable: Int = 1024

  /**
    * @return
    *   The given [[edges]], each with its multiplicity, combining the
    *   multiplicities of equal edges by [[operation]].
    */
  def combine[X, M]
    (edges: IterableOnce[(X, M)])
    (operation: (M, M) => M)
    : ValueMap[X, M] = edges
    .iterator
    .foldLeft(ValueMap.empty[X, M]):
      case (sums, (edge, multiplicity)) => sums.withEntry(
          edge,
          sums.get(edge).fold(multiplicity)(operation(_, multiplicity)),
        )

  /**
    * @return
    *   The sum of [[orders]] if they're all known and it fits in a [[Long]].
    */
  def knownTotal(orders: IterableOnce[Option[Long]]): Option[Long] = orders
    .iterator
    .foldLeft(Option(0L)): (total, order) =>
      for
        sum  <- total
        next <- order if next <= Long.MaxValue - sum
      yield sum + next

  /** @return The product of [[factors]], failing rather than overflowing. */
  def product(factors: IterableOnce[Long]): Long = factors
    .iterator
    .foldLeft(1L)(Math.multiplyExact)

  /** @return The sum of [[counts]], failing rather than overflowing. */
  def total(counts: IterableOnce[Int]): Int = counts
    .iterator
    .foldLeft(0)(Math.addExact)

  /**
    * @return
    *   The degree of [[vertex]] in a graph whose edges at [[vertex]] are
    *   [[incident]], counting every copy of each edge, and loops twice.
    */
  def degree[M : Multiplicity]
    (
      vertex: Any,
      incident: IterableOnce[(AnyEdge[?, ?, ?], M)],
    )
    : Int = total:
    incident
      .iterator
      .map((edge, multiplicity) =>
        Math.multiplyExact(
          edge.occurrences(vertex),
          multiplicity.count,
        ),
      )

  /**
    * @return
    *   The number of edges gained from [[before]] to [[after]], counting every
    *   copy of each, where the distinct [[edges]] include every edge whose
    *   multiplicity differs.
    */
  def changes[M : Multiplicity]
    (
      after: IncidenceStructure[?, M, ?],
      before: IncidenceStructure[?, M, ?],
      edges: IterableOnce[Any],
    )
    : Int = total:
    edges
      .iterator
      .map(edge =>
        after.multiplicity(edge).count - before.multiplicity(edge).count,
      )
