package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceStructure
import scala.compiletime.deferred
import scala.util.hashing.MurmurHash3

/**
  * A graph of any kind, with vertices of type [[V]], edge labels of type [[E]]
  * and edge multiplicities in [[M]].
  *
  * Each edge value has a multiplicity, and is present exactly when its
  * multiplicity isn't `zero` (see [[Multiplicity]]).
  *
  * Two graphs are equal when they belong to the same family, being both
  * directed or both undirected, with the same multiplicities, and have the same
  * vertices and the same multiplicity for every edge. This is how Scala's
  * collections compare within a family, so that `List() == Vector()` but
  * `Set() != List()`. So a graph equals the hypergraph with its edges, and a
  * lazy graph equals its materialisation, but an edgeless graph doesn't equal
  * an edgeless directed graph or multigraph.
  *
  * A graph known to have more vertices than an [[Int]] can count can only be
  * compared with a graph built the same way, such as an equal grid, or with one
  * known or found to have fewer vertices. Comparing it with any other graph
  * throws an [[UnsupportedOperationException]], since that would mean listing
  * more vertices than a set can hold. Such graphs hash only their order, so
  * they can be kept in hash sets and maps, alongside graphs built the same way.
  */
trait AnyGraph[+V, +E, M]:

  /** The algebra of edge multiplicities. */
  protected given algebra: Multiplicity[M] = deferred

  /** The incidence structure of this graph, which answers every query about it. */
  private[kneiphof] def structure: IncidenceStructure[V, M, AnyEdge[?, ?, ?]]

  /** `true` if and only if this graph's edges are directed. */
  private[kneiphof] def directed: Boolean

  /** The values of the vertices of this graph. */
  def vertexValues: ValueSet[V] = structure.vertexValues

  /** The multiplicity of each edge value present in this graph. */
  def edgeValues: ValueMap[Any, M]

  /** @return `true` if and only if [[vertex]] is a vertex of this graph. */
  def hasVertex[A : NotHandle](vertex: A): Boolean = structure.hasVertex(vertex)

  /** @return `true` if and only if [[edge]] is an edge of this graph. */
  def hasEdge[A : NotHandle](edge: A): Boolean = multiplicity(edge).nonZero

  /** @return The multiplicity of [[edge]], which is `zero` if it's absent. */
  def multiplicity[A : NotHandle](edge: A): M = structure.multiplicity(edge)

  /** The number of vertices in this graph. */
  def order: Int = structure.order

  /** The number of edges in this graph, counting every copy of each edge. */
  def size: Int = structure.size

  /**
    * Graphs too large to list are compared as the trait's documentation says.
    *
    * @return
    *   `true` if and only if [[that]] is a graph of the same family, with the
    *   same vertices and the same multiplicity for every edge.
    * @throws UnsupportedOperationException
    *   If either graph is known to have more vertices than an [[Int]] can
    *   count, the two aren't built the same way, and the other isn't known or
    *   found to have fewer vertices, since then they could only be compared by
    *   listing more vertices than a set can hold. Two lazy graphs whose orders
    *   aren't known are compared by listing them, however large they are.
    */
  override def equals(that: Any): Boolean = that match
    case that: AnyGraph[?, ?, ?] => sameGraph(that)
    case _                       => false

  /**
    * A graph known to have more vertices than an [[Int]] can count hashes only
    * its order, since its vertices can't be listed. Equal graphs have equal
    * orders, so this agrees with equality.
    */
  override def hashCode: Int = structure.knownOrder match
    case Some(order) if order > Int.MaxValue => order.##
    case _                                   => AnyGraph.hash(
        vertexValues,
        edgeValues.map((edge, multiplicity) =>
          AnyGraph.shape(edge) -> multiplicity,
        ),
      )

  /**
    * Graphs built the same way, such as equal shapes, are equal without listing
    * them. Otherwise, orders are compared first where they're known, since many
    * graphs count their vertices without listing them. Sizes aren't, since many
    * graphs count their edges only by listing them.
    *
    * @return
    *   `true` if and only if this graph and [[that]] belong to the same family,
    *   and have the same vertices and the same multiplicity for every edge.
    * @throws UnsupportedOperationException
    *   If the two graphs can only be compared by listing more vertices than an
    *   [[Int]] can count.
    */
  protected final def sameGraph(that: AnyGraph[?, ?, ?]): Boolean =
    (this eq that) ||
    directed == that.directed && algebra == that.algebra &&
    (structure.sameAs(that.structure) || sameValues(that))

  /**
    * @return
    *   `true` if and only if this graph and [[that]] have the same vertices and
    *   the same multiplicity for every edge, compared by listing them.
    */
  private def sameValues(that: AnyGraph[?, ?, ?]): Boolean =
    val orders = Seq(
      structure.knownOrder,
      that.structure.knownOrder,
    ).flatten
    if orders.distinct.size > 1 then false
    else
      orders.find(_ > Int.MaxValue) match
        case None => vertexValues == that.vertexValues &&
          edgeValues == that.edgeValues
        case Some(order) =>
          if Seq(this, that).exists(_.fewerThan(order)) then false
          else
            throw UnsupportedOperationException(
              s"A graph of $order vertices is too large to compare by listing them.",
            )

  /**
    * @return
    *   `true` if and only if this graph, whose order isn't known, is known to
    *   have fewer than [[order]] vertices, which is more than an [[Int]] can
    *   count. With no bound on its order, it's found by listing at most an
    *   [[Int]]'s worth of its vertices.
    */
  private def fewerThan(order: Long): Boolean = structure.knownOrder.isEmpty &&
    structure
      .maxOrder
      .fold(!structure.vertexIterator.drop(Int.MaxValue).hasNext)(_ < order)

/** The companion object for [[AnyGraph]]. */
object AnyGraph:

  /**
    * Edges hash themselves this way too, as the graph of one edge, so this
    * mustn't depend on the hashes of the edges themselves.
    *
    * @return
    *   The hash of a graph with the given [[vertices]], and edges with the
    *   given [[shapes]] (see [[AnyEdge.shapeHash]]) and multiplicities.
    */
  private[kneiphof] def hash
    (
      vertices: ValueSet[?],
      shapes: Iterable[(Int, Any)],
    )
    : Int = (vertices, MurmurHash3.unorderedHash(shapes)).##

  /** @return The hash of [[edge]] which doesn't depend on its hash code. */
  private def shape(edge: Any): Int = edge match
    case edge: AnyEdge[?, ?, ?] => edge.shapeHash
    case _                      => edge.##
