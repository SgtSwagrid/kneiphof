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

  override def equals(that: Any): Boolean = that match
    case that: AnyGraph[?, ?, ?] => sameGraph(that)
    case _                       => false

  override def hashCode: Int = AnyGraph.hash(
    vertexValues,
    edgeValues.map((edge, multiplicity) => AnyGraph.shape(edge) -> multiplicity),
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
    (structure.knownOrder, that.structure.knownOrder) match
      case (Some(order), Some(other)) if order != other => false
      case (Some(order), _) if order > Int.MaxValue     =>
        throw UnsupportedOperationException(
          s"A graph of $order vertices is too large to compare by listing them.",
        )
      case _ => vertexValues == that.vertexValues &&
        edgeValues == that.edgeValues

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
