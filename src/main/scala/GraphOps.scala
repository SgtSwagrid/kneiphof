package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.incidence.IncidenceStructure
import java.util.NoSuchElementException

/**
  * The operations shared by every kind of graph, which preserve its kind.
  *
  * Operations which add edges depend on the kind, so they are defined by each
  * kind instead: see [[HypergraphBase]], [[GraphBase]],
  * [[DirectedHypergraphBase]] and [[DirectedGraphBase]].
  *
  * @tparam X
  *   The kind of edge.
  * @tparam G
  *   The kind of graph.
  */
trait GraphOps[
  +V,
  +E,
  M,
  +X[+v, +e] <: AnyEdge[v, e, X],
  +G[+v, +e] <: GraphOps[v, e, M, X, G],
] extends AnyGraph[V, E, M]:
  this: G[V, E] =>

  /** A vertex of this graph, from which its neighbourhood can be explored. */
  type Vertex <: VertexOps

  /** An edge of this graph, from which its vertices can be explored. */
  type Edge <: EdgeOps

  override private[kneiphof] def structure: IncidenceStructure[V, M, X[V, E]]

  override lazy val edgeValues: ValueMap[X[V, E], M] =
    ValueMap.from(structure.edges)

  /** @return The values of the edges incident to [[vertex]]. */
  def incidentEdgeValues[A : NotHandle](vertex: A): ValueSet[X[V, E]] =
    structure.incidentEdgeValues(vertex)

  /** @return The empty graph of the same kind. */
  def empty: G[Nothing, Nothing]

  /** @return This graph with [[vertex]] added, if it's not already present. */
  def addVertex[W >: V, A <: W : NotHandle](vertex: A): G[W, E]

  /** @return This graph without [[vertex]] and its incident edges. */
  def removeVertex[A : NotHandle](vertex: A): G[V, E]

  /**
    * @return
    *   This graph with the multiplicity of [[edge]] set to [[multiplicity]], or
    *   this graph unchanged if it doesn't contain [[edge]].
    */
  def withMultiplicity[A : NotHandle](edge: A, multiplicity: M): G[V, E]

  /**
    * @return
    *   The image of this graph, with each vertex transformed by [[vertexMap]]
    *   and each label by [[labelMap]]. Vertices with the same image merge, and
    *   the multiplicities of edges with the same image are summed.
    */
  def map[W : NotHandle, F](vertexMap: V => W, labelMap: E => F): G[W, F]

  /**
    * @return
    *   This graph with every edge stored. Every operation computes its result
    *   only as it's queried, so a graph which will be queried repeatedly, or
    *   built from a long chain of operations, should be materialised.
    */
  def materialise: G[V, E]

  /** @return The vertex with the given [[value]], if it's in this graph. */
  def vertex[A : NotHandle](value: A): Option[Vertex]

  /** @return The edge with the given [[value]], if it's in this graph. */
  def edge[A : NotHandle](value: A): Option[Edge]

  /** The name of this kind of graph. */
  protected def kindName: String

  /**
    * @return
    *   The subgraph induced by the edges satisfying [[keep]], with only the
    *   vertices incident to them.
    */
  protected def edgeSubgraph(keep: X[V, E] => Boolean): G[V, E]

  /**
    * @return
    *   The subgraph with only the vertices satisfying [[keep]], and the edges
    *   between them satisfying [[keepEdge]], if given.
    */
  protected def subgraph
    (
      keep: V => Boolean,
      keepEdge: Option[X[V, E] => Boolean],
    )
    : G[V, E]

  /**
    * @return
    *   The handles on the edges incident to [[vertex]], made without looking
    *   each up again.
    */
  private[kneiphof] def edgesAt(vertex: Any): Iterable[Edge]

  /** The handles on every edge of this graph. */
  private[kneiphof] def allEdges: Iterable[Edge]

  /** @return The edges incident to [[vertex]], each with its multiplicity. */
  private[kneiphof] def incidentEdges(vertex: Any): Iterable[(X[V, E], M)] =
    structure.incidentEdges(vertex)

  /** The vertices of this graph. */
  def vertices: Set[Vertex] = vertexValues.flatMap(vertex).toSet

  /** The edges of this graph. */
  def edges: Set[Edge] = allEdges.toSet

  /**
    * @return
    *   The vertex with the given [[value]].
    * @throws NoSuchElementException
    *   If [[value]] isn't a vertex of this graph.
    */
  def apply[A : NotHandle](value: A): Vertex = vertex(value).getOrElse:
    throw NoSuchElementException(s"$value is not a vertex of this graph.")

  /**
    * @return
    *   The number of edges incident to [[vertex]], counting every copy of each
    *   edge, and counting loops twice.
    */
  def degree[A : NotHandle](vertex: A): Int = structure.degree(vertex)

  /** @return This graph with all of [[vertices]] added. */
  def addVertices[W >: V, A <: W : NotHandle](vertices: Iterable[A]): G[W, E] =
    vertices.foldLeft(this: G[W, E])(_.addVertex(_))

  /** @return This graph without any of [[vertices]] or their incident edges. */
  def removeVertices[A : NotHandle](vertices: Iterable[A]): G[V, E] = vertices
    .foldLeft(this: G[V, E])(_.removeVertex(_))

  /** @return This graph without any copies of [[edge]]. */
  def removeEdge[A : NotHandle](edge: A): G[V, E] =
    withMultiplicity(edge, algebra.zero)

  /**
    * @return
    *   The subgraph induced by the vertices satisfying [[predicate]], with
    *   every edge between them.
    */
  def filterVertices(predicate: V => Boolean): G[V, E] =
    subgraph(predicate, None)

  /**
    * @return
    *   The subgraph induced by [[vertices]], with every edge between them.
    */
  def induced[A : NotHandle](vertices: Iterable[A]): G[V, E] =
    filterVertices(ValueSet.from(vertices).contains)

  /**
    * @return
    *   The spanning subgraph with every vertex, but only the edges satisfying
    *   [[predicate]].
    */
  def filterEdges(predicate: X[V, E] => Boolean): G[V, E] =
    subgraph(_ => true, Some(predicate))

  /**
    * @return
    *   The subgraph induced by the edges satisfying [[predicate]], with only
    *   the vertices incident to them.
    */
  def edgeInduced(predicate: X[V, E] => Boolean): G[V, E] =
    edgeSubgraph(predicate)

  /**
    * @return
    *   The image of this graph, with each vertex transformed by [[transform]].
    *   Vertices with the same image merge, and so do their edges (see [[map]]).
    */
  def mapVertices[W : NotHandle](transform: V => W): G[W, E] =
    map(transform, identity)

  /** @return This graph, with each edge label transformed by [[transform]]. */
  def mapEdges[F](transform: E => F): G[V, F] = map(identity, transform)

  /**
    * @return
    *   This graph, with its vertices renumbered from `0` in the order given by
    *   their [[Ordering]].
    */
  def indexed[W >: V : Ordering]: G[Int, E] =
    mapVertices(vertexValues.toSeq.sorted[W].zipWithIndex.toMap)

  override def toString: String =
    (edgeValues.map(show) ++ isolated.map(_.toString)).mkString(
      s"$kindName(",
      ", ",
      ")",
    )

  /**
    * @return
    *   The edge of this graph equal to [[edge]], if there is one, found among
    *   the edges at one of its ends.
    */
  private[kneiphof] def found(edge: Any): Option[X[V, E]] = structure.find(edge)

  /** The vertices of this graph without any incident edges. */
  private def isolated: Iterable[V] =
    vertexValues.filter(incidentEdgeValues(_).isEmpty)

  /** @return The text of [[edge]], with its multiplicity unless that's `1`. */
  private def show(edge: Any, multiplicity: M): String =
    if multiplicity.count == 1 then edge.toString
    else s"$edge × ${ multiplicity.count }"

  /** The operations on a vertex of this graph. */
  trait VertexOps extends Handle:

    /** The value of this vertex. */
    def value: V

    /** The graph this vertex belongs to. */
    override def graph: GraphOps.this.type = GraphOps.this

    /** The edges incident to this vertex. */
    def edges: Set[Edge] = edgesAt(value).toSet

    /**
      * The number of edges incident to this vertex, counting every copy of each
      * edge, and counting loops twice.
      */
    def degree: Int = GraphOps.this.degree(value)

    /** The vertices joined to this vertex by an edge. */
    def neighbours: Set[Vertex] = incidentEdgeValues(value)
      .flatMap: edge =>
        edge.vertexValues.filter(v => v != value || edge.occurrences(value) > 1)
      .flatMap(vertex)
      .toSet

    override private[kneiphof] def onEdge: Boolean = false

  /** The operations on an edge of this graph. */
  trait EdgeOps extends Handle:

    /** The value of this edge. */
    def value: X[V, E]

    /** The graph this edge belongs to. */
    override def graph: GraphOps.this.type = GraphOps.this

    /** The label of this edge. */
    def label: E = value.label

    /** The vertices incident to this edge. */
    def vertices: Set[Vertex] = value.vertexValues.flatMap(vertex).toSet

    /** The number of copies of this edge in the graph. */
    def multiplicity: M = GraphOps.this.multiplicity(value)

    override private[kneiphof] def onEdge: Boolean = true
