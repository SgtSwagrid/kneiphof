package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.views.{Queries, Viewing}
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

  override def edgeValues: ValueMap[X[V, E], M]

  /** @return The values of the edges incident to [[vertex]]. */
  def incidentEdgeValues(vertex: Any): ValueSet[X[V, E]]

  /** @return An empty graph of the same kind and representation. */
  def empty: G[Nothing, Nothing]

  /** @return This graph with [[vertex]] added, if it's not already present. */
  def addVertex[W >: V](vertex: W): G[W, E]

  /** @return This graph without [[vertex]] and its incident edges. */
  def removeVertex(vertex: Any): G[V, E]

  /**
    * @return
    *   This graph with the multiplicity of [[edge]] set to [[multiplicity]], or
    *   this graph unchanged if it doesn't contain [[edge]].
    */
  def withMultiplicity(edge: Any, multiplicity: M): G[V, E]

  /**
    * @return
    *   The image of this graph, with each vertex transformed by [[vertexMap]]
    *   and each label by [[labelMap]]. Vertices with the same image merge, and
    *   the multiplicities of edges with the same image are summed.
    */
  def map[W, F](vertexMap: V => W, labelMap: E => F): G[W, F]

  /**
    * @return
    *   This graph as a lazy graph, so that operations on it are computed only
    *   when they're queried, rather than built straight away.
    */
  def view: G[V, E]

  /** @return The vertex with the given [[value]], if it's in this graph. */
  def vertex(value: Any): Option[Vertex]

  /** @return The edge with the given [[value]], if it's in this graph. */
  def edge(value: Any): Option[Edge]

  /** The name of this kind of graph. */
  protected def kindName: String

  /**
    * @return
    *   The subgraph with only the vertices satisfying [[keep]] and the edges
    *   satisfying [[keepEdge]], and each remaining edge's multiplicity changed
    *   by [[reweigh]], which may depend on [[operands]]. Only the edges in
    *   [[changed]] may have their multiplicities changed, and only those in
    *   [[support]], if given, may remain.
    */
  protected def refined
    (
      keep: V => Boolean,
      keepEdge: Option[X[V, E] => Boolean],
      reweigh: (Any, M) => M,
      changed: => Iterable[Any],
      support: Option[() => Iterable[Any]],
      operands: AnyGraph[?, ?, ?]*,
    )
    : G[V, E]

  /** This graph as it's stored, which builds it if it's lazy. */
  private[kneiphof] def built: G[V, E] = this

  /** The queries about this graph, answered by this graph itself. */
  private[kneiphof] def viewing: Queries[V, E, M, X[V, E]] =
    viewing(Embedding.identity[M])

  /**
    * @return
    *   The queries about this graph, with its multiplicities read in [[N]] by
    *   [[embed]], answered by this graph itself.
    */
  private[kneiphof] def viewing[N : Multiplicity]
    (embed: Embedding[M, N])
    : Queries[V, E, N, X[V, E]] = Viewing[V, E, M, N, X, G](this, embed)

  /**
    * @return
    *   The edges incident to [[vertex]], each with its multiplicity, so that
    *   lazy graphs needn't look each one up again.
    */
  private[kneiphof] def incidentEdges(vertex: Any): Iterable[(X[V, E], M)] =
    incidentEdgeValues(vertex).toSeq.map(edge => edge -> multiplicity(edge))

  /** The vertices of this graph. */
  def vertices: Set[Vertex] = vertexValues.flatMap(vertex).toSet

  /** The edges of this graph. */
  def edges: Set[Edge] = edgeValues.keys.flatMap(edge).toSet

  /**
    * @return
    *   The vertex with the given [[value]].
    * @throws NoSuchElementException
    *   If [[value]] isn't a vertex of this graph.
    */
  def apply(value: Any): Vertex = vertex(value).getOrElse:
    throw NoSuchElementException(s"$value is not a vertex of this graph.")

  /**
    * @return
    *   The number of edges incident to [[vertex]], counting every copy of each
    *   edge, and counting loops twice.
    */
  def degree(vertex: Any): Int = incidentEdgeValues(vertex)
    .toSeq
    .map(edge =>
      Math.multiplyExact(
        edge.occurrences(vertex),
        multiplicity(edge).count,
      ),
    )
    .foldLeft(0)(Math.addExact)

  /** @return This graph with all of [[vertices]] added. */
  def addVertices[W >: V](vertices: Iterable[W]): G[W, E] =
    vertices.foldLeft(this: G[W, E])(_.addVertex(_))

  /** @return This graph without any of [[vertices]] or their incident edges. */
  def removeVertices(vertices: Iterable[Any]): G[V, E] =
    vertices.foldLeft(this: G[V, E])(_.removeVertex(_))

  /** @return This graph without any copies of [[edge]]. */
  def removeEdge(edge: Any): G[V, E] = withMultiplicity(edge, algebra.zero)

  /**
    * @return
    *   The difference of this graph and [[that]], which keeps the vertices of
    *   this graph and subtracts the multiplicity of each edge in [[that]].
    *   [[that]] may have any multiplicities which embed in [[M]], so it may
    *   also be an edge or chain, as in `g - (1 ~ 2)`.
    */
  infix def - [N]
    (that: AnyGraph[Any, Any, N])
    (using embed: Embedding[N, M])
    : G[V, E] =
    if that.eager then subtracted(that.edgeValues, embed)
    else
      refined(
        _ => true,
        None,
        (edge, multiplicity) => multiplicity - embed(that.multiplicity(edge)),
        that.edgeValues.keys,
        None,
        that,
      )

  /**
    * @return
    *   The intersection of this graph and [[that]], which keeps their common
    *   vertices and the meet of each edge's multiplicities. [[that]] may have
    *   any multiplicities which embed in [[M]], as for `-`.
    */
  infix def & [N]
    (that: AnyGraph[Any, Any, N])
    (using embed: Embedding[N, M])
    : G[V, E] = refined(
    that.hasVertex,
    None,
    (edge, multiplicity) => multiplicity & embed(that.multiplicity(edge)),
    edgeValues.keys,
    Some(() => that.edgeValues.keys),
    that,
  )

  /**
    * @return
    *   The subgraph induced by the vertices satisfying [[predicate]], with
    *   every edge between them.
    */
  def filterVertices(predicate: V => Boolean): G[V, E] = refined(
    predicate,
    None,
    (_, multiplicity) => multiplicity,
    Nil,
    None,
  )

  /**
    * @return
    *   The subgraph induced by [[vertices]], with every edge between them.
    */
  def induced(vertices: Iterable[Any]): G[V, E] =
    filterVertices(ValueSet.from(vertices).contains)

  /**
    * @return
    *   The spanning subgraph with every vertex, but only the edges satisfying
    *   [[predicate]].
    */
  def filterEdges(predicate: X[V, E] => Boolean): G[V, E] = refined(
    _ => true,
    Some(predicate),
    (_, multiplicity) => multiplicity,
    edgeValues.keys,
    None,
  )

  /**
    * @return
    *   The subgraph induced by the edges satisfying [[predicate]], with only
    *   the vertices incident to them.
    */
  def edgeInduced(predicate: X[V, E] => Boolean): G[V, E] = refined(
    incidentEdgeValues(_).exists(predicate),
    Some(predicate),
    (_, multiplicity) => multiplicity,
    edgeValues.keys,
    None,
  )

  /**
    * @return
    *   The image of this graph, with each vertex transformed by [[transform]].
    *   Vertices with the same image merge, and so do their edges (see [[map]]).
    */
  def mapVertices[W](transform: V => W): G[W, E] = map(transform, identity)

  /** @return This graph, with each edge label transformed by [[transform]]. */
  def mapEdges[F](transform: E => F): G[V, F] = map(identity, transform)

  /**
    * @return
    *   This graph, with its vertices renumbered from `0` in the order given by
    *   [[ordering]].
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
  private[kneiphof] def found(edge: Any): Option[X[V, E]] = Queries
    .ends(edge)
    .headOption
    .flatMap(incidentEdgeValues(_).get(edge))

  /**
    * @return
    *   This graph, with the multiplicities in [[edges]], read in [[M]] by
    *   [[embed]], subtracted one edge at a time.
    */
  private def subtracted[N]
    (
      edges: ValueMap[Any, N],
      embed: Embedding[N, M],
    )
    : G[V, E] = edges.foldLeft(this: G[V, E]):
    case (graph, (edge, multiplicity)) => graph.withMultiplicity(
        edge,
        graph.multiplicity(edge) - embed(multiplicity),
      )

  /** The vertices of this graph without any incident edges. */
  private def isolated: Iterable[V] =
    vertexValues.filter(incidentEdgeValues(_).isEmpty)

  /** @return [[edge]] with its multiplicity, unless that's `1`. */
  private def show(edge: Any, multiplicity: M): String =
    if multiplicity.count == 1 then edge.toString
    else s"$edge × ${ multiplicity.count }"

  /** The operations on a vertex of this graph. */
  trait VertexOps:

    /** The value of this vertex. */
    def value: V

    /** The graph this vertex belongs to. */
    def graph: GraphOps.this.type = GraphOps.this

    /** The edges incident to this vertex. */
    def edges: Set[Edge] = incidentEdgeValues(value).flatMap(edge).toSet

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

    override def equals(that: Any): Boolean = that match
      case that: VertexOps => (that.graph eq graph) && that.value == value
      case _               => false

    override def hashCode: Int = value.##

    override def toString: String = value.toString

  /** The operations on an edge of this graph. */
  trait EdgeOps:

    /** The value of this edge. */
    def value: X[V, E]

    /** The graph this edge belongs to. */
    def graph: GraphOps.this.type = GraphOps.this

    /** The label of this edge. */
    def label: E = value.label

    /** The vertices incident to this edge. */
    def vertices: Set[Vertex] = value.vertexValues.flatMap(vertex).toSet

    /** The number of copies of this edge in the graph. */
    def multiplicity: M = GraphOps.this.multiplicity(value)

    override def equals(that: Any): Boolean = that match
      case that: EdgeOps => (that.graph eq graph) && that.value == value
      case _             => false

    override def hashCode: Int = value.##

    override def toString: String = value.toString
