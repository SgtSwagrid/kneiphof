package com.alecdorrington.kneiphof

import com.alecdorrington.kneiphof.GraphSuite.Landmass
import com.alecdorrington.kneiphof.GraphSuite.Landmass.*
import munit.FunSuite

/** Tests for undirected graphs whose edges form a set. */
class GraphSuite extends FunSuite:

  /** The seven bridges of Königsberg. */
  val bridges: Graph[Landmass, String] = Graph(
    Kneiphof ~ Altstadt % "Krämerbrücke",
    Kneiphof ~ Altstadt % "Schmiedebrücke",
    Kneiphof ~ Vorstadt % "Grüne Brücke",
    Kneiphof ~ Vorstadt % "Köttelbrücke",
    Kneiphof ~ Lomse    % "Honigbrücke",
    Altstadt ~ Lomse    % "Holzbrücke",
    Vorstadt ~ Lomse    % "Hohe Brücke",
  )

  test("parallel edges are distinct only if their labels are"):
    assertEquals(bridges.order, 4)
    assertEquals(bridges.size, 7)
    assertEquals(bridges(Kneiphof).degree, 5)
    assert(bridges.vertices.forall(_.degree % 2 == 1))
    assertEquals(Graph(1 ~ 2, 2 ~ 1).size, 1)
    assertEquals(bridges.mapEdges(_ => ()).size, 5)

  test("vertices know their neighbours"):
    assertEquals(
      bridges(Kneiphof).neighbours.map(_.value),
      Set(Altstadt, Vorstadt, Lomse),
    )
    assertEquals(
      bridges(Altstadt).edges.map(_.label).size,
      3,
    )

  test("vertices belong to their graph"):
    assertEquals(bridges(Kneiphof), bridges(Kneiphof))
    assertNotEquals[Any, Any](
      bridges(Kneiphof),
      bridges.addVertex(0)(Kneiphof),
    )
    assertEquals(bridges.vertex(0), None)

  test("loops count twice towards degree"):
    val graph = Graph(1 ~ 1, 1 ~ 2)
    assertEquals(graph(1).degree, 3)
    assertEquals(
      graph(1).neighbours.map(_.value),
      Set(1, 2),
    )

  test(
    "union, intersection and difference are set operations on edges, not vertices",
  ):
    val left  = Graph(1 ~ 2, 2 ~ 3)
    val right = Graph(2 ~ 3, 3 ~ 4)
    assertEquals(left | right, Graph(1 ~ 2, 2 ~ 3, 3 ~ 4))
    assertEquals(left + right, left | right)
    assertEquals(left & right, Graph(2 ~ 3))
    assertEquals(
      left - right,
      Graph.from(Seq(3), Seq(1 ~ 2)),
    )
    assertEquals(
      left - (1 ~ 2),
      Graph.from(Seq(1), Seq(2 ~ 3)),
    )
    val square = Graph.cycle(4)
    assertEquals(
      Graph.complete(4) - square,
      Graph(0 ~ 2, 1 ~ 3),
    )
    val pair = Graph.vertices(0, 1)
    assertEquals(square - pair, square)
    assertEquals(
      square.removeVertices(pair.vertexValues),
      Graph(2 ~ 3),
    )
    assertEquals(square & pair, pair)
    assertEquals(
      square.induced(pair.vertexValues),
      Graph(0 ~ 1),
    )

  test("operators are aliases of named methods"):
    val left   = Graph(1 ~ 2, 2 ~ 3)
    val right  = Graph(2 ~ 3, 3 ~ 4)
    val arrows = DirectedGraph(1 ~> 2)
    assertEquals(left + right, left.sum(right))
    assertEquals(left | right, left.union(right))
    assertEquals(left & right, left.intersection(right))
    assertEquals(left - right, left.difference(right))
    assertEquals(left ⊔ right, left.disjointUnion(right))
    assertEquals(
      left □ right,
      left.cartesianProduct(right),
    )
    assertEquals(left :~: right, left.join(right))
    assertEquals(
      (left :~: right) % "a",
      left.join(right).withLabel("a"),
    )
    assertEquals(left :~ 0, left.cone(0))
    assertEquals(0 ~: left, left.cone(0))
    assertEquals(arrows :~>: arrows, arrows.join(arrows))
    assertEquals(arrows :~> 0, arrows.coneTo(0))
    assertEquals(0 ~>: arrows, arrows.coneFrom(0))
    assertEquals(1 ~ 2, Edge(1, 2))
    assertEquals(1 ~> 2, DirectedEdge(1, 2))
    assertEquals(1 ~ 2 % "a", (1 ~ 2).withLabel("a"))
    assertEquals(1 ~ 2 ~ 3, (1 ~ 2).to(3))
    assertEquals(1 ~ 2 ~ 3 % "a", (1 ~ 2).to(3).withLabel("a"))
    assertEquals(1 ~> 2 ~> 3, (1 ~> 2).to(3))
    assertEquals(
      1 ~> 2 ~> 3 % "a",
      (1 ~> 2).to(3).withLabel("a"),
    )
    assertEquals(ValueSet(1) + 2, ValueSet(1).incl(2))
    assertEquals(ValueSet(1, 2) - 2, ValueSet(1, 2).excl(2))
    assertEquals(
      ValueSet(1) | ValueSet(2),
      ValueSet(1).union(ValueSet(2)),
    )

  test("handles can't become vertices, or be passed as vertex or edge values"):
    val graph  = Graph(1 ~ 2, 2 ~ 3)
    val vertex = graph(1)
    val edge   = graph.edge(1 ~ 2).get
    Seq(
      compileErrors("graph.hasVertex(vertex)"),
      compileErrors("graph.hasEdge(edge)"),
      compileErrors("graph.multiplicity(edge)"),
      compileErrors("graph.degree(vertex)"),
      compileErrors("graph.vertex(vertex)"),
      compileErrors("graph.edge(edge)"),
      compileErrors("graph(vertex)"),
      compileErrors("graph.incidentEdgeValues(vertex)"),
      compileErrors("graph.removeVertex(vertex)"),
      compileErrors("graph.removeVertices(vertex.neighbours)"),
      compileErrors("graph.induced(graph.vertices)"),
      compileErrors("graph.withMultiplicity(edge, false)"),
      compileErrors("graph.removeEdge(edge)"),
      compileErrors(
        "DirectedGraph(1 ~> 2).hasVertex(DirectedGraph(1 ~> 2)(1))",
      ),
      compileErrors("(1 ~ 2).occurrences(vertex)"),
      compileErrors("graph.vertexValues.contains(vertex)"),
      compileErrors("graph.vertexValues.get(vertex)"),
      compileErrors("graph.edgeValues.contains(edge)"),
      compileErrors("graph.edgeValues.get(edge)"),
      compileErrors("graph.edgeValues.getOrElse(edge, false)"),
      compileErrors("graph.edgeValues.getKey(edge)"),
      compileErrors("graph.vertexValues - vertex"),
      compileErrors("graph.vertexValues.excl(vertex)"),
      compileErrors("graph.edgeValues.without(edge)"),
      compileErrors("vertex ~ 2"),
      compileErrors("1 ~ vertex"),
      compileErrors("vertex ~> 2"),
      compileErrors("1 ~> vertex"),
      compileErrors("1 ~ 2 ~ vertex"),
      compileErrors("(1 ~ 2).to(vertex)"),
      compileErrors("1 ~> 2 ~> vertex"),
      compileErrors("graph.addVertex(vertex)"),
      compileErrors("graph.addVertices(graph.vertices)"),
      compileErrors("Graph.vertex(vertex)"),
      compileErrors("Graph.vertices(vertex)"),
      compileErrors("Graph.fromMultiplicities(Seq(vertex), Nil)"),
      compileErrors("Graph.path(vertex, vertex)"),
      compileErrors("DirectedGraph.transitiveTournament(vertex)"),
      compileErrors("Graph.from(Seq(vertex), Nil)"),
      compileErrors("Graph.clique(vertex)"),
      compileErrors("Graph.cycle(vertex)"),
      compileErrors("Graph.star(vertex)"),
      compileErrors("Graph.wheel(vertex)"),
      compileErrors("Graph.fan(vertex)"),
      compileErrors("Graph.tree(2, vertex)"),
      compileErrors("(1 ~ 2).substitute(_ => Seq(vertex))"),
      compileErrors("graph.mapVertices(graph(_))"),
      compileErrors("graph.map(graph(_), identity)"),
      compileErrors("(1 ~ 2).mapVertices(graph(_))"),
      compileErrors("graph.cone(vertex)"),
      compileErrors("graph :~ vertex"),
      compileErrors("vertex ~: graph"),
      compileErrors("DirectedGraph(1 ~> 2).coneTo(vertex)"),
      compileErrors("DirectedGraph(1 ~> 2) :~> vertex"),
      compileErrors("DirectedGraph(1 ~> 2).coneFrom(vertex)"),
      compileErrors("vertex ~>: DirectedGraph(1 ~> 2)"),
    ).foreach(errors =>
      assert(
        errors.contains("is a handle on a vertex or edge"),
        errors,
      ),
    )
    assertEquals(graph.addVertex(vertex.value), graph)
    assertEquals(vertex.value ~ 2, 1 ~ 2)
    assertEquals(
      graph.mapVertices(graph(_).value),
      graph,
    )
    assert(graph.hasVertex(vertex.value))
    assert(graph.hasEdge(edge.value))
    assert(ValueSet(vertex).contains(vertex))
    assertEquals(
      ValueSet(vertex) - vertex,
      ValueSet.empty,
    )
    assertEquals(
      ValueMap.empty.withEntry(edge, 1).get(edge),
      Some(1),
    )
    assertEquals(
      graph.induced(vertex.neighbours.map(_.value)),
      Graph.vertex(2),
    )
    assertEquals(
      graph.removeVertices(graph.vertices.map(_.value)),
      Graph.empty,
    )

  test(
    "graphs can be built from vertices alone, and empty calls make empty ones",
  ):
    assertEquals(Graph.vertices(1, 2, 3).order, 3)
    assertEquals(Graph.vertices(1, 2, 3).size, 0)
    assertEquals(
      DirectedGraph.vertices(1, 2),
      DirectedGraph.vertices(2, 1),
    )
    val graph = Graph(1 ~ 2)
    assertEquals(Graph.empty, Graph.vertices())
    assertEquals(Graph.path(), Graph.empty)
    assertEquals(Graph.clique(), Graph.empty)
    assertEquals(
      DirectedGraph.transitiveTournament(),
      DirectedGraph.empty,
    )
    assertEquals(
      Graph.fromMultiplicities(Nil, Nil),
      Graph.empty,
    )
    assertEquals(Graph.from(Nil, Nil), Graph.empty)
    assertEquals(graph.addVertices(Nil), graph)
    assertEquals(graph.induced(Nil), Graph.empty)
    assertEquals(graph.removeVertices(Set()), graph)

  test("graphs are only equal within their family"):
    assertNotEquals[Any, Any](
      Graph.vertices(1),
      DirectedGraph.vertices(1),
    )
    assertNotEquals[Any, Any](
      Graph.vertices(1),
      Multigraph.vertices(1),
    )
    assertNotEquals[Any, Any](Graph.empty, DirectedMultigraph.empty)
    assertNotEquals[Any, Any](Graph(1 ~ 2), Multigraph(1 ~ 2))
    assertEquals[Any, Any](
      Graph.vertices(1),
      Hypergraph.vertices(1),
    )
    assertEquals[Any, Any](Graph(1 ~ 2).toHypergraph, Graph(1 ~ 2))
    assertEquals[Any, Any](Hypergraph(Hyperedge(1, 2)), Graph(1 ~ 2))
    assertEquals[Any, Any](1 ~ 2, Multigraph(1 ~ 2))
    assertEquals[Any, Any](Graph.path(3), Graph(0 ~ 1 ~ 2))
    assertEquals(
      DirectedGraph.vertices(1).##,
      DirectedGraph.vertices(1).materialise.##,
    )

  test("graphs with different vertex types combine"):
    val graph = Graph(1 ~ 2) | Graph("a" ~ "b")
    assertEquals(graph.order, 4)
    val wider: Graph[Any, Any] = Graph(1 ~ 2)
    assert(wider.hasVertex(1))

  test("subgraphs can be induced by vertices or edges"):
    val graph = Graph.from(Seq(0), Seq(1 ~ 2, 2 ~ 3, 3 ~ 1))
    assertEquals(graph.induced(Seq(1, 2)), Graph(1 ~ 2))
    assertEquals(graph.filterVertices(_ > 1), Graph(2 ~ 3))
    assertEquals(graph.filterEdges(_ == (1 ~ 2)).order, 4)
    assertEquals(
      graph.edgeInduced(_ == (1 ~ 2)),
      Graph(1 ~ 2),
    )
    assertEquals(
      graph.removeVertex(1),
      Graph.from(Seq(0), Seq(2 ~ 3)),
    )

  test("single-vertex graphs are the unit of substitution"):
    val graph = Graph(1 ~ 2, 2 ~ 3) | Graph.vertex(4)
    assertEquals(
      graph.flatMapVertices(v => Graph.vertex(v)),
      graph,
    )
    val expand = (v: Int) => Graph(v ~ -v)
    assertEquals(
      Graph.vertex(5).flatMapVertices(expand),
      expand(5),
    )

  test("disjoint union keeps both sides apart"):
    val union = Graph(0 ~ 1).disjointUnion(Graph(0 ~ 1))
    assertEquals(union.order, 4)
    assertEquals(union.size, 2)
    assertEquals(union, Graph(0 ~ 1) ⊔ Graph(0 ~ 1))

  test("vertices can be renumbered"):
    assertEquals(
      Graph("b" ~ "c", "a" ~ "b").renumber,
      Graph(0 ~ 1, 1 ~ 2),
    )

  test("adding a hyperedge to a graph, even a widened one, gives a hypergraph"):
    val stillGraph: Graph[Int, Unit]      = Graph(1 ~ 2) + (2 ~ 3)
    val hypergraph: Hypergraph[Int, Unit] = Graph(1 ~ 2) + Hyperedge(1, 2, 3)
    assertEquals(stillGraph.size, 2)
    assertEquals(hypergraph.size, 2)
    assert(hypergraph.hasEdge(Hyperedge(3, 2, 1)))
    val widened: Hypergraph[Int, Unit] = Graph(1 ~ 2)
    val grown                          = widened + Hyperedge(1, 2, 3)
    assertEquals(
      grown,
      Hypergraph(Hyperedge(1, 2), Hyperedge(1, 2, 3)),
    )

  test("symmetric digraphs have an edge each way"):
    assertEquals(
      Graph(1 ~ 2, 3 ~ 3).symmetric,
      DirectedGraph(1 ~> 2, 2 ~> 1, 3 ~> 3),
    )

  test("graphs print their edges and isolated vertices"):
    assertEquals(
      Graph.from(Seq(3), Seq(1 ~ 2)).toString,
      "Graph(1 ~ 2, 3)",
    )

  test("joining adds an edge between every pair, and a vertex forms a cone"):
    val joined: Graph[Int, Unit] = Graph(1 ~ 2) :~: Graph.vertex(3)
    assertEquals(joined, Graph(1 ~ 2, 2 ~ 3, 3 ~ 1))
    assertEquals(
      joined,
      Graph(Graph(1 ~ 2) ~ Graph.vertex(3)).flatten,
    )
    val star: Graph[Int, Unit] = 0 ~: Graph.vertices(1, 2, 3)
    assertEquals(star, Graph(0 ~ 1, 0 ~ 2, 0 ~ 3))
    assertEquals[Any, Any](Graph.vertices(1, 2, 3) :~ 0, star)
    assertEquals((0 ~: Graph(1 ~ 2, 2 ~ 3, 3 ~ 1)).size, 6)
    assertEquals[Any, Any](
      0 ~: 1 ~: Graph.vertex(2),
      Graph(0 ~ 1, 1 ~ 2, 2 ~ 0),
    )

  test(
    "only the new edges of a join are relabelled, so equal joins can differ",
  ):
    val cone: Graph[Int, String] = (Graph(1 ~ 2 % "a") :~ 3) % "b"
    assertEquals(cone, Graph(1 ~ 2 % "a", 1 ~ 3 % "b", 2 ~ 3 % "b"))
    assertEquals[Any, Any](
      Graph(1 ~ 2 % "a").join(Graph.vertex(3)) % "b",
      cone,
    )
    val star: Graph[Int, String] = 0 ~: Graph.vertices(1, 2) % "x"
    assertEquals(star, Graph(0 ~ 1 % "x", 0 ~ 2 % "x"))
    val mixed: Graph[Int, Unit | String] =
      (Graph(1 ~ 2) :~: Graph.vertex(3)) % "x"
    assertEquals(
      mixed,
      Graph[Int, Unit | String](1 ~ 2, 1 ~ 3 % "x", 2 ~ 3 % "x"),
    )
    val first  = Graph(1 ~ 2) :~: Graph.vertex(3)
    val second = Graph.vertex(1) :~: Graph(2 ~ 3)
    assertEquals[Any, Any](first, second)
    assertEquals[Any, Any](first  % "a", Graph(1 ~ 2, 1 ~ 3 % "a", 2 ~ 3 % "a"))
    assertEquals[Any, Any](second % "a", Graph(1 ~ 2 % "a", 1 ~ 3 % "a", 2 ~ 3))

/** The companion object for [[GraphSuite]]. */
object GraphSuite:

  /** The land masses of Königsberg. */
  enum Landmass:
    case Kneiphof, Altstadt, Vorstadt, Lomse
