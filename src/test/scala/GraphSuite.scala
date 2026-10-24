package com.alecdorrington.kneiphof

/** Tests for undirected graphs whose edges form a set. */
class GraphSuite extends munit.FunSuite:

  enum Landmass:
    case Kneiphof, Altstadt, Vorstadt, Lomse

  import Landmass.*

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

  test("parallel edges with different labels are distinct"):
    assertEquals(bridges.order, 4)
    assertEquals(bridges.size, 7)
    assertEquals(bridges(Kneiphof).degree, 5)
    assert(bridges.vertices.forall(_.degree % 2 == 1))

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

  test("unlabelled graphs have no parallel edges"):
    assertEquals(Graph(1 ~ 2, 2 ~ 1).size, 1)
    assertEquals(bridges.mapEdges(_ => ()).size, 5)

  test("loops count twice towards degree"):
    val graph = Graph(1 ~ 1, 1 ~ 2)
    assertEquals(graph(1).degree, 3)
    assertEquals(
      graph(1).neighbours.map(_.value),
      Set(1, 2),
    )

  test("union, intersection and difference are set operations"):
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

  test("graphs with different vertex types combine"):
    val graph = Graph(1 ~ 2) | Graph("a" ~ "b")
    assertEquals(graph.order, 4)
    val wider: Graph[Any, Any] = Graph(1 ~ 2)
    assert(wider.hasVertex(1))

  test("mapping vertices merges vertices and edges"):
    assertEquals(
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 4).mapVertices(_ % 2),
      Graph(0 ~ 1),
    )

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

  test("substituting graphs for vertices"):
    val path = Graph(0 ~ 1)
    val k4   = path.lexicographicProduct(path)
    assertEquals(k4.order, 4)
    assertEquals(k4.size, 6)
    val triangle = Graph(Graph(1 ~ 2) ~ Graph.from(Seq(3), Seq())).flatten
    assertEquals(triangle, Graph(1 ~ 2, 2 ~ 3, 3 ~ 1))

  test("disjoint union keeps both sides apart"):
    val union = Graph(0 ~ 1).disjointUnion(Graph(0 ~ 1))
    assertEquals(union.order, 4)
    assertEquals(union.size, 2)
    assertEquals(union, Graph(0 ~ 1) ⊔ Graph(0 ~ 1))

  test("vertices can be renumbered"):
    assertEquals(
      Graph("b" ~ "c", "a" ~ "b").indexed,
      Graph(0 ~ 1, 1 ~ 2),
    )

  test("adding a hyperedge to a graph gives a hypergraph"):
    val stillGraph: Graph[Int, Unit]      = Graph(1 ~ 2) + (2 ~ 3)
    val hypergraph: Hypergraph[Int, Unit] = Graph(1 ~ 2) + Hyperedge(1, 2, 3)
    assertEquals(stillGraph.size, 2)
    assertEquals(hypergraph.size, 2)
    assert(hypergraph.hasEdge(Hyperedge(3, 2, 1)))

  test("a graph widened to a hypergraph accepts hyperedges"):
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
