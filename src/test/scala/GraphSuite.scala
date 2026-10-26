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
    val triangle = Graph(Graph(1 ~ 2) ~ Graph.vertex(3)).flatten
    assertEquals(triangle, Graph(1 ~ 2, 2 ~ 3, 3 ~ 1))

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

  test("graphs can be built from vertices alone"):
    assertEquals(Graph.vertices(1, 2, 3).order, 3)
    assertEquals(Graph.vertices(1, 2, 3).size, 0)
    assertEquals(
      DirectedGraph.vertices(1, 2),
      DirectedGraph.vertices(2, 1),
    )

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

  test("joining adds an edge between every pair of vertices"):
    val joined: Graph[Int, Unit] = Graph(1 ~ 2) :~: Graph.vertex(3)
    assertEquals(joined, Graph(1 ~ 2, 2 ~ 3, 3 ~ 1))
    assertEquals(
      joined,
      Graph(Graph(1 ~ 2) ~ Graph.vertex(3)).flatten,
    )
    assertEquals(
      Graph(1 ~ 2 % "a").join(Graph.vertex(3), "b"),
      Graph(1 ~ 2 % "a", 1 ~ 3 % "b", 2 ~ 3 % "b"),
    )

  test("a vertex joined to a graph forms a cone"):
    val star: Graph[Int, Unit] = 0 ~: Graph.vertices(1, 2, 3)
    assertEquals(star, Graph(0 ~ 1, 0 ~ 2, 0 ~ 3))
    assertEquals[Any, Any](Graph.vertices(1, 2, 3) :~ 0, star)
    assertEquals((0 ~: Graph(1 ~ 2, 2 ~ 3, 3 ~ 1)).size, 6)
    assertEquals[Any, Any](
      0 ~: 1 ~: Graph.vertex(2),
      Graph(0 ~ 1, 1 ~ 2, 2 ~ 0),
    )

  test("only the new edges of a join are relabelled"):
    val cone: Graph[Int, String] = (Graph(1 ~ 2 % "a") :~ 3) % "b"
    assertEquals(cone, Graph(1 ~ 2 % "a", 1 ~ 3 % "b", 2 ~ 3 % "b"))
    val star: Graph[Int, String] = 0 ~: Graph.vertices(1, 2) % "x"
    assertEquals(star, Graph(0 ~ 1 % "x", 0 ~ 2 % "x"))
    val mixed: Graph[Int, Unit | String] =
      (Graph(1 ~ 2) :~: Graph.vertex(3)) % "x"
    assertEquals(
      mixed,
      Graph[Int, Unit | String](1 ~ 2, 1 ~ 3 % "x", 2 ~ 3 % "x"),
    )

/** The companion object for [[GraphSuite]]. */
object GraphSuite:

  /** The land masses of Königsberg. */
  enum Landmass:
    case Kneiphof, Altstadt, Vorstadt, Lomse
