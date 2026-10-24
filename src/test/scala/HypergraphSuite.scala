package com.alecdorrington.kneiphof

/** Tests for undirected hypergraphs. */
class HypergraphSuite extends munit.FunSuite:

  val hypergraph: Hypergraph[Int, Unit] =
    Hypergraph(Hyperedge(1, 2, 3), Hyperedge(3, 4))

  test("hyperedges join any number of vertices"):
    assertEquals(hypergraph.order, 4)
    assertEquals(hypergraph.size, 2)
    assertEquals(hypergraph(3).degree, 2)
    assertEquals(
      hypergraph(1).neighbours.map(_.value),
      Set(2, 3),
    )

  test("repeated ends count towards degree"):
    assertEquals(
      Hypergraph(Hyperedge(1, 1, 2))(1).degree,
      2,
    )

  test("every graph is a hypergraph"):
    val graph: Hypergraph[Int, Unit] = Graph(1 ~ 2)
    assertEquals(graph, Hypergraph(Hyperedge(1, 2)))
    assertEquals((hypergraph | Graph(4 ~ 5)).size, 3)

  test("edges know their vertices"):
    val edge = hypergraph.edge(Hyperedge(3, 2, 1)).get
    assertEquals(edge.vertices.map(_.value), Set(1, 2, 3))
    assertEquals(hypergraph.edge(Hyperedge(1, 2)), None)
