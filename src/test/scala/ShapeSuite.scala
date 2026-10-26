package com.alecdorrington.kneiphof

/** Tests for the built-in shapes, and for joins answering queries lazily. */
class ShapeSuite extends munit.FunSuite:

  test("cliques join every two vertices"):
    val k5 = Graph.clique(5)
    assertEquals(k5.order, 5)
    assertEquals(k5.size, 10)
    assert(k5.hasEdge(4 ~ 0))
    assert(!k5.hasEdge(0 ~ 0))
    assertEquals(k5(2).degree, 4)
    assertEquals(
      Graph.clique("A", "B", "C"),
      Graph("A" ~ "B" ~ "C" ~ "A"),
    )

  test("paths and cycles follow their vertices in order"):
    assertEquals(Graph.path(4), Graph(0 ~ 1 ~ 2 ~ 3))
    assertEquals(
      Graph.path("a", "b", "c"),
      Graph("a" ~ "b" ~ "c"),
    )
    assertEquals(Graph.cycle(4), Graph(0 ~ 1 ~ 2 ~ 3 ~ 0))
    assertEquals(
      Graph.cycle("a", "b", "c"),
      Graph("a" ~ "b" ~ "c" ~ "a"),
    )
    intercept[IllegalArgumentException](Graph.cycle(2))

  test("stars, wheels and fans have a centre"):
    assertEquals(Graph.star(3), Graph(0 ~ 1, 0 ~ 2, 0 ~ 3))
    assertEquals(
      Graph.star("c", "x", "y"),
      Graph("c" ~ "x", "c" ~ "y"),
    )
    assertEquals(
      Graph.wheel(4),
      0 ~: Graph.cycle(1, 2, 3, 4),
    )
    assertEquals(
      Graph.wheel("h", "a", "b", "c")("h").degree,
      3,
    )
    assertEquals(Graph.fan(3), Graph(0 ~ 1, 0 ~ 2, 0 ~ 3, 1 ~ 2 ~ 3))
    intercept[IllegalArgumentException](Graph.wheel(2))

  test("trees branch in breadth-first order"):
    assertEquals(
      Graph.tree(2, 7),
      Graph(0 ~ 1, 0 ~ 2, 1 ~ 3, 1 ~ 4, 2 ~ 5, 2 ~ 6),
    )
    assertEquals(
      Graph.tree(3, "r", "a", "b", "c", "d"),
      Graph("r" ~ "a", "r" ~ "b", "r" ~ "c", "a" ~ "d"),
    )
    assertEquals(Graph.tree(1, 4), Graph.path(4))

  test("shapes need distinct vertices"):
    intercept[IllegalArgumentException](Graph.clique("A", "A"))

  test("large shapes answer queries without building their edges"):
    val clique = Graph.clique(50_000)
    assertEquals(clique.size, 1_249_975_000)
    assert(clique.hasEdge(49_999 ~ 7))
    assertEquals(clique(7).degree, 49_999)
    assertEquals(
      Graph.tree(2, 1_000_000)(499_999).degree,
      2,
    )

  test("directed shapes follow their arrows"):
    assertEquals(
      DirectedGraph.cycle(3),
      DirectedGraph(0 ~> 1 ~> 2 ~> 0),
    )
    assertEquals(DirectedGraph.clique(3).size, 6)
    assert(DirectedGraph.clique(3).hasEdge(2 ~> 0))
    assertEquals(
      DirectedGraph.star(2),
      DirectedGraph(0 ~> 1, 0 ~> 2),
    )
    assertEquals(
      DirectedGraph.tree(2, 7)(0).outDegree,
      2,
    )
    assertEquals(DirectedGraph.tree(2, 7)(6).inDegree, 1)

  test("multigraph shapes have one copy of each edge"):
    assertEquals(Multigraph.clique(4).size, 6)
    assertEquals(
      Multigraph.clique(4).multiplicity(0 ~ 1),
      Count(1),
    )

  test("shapes support every graph operation"):
    assertEquals(Graph.path(3) + (2 ~ 0), Graph.cycle(3))
    assertEquals(
      Graph.clique(4).removeVertex(3),
      Graph.clique(3),
    )
    assertEquals(
      Graph.cycle(5).mapVertices(_ % 2),
      Graph(0 ~ 1, 0 ~ 0),
    )

  test("joins answer queries without building their edges"):
    val cone = Graph.clique(50_000) :~ -1
    assertEquals(cone.size, 1_250_025_000)
    assert(cone.hasEdge(-1 ~ 123))
    assertEquals(cone(-1).degree, 50_000)
    assertEquals(cone(0).degree, 50_000)
    val labelled = (Graph.path(3) :~ "hub") % "spoke"
    assert(labelled.hasEdge(1 ~ "hub" % "spoke"))
    assert(!labelled.hasEdge(1 ~ "hub"))
    assertEquals(labelled.multiplicity(0 ~ 1), true)

  test("joins of overlapping graphs count their new edges"):
    val joined = Multigraph(1 ~ 2) :~: Multigraph(2 ~ 3)
    assertEquals(joined.multiplicity(2 ~ 2), Count(1))
    assertEquals(joined.multiplicity(1 ~ 2), Count(2))
    assertEquals[Any, Any](
      joined,
      Multigraph(1 ~ 2, 2 ~ 3, 1 ~ 2, 1 ~ 3, 2 ~ 2, 2 ~ 3),
    )
    assertEquals(joined.size, 6)
