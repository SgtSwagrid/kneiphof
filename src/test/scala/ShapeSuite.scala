package com.alecdorrington.kneiphof

import munit.FunSuite

/** Tests for the built-in shapes, and for joins answering queries lazily. */
class ShapeSuite extends FunSuite, LazyAssertions:

  test("complete graphs and cliques join every two vertices"):
    val k5 = Graph.complete(5)
    assertEquals(k5.order, 5)
    assertEquals(k5.size, 10)
    assert(k5.hasEdge(4 ~ 0))
    assert(!k5.hasEdge(0 ~ 0))
    assertEquals(k5(2).degree, 4)
    assertEquals(Graph.clique(0, 1, 2, 3, 4), k5)
    assertEquals(
      Graph.clique("A", "B", "C"),
      Graph("A" ~ "B" ~ "C" ~ "A"),
    )
    assertEquals(Graph.clique(3), Graph.vertex(3))
    assertEquals(Graph.complete(1), Graph.vertex(0))

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
    intercept[IllegalArgumentException](Graph.cycle(0))

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
    intercept[IllegalArgumentException](Graph.wheel(0))

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
    val clique = Graph.complete(50_000)
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
    assertEquals(DirectedGraph.complete(3).size, 6)
    assert(DirectedGraph.complete(3).hasEdge(2 ~> 0))
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
    assertEquals(Multigraph.complete(4).size, 6)
    assertEquals(
      Multigraph.complete(4).multiplicity(0 ~ 1),
      Count(1),
    )

  test("shapes support every graph operation"):
    assertEquals(Graph.path(3) + (2 ~ 0), Graph.cycle(3))
    assertEquals(
      Graph.complete(4).removeVertex(3),
      Graph.complete(3),
    )
    assertEquals(
      Graph.cycle(5).mapVertices(_ % 2),
      Graph(0 ~ 1, 0 ~ 0),
    )

  test("joins answer queries without building their edges"):
    val cone = Graph.complete(50_000) :~ -1
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

  test(
    "complete multipartite graphs join every two vertices in different parts",
  ):
    val bipartite = Graph.complete(3, 4)
    assertEquals(bipartite.size, 12)
    assertEquals(
      bipartite,
      Graph.vertices(0, 1, 2) :~: Graph.vertices(3, 4, 5, 6),
    )
    assertEquals(
      Graph.complete(1, 1, 1, 1),
      Graph.complete(4),
    )
    assertEquals(
      Graph.complete(5, 0),
      Graph.vertices(0 until 5*),
    )
    assertEquals(Graph.complete(2, 0, 1), Graph(0 ~ 2, 1 ~ 2))
    assertEquals(Graph.complete(2, 2, 2)(0).degree, 4)
    assertLazy(Graph.complete(2, 3, 1), 0 ~ 1, 3 ~ 4, 0 ~ 5)
    assertEquals(
      DirectedGraph.complete(1, 1, 1),
      DirectedGraph.complete(3),
    )
    assertEquals(
      DirectedGraph.complete(2, 1),
      DirectedGraph(0 ~> 2 ~> 1, 2 ~> 0, 1 ~> 2),
    )
    assertLazyDirected(DirectedGraph.complete(2, 3), 0 ~> 1, 2 ~> 0)
    intercept[IllegalArgumentException](Graph.complete(2, -1))

  test("hypercubes join vertices whose bits differ in one place"):
    val cube = Graph.hypercube(3)
    assertEquals(cube.order, 8)
    assertEquals(cube.size, 12)
    assert(cube.hasEdge(2 ~ 6))
    assert(!cube.hasEdge(1 ~ 2))
    assertEquals(
      Graph.hypercube(2),
      Graph.cycle(0, 1, 3, 2),
    )
    assertEquals(Graph.hypercube(0), Graph.vertex(0))
    assertLazy(cube, 0 ~ 3, 0 ~ 7, 8)
    assertEquals(
      DirectedGraph.hypercube(2),
      DirectedGraph(0 ~> 1 ~> 3, 0 ~> 2 ~> 3),
    )
    assertLazyDirected(DirectedGraph.hypercube(3), 4 ~> 0, 0 ~> 4)
    intercept[IllegalArgumentException](Graph.hypercube(31))

  test("circulants jump round a cycle"):
    assertEquals(Graph.circulant(5, 1), Graph.cycle(5))
    assertEquals(
      Graph.circulant(5, 1, 2),
      Graph.complete(5),
    )
    assertEquals(
      Graph.circulant(5, 4, -1, 6),
      Graph.cycle(5),
    )
    assertEquals(Graph.circulant(2, 1), Graph(0 ~ 1))
    val ladder = Graph.circulant(6, 1, 3)
    assertEquals(ladder.size, 9)
    assertEquals(ladder(4).degree, 3)
    assertLazy(ladder, 0 ~ 2, 0 ~ 3, 1 ~ 4)
    assertEquals(
      DirectedGraph.circulant(4, 1),
      DirectedGraph.cycle(4),
    )
    assertEquals(DirectedGraph.circulant(4, 1, 3).size, 8)
    assertLazyDirected(
      DirectedGraph.circulant(6, 2, 3),
      0 ~> 2,
      2 ~> 0,
      3 ~> 0,
    )
    intercept[IllegalArgumentException](Graph.circulant(4, 4))

  test("Petersen graphs have an outer cycle, spokes and inner jumps"):
    val petersen = Graph.petersen
    assertEquals(petersen.order, 10)
    assertEquals(petersen.size, 15)
    assert((0 until 10).forall(petersen(_).degree == 3))
    assert(petersen.hasEdge(0 ~ 5))
    assert(petersen.hasEdge(5 ~ 7))
    assert(!petersen.hasEdge(5 ~ 6))
    assertEquals(petersen, Graph.petersen(5, 2))
    assertEquals(Graph.petersen(4, 1).size, 12)
    assertLazy(Graph.petersen(7, 3), 7 ~ 8, 7 ~ 10, 0 ~ 7)
    assertLazyDirected(
      DirectedGraph.petersen(5, 2),
      1 ~> 0,
      5 ~> 0,
      5 ~> 7,
    )
    intercept[IllegalArgumentException](Graph.petersen(6, 3))
    intercept[IllegalArgumentException](Graph.petersen(5, 0))

  test("grids, tori and prisms pair up the vertices of paths and cycles"):
    val grid = Graph.grid(2, 3)
    assertEquals(grid.order, 6)
    assertEquals(grid.size, 7)
    assert(grid.hasEdge((0, 1) ~ (1, 1)))
    assert(!grid.hasEdge((0, 0) ~ (1, 1)))
    assertEquals(grid((0, 1)).degree, 3)
    assertEquals(
      Graph.grid(1, 4),
      Graph.path((0, 0), (0, 1), (0, 2), (0, 3)),
    )
    assertLazy(grid, (0, 2) ~ (0, 0), (2, 0), (0, 0) ~ (0, 0))
    val torus = Graph.torus(3, 4)
    assertEquals(torus.size, 24)
    assert(torus.hasEdge((2, 3) ~ (0, 3)))
    assert(torus.hasEdge((2, 3) ~ (2, 0)))
    assertLazy(torus, (0, 0) ~ (1, 1), (0, 0) ~ (2, 0))
    val prism = Graph.prism(4)
    assertEquals(prism.size, 12)
    assert(prism.hasEdge((3, 1) ~ (0, 1)))
    assert(prism.hasEdge((2, 0) ~ (2, 1)))
    assertLazy(prism, (0, 0) ~ (1, 1), (0, 2))
    assertEquals(
      DirectedGraph.grid(2, 2),
      DirectedGraph(
        (0, 0) ~> (0, 1) ~> (1, 1),
        (0, 0) ~> (1, 0) ~> (1, 1),
      ),
    )
    assertLazyDirected(
      DirectedGraph.prism(3),
      (0, 1) ~> (0, 0),
      (2, 0) ~> (0, 0),
    )
    intercept[IllegalArgumentException](Graph.torus(0, 5))
    intercept[IllegalArgumentException](Graph.prism(0))

  test("cycles of 1 and 2 vertices are loops and double edges"):
    assertEquals(Graph.cycle(1), Graph(0 ~ 0))
    assertEquals(Graph.cycle(1).degree(0), 2)
    assertEquals(Multigraph.cycle(2), Multigraph(0 ~ 1 ~ 0))
    assertEquals(Multigraph.cycle(2).size, 2)
    assertEquals(
      Multigraph.cycle(2).multiplicity(1 ~ 0),
      Count(2),
    )
    assertEquals(Multigraph.cycle(2).degree(0), 2)
    assertEquals(
      DirectedGraph.cycle(2),
      DirectedGraph(0 ~> 1 ~> 0),
    )
    assertEquals(Graph.cycle(2), Graph.path(2))
    assertEquals(Graph.cycle(2).size, 1)
    assertEquals(Graph.cycle(2).degree(0), 1)
    assertEquals(Graph.wheel(1), Graph(0 ~ 1 ~ 1))
    assertEquals(Graph.wheel(2), Graph.complete(3))
    assertEquals(
      Multigraph.wheel(2),
      Multigraph(0 ~ 1, 0 ~ 2, 1 ~ 2 ~ 1),
    )
    val absent = Seq(0 ~ 1, 1 ~ 1, 2 ~ 2)
    Seq(
      Graph.cycle(1),
      Graph.cycle(2),
      Graph.wheel(1),
      Graph.wheel(2),
    ).foreach(assertLazy(_, absent*))
    Seq(
      Multigraph.cycle(2),
      Multigraph.wheel(2),
    ).foreach(assertLazy(_, absent*))
    assertLazyDirected(DirectedGraph.cycle(2), 0 ~> 0, 1 ~> 0)

  test("tori and prisms of small cycles have loops and double edges"):
    assertEquals(Graph.torus(1, 1), Graph((0, 0) ~ (0, 0)))
    assertEquals(Graph.torus(1, 1).degree((0, 0)), 2)
    assertEquals(Multigraph.torus(1, 1).size, 2)
    assertEquals(Multigraph.torus(1, 1).degree((0, 0)), 4)
    assertEquals(
      DirectedMultigraph.torus(1, 1).multiplicity((0, 0) ~> (0, 0)),
      Count(2),
    )
    assertEquals(
      Graph.torus(1, 3),
      Graph(
        (0, 0) ~ (0, 1) ~ (0, 2) ~ (0, 0),
        (0, 0) ~ (0, 0),
        (0, 1) ~ (0, 1),
        (0, 2) ~ (0, 2),
      ),
    )
    assertEquals(
      Graph.torus(2, 3),
      Graph.prism(3).mapVertices(_.swap),
    )
    assertEquals(Graph.torus(2, 3).size, 9)
    assertEquals(Graph.torus(2, 3).degree((1, 2)), 3)
    assertEquals(Multigraph.torus(2, 3).size, 12)
    assertEquals(
      Multigraph.torus(2, 3).multiplicity((1, 2) ~ (0, 2)),
      Count(2),
    )
    assertEquals(
      Graph.prism(2),
      Graph.cycle((0, 0), (1, 0), (1, 1), (0, 1)),
    )
    assertEquals(
      Graph.prism(1),
      Graph((0, 0) ~ (0, 0), (0, 1) ~ (0, 1), (0, 0) ~ (0, 1)),
    )
    val absent = Seq((0, 0) ~ (0, 0), (0, 0) ~ (1, 0), (2, 0))
    Seq(
      Graph.torus(1, 1),
      Graph.torus(1, 2),
      Graph.torus(2, 3),
      Graph.prism(1),
      Graph.prism(2),
    ).foreach(assertLazy(_, absent*))
    Seq(
      Multigraph.torus(1, 1),
      Multigraph.torus(1, 2),
      Multigraph.torus(2, 3),
      Multigraph.prism(2),
    ).foreach(assertLazy(_, absent*))
    val arrows = Seq((0, 0) ~> (0, 0), (1, 0) ~> (0, 0))
    Seq(
      DirectedGraph.torus(1, 1),
      DirectedGraph.torus(2, 2),
    ).foreach(assertLazyDirected(_, arrows*))
    Seq(
      DirectedMultigraph.torus(1, 1),
      DirectedMultigraph.prism(1),
    ).foreach(assertLazyDirected(_, arrows*))

  test("transitive tournaments point from each vertex to every later one"):
    val tournament = DirectedGraph.transitiveTournament(4)
    assertEquals(tournament.size, 6)
    assert(tournament.hasEdge(0 ~> 3))
    assert(!tournament.hasEdge(3 ~> 0))
    assertEquals(
      tournament.underlying,
      Graph.complete(4),
    )
    assertEquals(
      DirectedGraph.transitiveTournament("a", "b", "c"),
      DirectedGraph("a" ~> "b" ~> "c", "a" ~> "c"),
    )
    assertLazyDirected(tournament, 1 ~> 0, 2 ~> 3)

  test("large new shapes answer queries without building their edges"):
    val cube = Graph.hypercube(20)
    assertEquals(cube.size, 10_485_760)
    assertEquals(cube.degree(12_345), 20)
    assert(cube.hasEdge(0 ~ (1 << 19)))
    val bipartite = Graph.complete(30_000, 30_000)
    assertEquals(bipartite.size, 900_000_000)
    assertEquals(bipartite.degree(0), 30_000)
    assert(bipartite.hasEdge(29_999 ~ 30_000))
    assert(!bipartite.hasEdge(0 ~ 1))
    assertEquals(
      Graph.circulant(1_000_000, 1, 2, 500_000).size,
      2_500_000,
    )
    assertEquals(
      Graph.petersen(1_000_000, 3).degree(1_500_000),
      3,
    )
    val torus = Graph.torus(1_000, 1_000)
    assertEquals(torus.size, 2_000_000)
    assertEquals(torus.degree((999, 0)), 4)
    assert(torus.hasEdge((999, 0) ~ (0, 0)))

  test("shapes find vertices and edges by equality, like eager graphs"):
    val path = Graph.path(3)
    assert(path.hasVertex(1L))
    assertEquals(
      path.hasVertex(1L),
      Graph(0 ~ 1, 1 ~ 2).hasVertex(1L),
    )
    assert(path.hasEdge(1L ~ 2))
    assert(Graph.grid(2, 2).hasVertex((1L, 0)))
    assert(DirectedGraph.path(3).hasEdge(1L ~> 2))

  test("shapes reject negative counts"):
    intercept[IllegalArgumentException](Graph.star(-1))
    intercept[IllegalArgumentException](Graph.fan(-1))
