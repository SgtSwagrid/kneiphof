package com.alecdorrington.kneiphof

import munit.FunSuite

/** Tests that counts of vertices and edges fail rather than overflow. */
class ArithmeticSuite extends FunSuite:

  /** A multigraph with as many copies of one edge as an `Int` holds. */
  private val heavy =
    Multigraph.fromMultiplicities(Nil, Seq((1 ~ 2) -> Count(Int.MaxValue)))

  test("joins and cones fail rather than overflow"):
    assertEquals(heavy.degree(1), Int.MaxValue)
    intercept[ArithmeticException](heavy.cone(3).degree(1))
    intercept[ArithmeticException](heavy.cone(3).size)

  test("substitutions fail rather than overflow"):
    val product = heavy.lexicographicProduct(Multigraph.vertices(0, 1))
    intercept[ArithmeticException](product.degree((1, 0)))
    intercept[ArithmeticException](product.size)

  test("degrees, in-degrees and out-degrees fail rather than overflow"):
    val graph = DirectedMultigraph.fromMultiplicities(
      Nil,
      Seq(
        (1 ~> 2) -> Count(Int.MaxValue),
        (1 ~> 3) -> Count(1),
      ),
    )
    intercept[ArithmeticException](graph(1).outDegree)
    intercept[ArithmeticException](graph.transpose(1).inDegree)
    intercept[ArithmeticException](graph.degree(1))
    val loop =
      Multigraph.fromMultiplicities(Nil, Seq((1 ~ 1) -> Count(1 << 30)))
    intercept[ArithmeticException](loop.degree(1))

  test("large shapes fail rather than overflow"):
    intercept[ArithmeticException](Graph.petersen(800_000_000, 2).size)
    intercept[ArithmeticException]:
      DirectedGraph.complete(1_100_000_000, 1).degree(1_100_000_000)
    intercept[IllegalArgumentException](Graph.petersen(4, 1_073_741_826))
    intercept[IllegalArgumentException](Graph.petersen(5, 1_073_741_825))
    intercept[ArithmeticException](Graph.star(Int.MaxValue))
    intercept[ArithmeticException](Graph.petersen(1_100_000_000, 2))

  test("equality never counts edges, so it doesn't fail on huge sizes"):
    val graph = Multigraph.fromMultiplicities(
      Nil,
      Seq(
        (1 ~ 2) -> Count(Int.MaxValue),
        (2 ~ 3) -> Count(1),
      ),
    )
    val copy = Multigraph.fromMultiplicities(
      Nil,
      Seq(
        (2 ~ 3) -> Count(1),
        (1 ~ 2) -> Count(Int.MaxValue),
      ),
    )
    intercept[ArithmeticException](graph.size)
    assertEquals(graph, graph)
    assertEquals(graph, copy)
    assert(Set(graph).contains(copy))

  test("choosing how to count never fails"):
    assertEquals(
      (Multigraph(0 ~ 1) - Multigraph.path(3).mapVertices(identity) -
        heavy.mapVertices(identity)).size,
      0,
    )
    assertEquals(
      (Graph(0 ~ 1) - Graph.complete(70_000)).size,
      0,
    )
    val grid = Graph.grid(100_000, 100_000)
    intercept[ArithmeticException](grid.order)
    assertEquals((Graph((0, 0) ~ (0, 1)) & grid).order, 2)
    assert((grid | Graph.path(3).mapVertices(identity)).hasVertex(0))
    assert((grid | Graph.path(3)).hasVertex(0))

  test("cycles of more than half an Int of vertices don't wrap"):
    val cycle = Graph.cycle(2_000_000_000)
    assertEquals(
      cycle.incidentEdgeValues(1_500_000_000),
      ValueSet(
        1_499_999_999 ~ 1_500_000_000,
        1_500_000_000 ~ 1_500_000_001,
      ),
    )
    assertEquals(
      cycle.incidentEdgeValues(0),
      ValueSet(1_999_999_999 ~ 0, 0 ~ 1),
    )

  test("huge shapes built the same way are equal without listing them"):
    val grid = Graph.grid(100_000, 100_000)
    assertEquals(grid, Graph.grid(100_000, 100_000))
    assertNotEquals(grid, Graph.grid(100_000, 100_001))
    intercept[UnsupportedOperationException](
      grid == Graph.torus(100_000, 100_000),
    )
    intercept[UnsupportedOperationException](grid.mapVertices(identity) == grid)
    intercept[UnsupportedOperationException](grid == grid.mapVertices(identity))
    assert(Graph.grid(100, 100) != Graph.torus(100, 100))

  test("huge graphs differ from graphs known or found to be smaller"):
    val grid  = Graph.grid(100_000, 100_000)
    val small = Graph((0, 0) ~ (0, 1))
    assert(grid != small.filterVertices(_ => true))
    assert(small.mapVertices(identity) != grid)
    assert(grid != (small.mapVertices(identity) | Graph.path((0, 2), (0, 3))))
    val unbounded = Graph.path(2_000).flatMapVertices(v => Graph.vertex((v, v)))
    assert(unbounded.structure.maxOrder.isEmpty)
    assert(grid != unbounded)
    assert(Set(grid).contains(Graph.grid(100_000, 100_000)))
    assertEquals(grid.##, Graph.grid(100_000, 100_000).##)
