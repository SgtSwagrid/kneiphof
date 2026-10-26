package com.alecdorrington.kneiphof

/**
  * Tests that lazily built graphs answer queries as the graphs they build
  * would, and without building them.
  */
class LazySuite extends munit.FunSuite:

  /**
    * Checks that the queries [[graph]] answers lazily agree with the graph it
    * builds, both for its own vertices and edges and for the [[absent]] ones.
    */
  def assertLazy[M]
    (
      graph: GraphBase[Any, Any, M],
      absent: Any*,
    )
    (using munit.Location)
    : Unit =
    val built = graph.mapVertices(identity)
    assertEquals(graph.order, built.order)
    assertEquals(graph.size, built.size)
    assertEquals(graph.vertexValues, built.vertexValues)
    (built.vertexValues ++ absent).foreach: vertex =>
      assertEquals(
        graph.hasVertex(vertex),
        built.hasVertex(vertex),
      )
      assertEquals(
        graph.incidentEdgeValues(vertex),
        built.incidentEdgeValues(vertex),
      )
    (built.edgeValues.keys ++ absent).foreach: edge =>
      assertEquals(
        graph.multiplicity(edge),
        built.multiplicity(edge),
      )

  /** Checks a directed graph in the same way as [[assertLazy]]. */
  def assertLazyDirected[M]
    (
      graph: DirectedGraphBase[Any, Any, M],
      absent: Any*,
    )
    (using munit.Location)
    : Unit =
    val built = graph.mapVertices(identity)
    assertEquals(graph.order, built.order)
    assertEquals(graph.size, built.size)
    (built.vertexValues ++ absent).foreach: vertex =>
      assertEquals(
        graph.hasVertex(vertex),
        built.hasVertex(vertex),
      )
      assertEquals(
        graph.incidentEdgeValues(vertex),
        built.incidentEdgeValues(vertex),
      )
    (built.edgeValues.keys ++ absent).foreach: edge =>
      assertEquals(
        graph.multiplicity(edge),
        built.multiplicity(edge),
      )

  test("disjoint unions keep their sides apart"):
    val union = Graph.cycle(4) ⊔ Graph.path("a", "b", "c")
    assertEquals(union.order, 7)
    assertEquals(union.size, 6)
    assertLazy(
      union,
      Left(0) ~ Right("a"),
      Left(0) ~ Left(2),
      Left(9),
      Right("z"),
      "a",
    )

  test("substituting disjoint graphs for vertices"):
    val graph = Graph(1 ~ 2 ~ 3).flatMapVertices(v => Graph.path(v, v * 10))
    assertEquals(graph.size, 3 + 2 * 4)
    assertLazy(graph, 1 ~ 3, 10 ~ 30, 99)

  test("substituting overlapping graphs merges their vertices"):
    val graph = Graph(1 ~ 2, 2 ~ 3).flatMapVertices(v => Graph.clique(0, v))
    assert(graph.hasEdge(0 ~ 0))
    assertLazy(graph, 1 ~ 3, 0 ~ 4)

  test("substitution counts the copies of loops and repeated edges"):
    val graph = Multigraph(1 ~ 1, 1 ~ 2, 1 ~ 2).flatMapVertices(v =>
      Multigraph.path(v, -v),
    )
    assertEquals(graph.multiplicity(1 ~ -1), Count(3))
    assertLazy(graph, 1 ~ 3, -1 ~ -2)

  test("lexicographic products tag each copy"):
    val product = Graph.cycle(4).lexicographicProduct(Graph.path(3))
    assertEquals(product.order, 12)
    assertEquals(product.size, 4 * 2 + 4 * 9)
    assertLazy(product, (0, 0) ~ (2, 0), (0, 0) ~ (0, 2), (5, 0))

  test("flattening substitutes the graphs held by vertices"):
    val graph = Graph(Graph(1 ~ 2) ~ Graph.vertex(3)).flatten
    assertEquals(graph, Graph(1 ~ 2 ~ 3 ~ 1))
    assertLazy(graph, 1 ~ 1, 4)

  test("directed substitutions follow their arrows"):
    val graph = DirectedGraph(1 ~> 2 ~> 3).flatMapVertices(v =>
      DirectedGraph.path(v, v * 10),
    )
    assert(graph.hasEdge(10 ~> 2))
    assert(!graph.hasEdge(2 ~> 10))
    assertLazyDirected(graph, 2 ~> 1, 30 ~> 20)
    assertLazyDirected(
      DirectedGraph.cycle(3) ⊔ DirectedGraph.star(2),
      Left(0) ~> Right(0),
    )

  test("large products answer queries without building their edges"):
    val product = Graph.clique(200).lexicographicProduct(Graph.clique(200))
    assertEquals(product.order, 40_000)
    assertEquals(product.size, 200 * 19_900 + 19_900 * 40_000)
    assert(product.hasEdge((0, 5) ~ (1, 7)))
    assert(product.hasEdge((3, 1) ~ (3, 2)))
    assertEquals(product((0, 0)).degree, 199 + 199 * 200)

  test("large disjoint unions answer queries without building their edges"):
    val union = Graph.clique(50_000) ⊔ Graph.clique(10)
    assertEquals(union.size, 1_249_975_000 + 45)
    assert(union.hasEdge(Left(1) ~ Left(2)))
    assert(!union.hasEdge(Left(1) ~ Right(2)))
    assertEquals(union(Right(3)).degree, 9)
