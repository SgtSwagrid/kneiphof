package com.alecdorrington.kneiphof

import munit.FunSuite

/**
  * Tests that lazily built graphs answer queries as the graphs they build
  * would, and without building them.
  */
class LazySuite extends FunSuite, LazyAssertions:

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
    val product = Graph.complete(200).lexicographicProduct(Graph.complete(200))
    assertEquals(product.order, 40_000)
    assertEquals(product.size, 200 * 19_900 + 19_900 * 40_000)
    assert(product.hasEdge((0, 5) ~ (1, 7)))
    assert(product.hasEdge((3, 1) ~ (3, 2)))
    assertEquals(product((0, 0)).degree, 199 + 199 * 200)

  test("large disjoint unions answer queries without building their edges"):
    val union = Graph.complete(50_000) ⊔ Graph.complete(10)
    assertEquals(union.size, 1_249_975_000 + 45)
    assert(union.hasEdge(Left(1) ~ Left(2)))
    assert(!union.hasEdge(Left(1) ~ Right(2)))
    assertEquals(union(Right(3)).degree, 9)

  test("operations on eager graphs are eager"):
    val graph                           = Graph(1 ~ 2 ~ 3)
    val other                           = Graph(2 ~ 3 ~ 4)
    val results: Seq[AnyGraph[?, ?, ?]] = Seq(
      graph + other,
      graph | other,
      graph & other,
      graph - other,
      graph + (5 ~ 6),
      graph ⊔ other,
      graph.filterVertices(_ != 1),
      graph.filterEdges(_.isLoop),
      graph.edgeInduced(_.vertexValues.contains(1)),
      graph.mapVertices(_ * 2),
      graph.toHypergraph,
      graph.symmetric,
      graph.symmetric.transpose,
      graph.symmetric.underlying,
      graph.addVertex(7).removeVertex(1).removeEdge(2 ~ 3),
    )
    results.foreach(result => assert(result.eager, result))

  test("joins and substitutions are lazy even for eager graphs"):
    val graph                           = Graph(1 ~ 2)
    val results: Seq[AnyGraph[?, ?, ?]] = Seq(
      graph :~: Graph(3 ~ 4),
      graph :~ 3,
      graph.join(Graph.vertex(3)),
      graph.lexicographicProduct(graph),
      graph.flatMapVertices(v => Graph.vertex(v)),
      graph.view,
      Graph.complete(3),
    )
    results.foreach(result => assert(!result.eager, result))

  test("operations on lazy graphs are lazy, and agree with their built graphs"):
    val cycle = Graph.cycle(5)
    val graph = Graph(0 ~ 1, 1 ~ 3)
    assertEquals(cycle - (0 ~ 1), Graph(1 ~ 2 ~ 3 ~ 4 ~ 0))
    assertEquals(cycle & graph, Graph(0 ~ 1).addVertex(3))
    assertEquals(
      cycle.filterVertices(_ != 0),
      Graph.path(1, 2, 3, 4),
    )
    assertEquals(
      cycle.edgeInduced(_.vertexValues.contains(0)),
      Graph(4 ~ 0 ~ 1),
    )
    assertLazy(cycle + graph, 1 ~ 3, 0 ~ 2)
    assertLazy(cycle | (0 ~ 2), 0 ~ 2, 1 ~ 3)
    assertLazy(cycle & graph, 0 ~ 1, 1 ~ 2, 1 ~ 3, 2)
    assertLazy(cycle - (0 ~ 1), 0 ~ 1)
    assertLazy(cycle - graph, 0 ~ 1, 1 ~ 3)
    assertLazy(cycle.filterVertices(_ != 0), 0 ~ 1, 0)
    assertLazy(cycle.induced(Seq(1, 2, 3)), 3 ~ 4, 4)
    assertLazy(
      cycle.filterEdges(_.vertexValues.contains(0)),
      1 ~ 2,
      0 ~ 1,
    )
    assertLazy(
      cycle.edgeInduced(_.vertexValues.contains(0)),
      1 ~ 2,
      2,
    )
    assertLazy(cycle.mapVertices(_ % 3), 0 ~ 0, 1 ~ 1, 2 ~ 0, 3)
    assertLazy(cycle.mapEdges(_ => "x"), 0 ~ 1, 0 ~ 1 % "x")

  test("sums of lazy multigraphs add the counts of shared edges"):
    val sum = Multigraph.cycle(4) + Multigraph(0 ~ 1, 0 ~ 1, 0 ~ 2)
    assertEquals(sum.size, 7)
    assertEquals(sum.multiplicity(0 ~ 1), Count(3))
    assertLazy(sum, 1 ~ 3, 0 ~ 3)
    assertLazy(sum - (0 ~ 1), 0 ~ 1)
    assertLazy(
      sum & Multigraph(0 ~ 1, 0 ~ 1, 2 ~ 3),
      0 ~ 1,
      2 ~ 3,
      1 ~ 2,
    )

  test("transposes and conversions of lazy graphs are lazy"):
    val cycle = DirectedGraph.cycle(4)
    assertEquals(
      cycle.transpose,
      DirectedGraph(0 ~> 3 ~> 2 ~> 1 ~> 0),
    )
    assertEquals(cycle.underlying, Graph.cycle(4))
    assertEquals(
      Graph.path(3).symmetric,
      DirectedGraph(0 ~> 1 ~> 2 ~> 1 ~> 0),
    )
    assertLazyDirected(cycle.transpose, 1 ~> 0, 0 ~> 1)
    assertLazyDirected(Graph.path(3).symmetric, 0 ~> 2, 1 ~> 0)
    assertLazy(cycle.underlying, 0 ~ 2, 1 ~ 0)
    val hypergraph = Graph.cycle(3).toHypergraph
    assert(!hypergraph.eager)
    assert(hypergraph.hasEdge(0 ~ 1))
    val grown = hypergraph.updated(Hyperedge(0, 1, 2), true)
    assert(!grown.eager)
    assert(grown.hasEdge(Hyperedge(0, 1, 2)))
    assertEquals(grown.size, 4)

  test("single updates to lazy graphs patch them"):
    val cycle   = Graph.cycle(5)
    val patched = cycle
      .removeVertex(0)
      .addVertex(9)
      .updated(9 ~ 1, true)
      .removeEdge(2 ~ 3)
    assertEquals(patched, Graph(1 ~ 2, 3 ~ 4, 9 ~ 1))
    assertLazy(patched, 0 ~ 1, 2 ~ 3, 9 ~ 2, 0)
    val restored = Graph.cycle(4).removeVertex(0).addVertex(0)
    assertEquals(
      restored,
      Graph.path(1, 2, 3).addVertex(0),
    )
    assertLazy(restored, 0 ~ 1, 3 ~ 0)
    assertLazy(
      cycle.withMultiplicity(0 ~ 1, false).updated(0 ~ 1, true),
      0 ~ 1,
    )

  test("large lazy graphs are patched without building their edges"):
    val patched = Graph
      .complete(50_000)
      .removeVertex(0)
      .addVertex(-1)
      .updated(-1 ~ 1, true)
      .removeEdge(1 ~ 2)
    assertEquals(patched.order, 50_000)
    assertEquals(patched.size, 1_249_975_000 - 49_999)
    assert(patched.hasEdge(-1 ~ 1))
    assert(!patched.hasEdge(0 ~ 1))
    assert(!patched.hasEdge(1 ~ 2))
    assert(patched.hasEdge(2 ~ 3))
    assertEquals(patched(1).degree, 49_998)

  test("large lazy graphs are combined and filtered without building them"):
    val clique = Graph.complete(50_000)
    val sum    = clique + (0 ~ 50_000)
    assertEquals(sum.size, 1_249_975_001)
    assertEquals(sum(50_000).degree, 1)
    val difference = clique - Graph(0 ~ 1, 2 ~ 3, 4 ~ 4)
    assertEquals(difference.size, 1_249_974_998)
    assert(!difference.hasEdge(2 ~ 3))
    val even = clique.filterVertices(_ % 2 == 0)
    assertEquals(even.order, 25_000)
    assert(even.hasEdge(0 ~ 2))
    assert(!even.hasEdge(0 ~ 1))
    assertEquals(even(0).degree, 24_999)
    val transposed = DirectedGraph.star(50_000).transpose
    assertEquals(transposed.size, 50_000)
    assert(transposed.hasEdge(7 ~> 0))
    assertEquals(transposed(0).degree, 50_000)

  test("zero updates add no vertices, lazily or eagerly"):
    val lazily  = Graph.cycle(3).updated(7 ~ 8, false)
    val eagerly = Graph(0 ~ 1 ~ 2 ~ 0).updated(7 ~ 8, false)
    assertEquals(lazily.order, 3)
    assertEquals(eagerly.order, 3)
    assertEquals(lazily, eagerly)
    assertLazy(lazily, 7, 8, 7 ~ 8)

  test("long chains of updates and sums stay shallow"):
    val start: GraphBase[Int, Unit, Boolean] = Graph.complete(3)
    val updated = (0 until 20_000).foldLeft(start): (graph, vertex) =>
      graph.updated(vertex ~ (vertex + 1), true)
    assertEquals(updated.size, 20_001)
    assertEquals(updated.edgeValues.size, 20_001)
    val summed = (0 until 20_000).foldLeft(start): (graph, vertex) =>
      graph + (vertex ~ (vertex + 1))
    assert(!summed.eager)
    assertEquals(summed.size, 20_001)
    assertEquals(summed.edgeValues.size, 20_001)
    val removed = (0 until 20_000).foldLeft(updated): (graph, vertex) =>
      graph.removeEdge(vertex ~ (vertex + 1))
    assertEquals(removed.degree(0), 1)
    val thinned = Graph.cycle(100_000).filterVertices(_ % 3 != 0)
    assertEquals(thinned.size, 33_333)
    assertEquals(thinned.edgeValues.size, 33_333)

  test("lazy filters, differences and maps agree with their built graphs"):
    assertLazy(
      Graph.complete(30).filterVertices(_ % 3 != 0),
      0 ~ 1,
      1 ~ 2,
    )
    assertLazy(Graph.complete(12).filterEdges(_.vertexValues.contains(0)))
    assertLazy(
      Graph.complete(12) - Graph(0 ~ 1, 2 ~ 3).view,
      0 ~ 1,
    )
    assertLazy(
      Graph.complete(12) & Graph(0 ~ 1, 1 ~ 20),
      0 ~ 1,
      1 ~ 20,
    )
    val folded = Multigraph.complete(4).mapVertices(_ % 2)
    assertEquals(folded.size, 6)
    assertEquals(folded.degree(0), 6)
    assertLazy(folded, 0 ~ 0, 0 ~ 1, 1 ~ 1)

  test("cheap queries on large lazy graphs don't build them"):
    val clique = Graph.complete(50_000)
    assertEquals((clique & (1 ~ 2)).size, 1)
    assertEquals(
      (Graph.path(3) + clique).size,
      1_249_975_000,
    )
    assertEquals(
      clique.edge(0 ~ 1).map(_.value),
      Some(0 ~ 1),
    )
    assertEquals(clique(0).edges.size, 49_999)
    assertEquals(
      clique.filterEdges(_ => true).degree(0),
      49_999,
    )
    assertEquals(
      Multigraph.complete(50_000).mapVertices(_ / 2).size,
      1_249_975_000,
    )

  test("substituting one-vertex hyperedges counts merged copies once"):
    val substituted =
      Hypergraph(Hyperedge(0)).flatMapVertices(_ => Hypergraph(Hyperedge("a")))
    assertEquals(
      substituted,
      Hypergraph(Hyperedge("a")),
    )
    assertEquals(substituted.size, 1)
    assertEquals(substituted.degree("a"), 1)

  test(
    "substitutions count sizes and degrees without building their outer graph",
  ):
    val product = Graph
      .complete(3_000)
      .lexicographicProduct(Graph.vertices(0, 1))
    assertEquals(product.size, 4_498_500 * 4)
    val looped = Multigraph(1 ~ 1, 1 ~ 2, 2 ~ 3)
      .view
      .lexicographicProduct(Multigraph.complete(2))
    assertLazy(
      looped,
      (1, 0) ~ (1, 1),
      (1, 0) ~ (3, 0),
      (1, 1) ~ (1, 1),
    )
