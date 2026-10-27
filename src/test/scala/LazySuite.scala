package com.alecdorrington.kneiphof

import com.alecdorrington.kneiphof.incidence.{
  Difference, EdgeInduced, Image, Intersection, Patch, Subgraph, Sum, Union,
}
import munit.{FunSuite, Location}

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

  test("every operation is lazy, even on stored graphs"):
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
      graph.symmetric,
      graph.symmetric.transpose,
      graph.symmetric.underlying,
      graph.addVertex(7).removeVertex(1).removeEdge(2 ~ 3),
    )
    results.foreach(result => assert(!result.structure.stored, result))

  test("joins, substitutions and shapes are lazy"):
    val graph                           = Graph(1 ~ 2)
    val results: Seq[AnyGraph[?, ?, ?]] = Seq(
      graph :~: Graph(3 ~ 4),
      graph :~ 3,
      graph.join(Graph.vertex(3)),
      graph.lexicographicProduct(graph),
      graph.flatMapVertices(v => Graph.vertex(v)),
      Graph.complete(3),
    )
    results.foreach(result => assert(!result.structure.stored, result))

  test("materialising a graph stores it without changing it"):
    val graph  = Graph.complete(4) - (0 ~ 1)
    val stored = graph.materialise
    assert(!graph.structure.stored)
    assert(stored.structure.stored)
    assertEquals(stored, graph)
    assert(stored.materialise eq stored)
    assert(Graph(1 ~ 2).toHypergraph.structure.stored)
    assert(!Graph.path(3).toHypergraph.structure.stored)

  test("long chains of the same operation merge into one structure"):
    val steps                   = 1 until 10_000
    val start: Graph[Int, Unit] = Graph.path(2)
    val unioned                 =
      steps.foldLeft(start)((graph, i) => graph | lazyPath(i, i + 1))
    assertEquals(unioned.size, 10_000)
    assertEquals(unioned.degree(5_000), 2)
    assertEquals(
      unioned.materialise,
      Graph.path(10_001),
    )
    unioned.structure match
      case union: Union[?, ?, ?] => assertEquals(union.operands.size, 10_000)
      case structure             => fail(s"$structure isn't one union.")
    val counts: Multigraph[Int, Unit] = Multigraph.path(2)
    val summed                        = steps.foldLeft(counts): (graph, _) =>
      graph + Multigraph.path(2).mapVertices(identity)
    assertEquals(
      summed.multiplicity(0 ~ 1),
      Count(10_000),
    )
    assertEquals(summed.degree(0), 10_000)
    summed.structure match
      case sum: Sum[?, ?, ?] => assertEquals(sum.operands.size, 10_000)
      case structure         => fail(s"$structure isn't one sum.")
    val clique: Graph[Int, Unit] = Graph.complete(5)
    val met = steps.foldLeft(clique)((graph, _) => graph & Graph.complete(5))
    assertEquals(met, Graph.complete(5).materialise)
    met.structure match
      case intersection: Intersection[?, ?, ?] =>
        assertEquals(intersection.others.size, 9_999)
      case structure => fail(s"$structure isn't one intersection.")
    val thinned =
      steps.foldLeft(clique)((graph, _) => graph - lazyPath(0, 1, 2))
    assertEquals(thinned.size, 8)
    assert(!thinned.hasEdge(0 ~ 1))
    thinned.structure match
      case difference: Difference[?, ?, ?] =>
        assertEquals(difference.others.all.size, 9_999)
      case structure => fail(s"$structure isn't one difference.")
    val mapped =
      steps.foldLeft(start)((graph, _) => graph.mapVertices(identity))
    assertEquals(mapped.materialise, Graph.path(2))
    mapped.structure match
      case _: Image[?, ?, ?, ?, ?] => ()
      case structure               => fail(s"$structure isn't one image.")
    val path: Graph[Int, Unit] = Graph.path(10_001)
    val filtered               =
      steps.foldLeft(path)((graph, i) => graph.filterVertices(_ != i))
    assertEquals(
      filtered.materialise,
      Graph.vertices(0, 10_000),
    )
    filtered.structure match
      case _: Subgraph[?, ?, ?] => ()
      case structure            => fail(s"$structure isn't one subgraph.")

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
    assert(!hypergraph.structure.stored)
    assert(hypergraph.hasEdge(0 ~ 1))
    val grown = hypergraph.withEdge(Hyperedge(0, 1, 2), true)
    assert(!grown.structure.stored)
    assert(grown.hasEdge(Hyperedge(0, 1, 2)))
    assertEquals(grown.size, 4)

  test("single updates to lazy graphs patch them"):
    val cycle   = Graph.cycle(5)
    val patched = cycle
      .removeVertex(0)
      .addVertex(9)
      .withEdge(9 ~ 1, true)
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
      cycle.withMultiplicity(0 ~ 1, false).withEdge(0 ~ 1, true),
      0 ~ 1,
    )

  test("large lazy graphs are patched without building their edges"):
    val patched = Graph
      .complete(50_000)
      .removeVertex(0)
      .addVertex(-1)
      .withEdge(-1 ~ 1, true)
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
    val lazily  = Graph.cycle(3).withEdge(7 ~ 8, false)
    val eagerly = Graph(0 ~ 1 ~ 2 ~ 0).withEdge(7 ~ 8, false)
    assertEquals(lazily.order, 3)
    assertEquals(eagerly.order, 3)
    assertEquals(lazily, eagerly)
    assertLazy(lazily, 7, 8, 7 ~ 8)

  test("long chains of updates and sums stay shallow"):
    val start: GraphBase[Int, Unit, Boolean] = Graph.complete(3)
    val updated = (0 until 20_000).foldLeft(start): (graph, vertex) =>
      graph.withEdge(vertex ~ (vertex + 1), true)
    assertEquals(updated.size, 20_001)
    assertEquals(updated.edgeValues.size, 20_001)
    val summed = (0 until 20_000).foldLeft(start): (graph, vertex) =>
      graph + (vertex ~ (vertex + 1))
    assert(!summed.structure.stored)
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
      Graph.complete(12) - Graph(0 ~ 1, 2 ~ 3).mapVertices(identity),
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
    val looped = Multigraph(1 ~ 1, 1 ~ 2, 2 ~ 3).lexicographicProduct(
      Multigraph.complete(2),
    )
    assertLazy(
      looped,
      (1, 0) ~ (1, 1),
      (1, 0) ~ (3, 0),
      (1, 1) ~ (1, 1),
    )

  test(
    "sums and unions of lazy operands are lazy, and agree with their built graphs",
  ):
    val cycle = Graph.cycle(5)
    val path  = lazyPath(2, 3, 4, 5)
    assertLazy(cycle + path, 0 ~ 5, 4 ~ 5, 5)
    assertLazy(cycle | path, 0 ~ 5, 1 ~ 3)
    (cycle | path).structure match
      case union: Union[?, ?, ?] => assertEquals(union.operands.size, 2)
      case structure             => fail(s"$structure isn't one union.")
    val counts = Multigraph.cycle(4)
    val more   = Multigraph.path(0, 1, 2).mapVertices(identity) +
      Multigraph.path(0, 1).mapVertices(identity)
    assertLazy(counts | more, 0 ~ 1, 0 ~ 2, 1 ~ 2)
    assertLazy(counts + more, 0 ~ 1, 0 ~ 2, 1 ~ 2)
    assertEquals(
      (counts | more).multiplicity(0 ~ 1),
      Count(2),
    )
    assertEquals(
      (counts + more).multiplicity(0 ~ 1),
      Count(3),
    )

  test("alternating few and lazy operands still merge into one structure"):
    val steps                   = 1 until 10_000
    val start: Graph[Int, Unit] = Graph.path(2)
    val unioned                 = steps.foldLeft(start): (graph, i) =>
      graph | (if i % 2 == 0 then Graph.path(i, i + 1) else lazyPath(i, i + 1))
    assertEquals(unioned.size, 10_000)
    assert(unioned.hasVertex(10_000))
    assertEquals(unioned.degree(5_000), 2)
    assertEquals(
      unioned.materialise,
      Graph.path(10_001),
    )
    unioned.structure match
      case union: Union[?, ?, ?] =>
        assertEquals(union.parts.all.size, 10_000)
        assertEquals(union.gathered.size, 0)
      case structure => fail(s"$structure isn't one union.")
    val clique: Graph[Int, Unit] = Graph.complete(5)
    val thinned                  = steps.foldLeft(clique): (graph, i) =>
      graph - (if i % 2 == 0 then Graph.path(3) else lazyPath(0, 1, 2))
    assertEquals(thinned.size, 8)
    assert(thinned.hasVertex(4))
    thinned.structure match
      case difference: Difference[?, ?, ?] =>
        assertEquals(difference.others.all.size, 5_000)
      case structure => fail(s"$structure isn't one difference.")
    val counts: Multigraph[Int, Unit] = Multigraph.path(2)
    val summed                        = steps.foldLeft(counts): (graph, i) =>
      val next = Multigraph.path(0, 1)
      graph + (if i % 2 == 0 then next else next.mapVertices(identity))
    assertEquals(
      summed.multiplicity(0 ~ 1),
      Count(10_000),
    )
    summed.structure match
      case sum: Sum[?, ?, ?] =>
        assertEquals(sum.parts.all.size, 10_000)
        assertEquals(sum.gathered.size, 0)
      case structure => fail(s"$structure isn't one sum.")

  test("few operands are copied, and others aren't"):
    val start: Graph[Int, Unit] = Graph.path(2)
    (start | Graph.path(5, 6)).structure match
      case _: Patch[?, ?, ?] => ()
      case structure         => fail(s"$structure didn't patch the few edges.")
    val stored = Graph.complete(100).materialise
    (Graph.vertex(0) + stored).structure match
      case _: Union[?, ?, ?] => ()
      case structure         => fail(s"$structure copied the stored operand.")
    val vertices = Graph.vertices((0 until 1_000_000)*)
    (start + vertices).structure match
      case _: Union[?, ?, ?] => ()
      case structure         => fail(s"$structure copied the vertices.")
    val subtrahend = Graph.complete(200).materialise
    val thinned    = Graph.complete(5) - subtrahend
    assertEquals(thinned.size, 0)
    thinned.structure match
      case _: Difference[?, ?, ?] => ()
      case structure              => fail(s"$structure copied the subtrahend.")

  test("adding or removing edges one at a time stays cheap"):
    val union: Graph[Int, Unit] = Graph.path(3) | Graph.complete(100)
    val grown = (0 until 20_000).foldLeft(union): (graph, i) =>
      val next = graph | ((i + 1_000) ~ (i + 1_001))
      assert(next.hasEdge((i + 1_000) ~ (i + 1_001)))
      next
    assertEquals(grown.degree(1_000), 1)
    grown.structure match
      case union: Union[?, ?, ?] => assertEquals(union.parts.all.size, 2)
      case structure             => fail(s"$structure isn't one union.")
    val counts: Multigraph[Int, Unit] = Multigraph.complete(100) +
      Multigraph.complete(100).mapVertices(identity)
    val summed = (0 until 20_000).foldLeft(counts): (graph, _) =>
      val next = graph + (0 ~ 1)
      assert(next.hasEdge(0 ~ 1))
      next
    assertEquals(
      summed.multiplicity(0 ~ 1),
      Count(20_002),
    )
    val difference: Graph[Int, Unit] = Graph.complete(200) - lazyPath(0, 1, 2)
    val thinned = (0 until 199).foldLeft(difference): (graph, i) =>
      val next = graph - (i ~ (i + 1))
      assert(!next.hasEdge(i ~ (i + 1)))
      next
    assertEquals(thinned.size, 199 * 100 - 199)
    thinned.structure match
      case difference: Difference[?, ?, ?] =>
        assertEquals(difference.others.all.size, 1)
      case structure => fail(s"$structure isn't one difference.")

  test("alternating different operations on few operands stays shallow"):
    val start: Graph[Int, Unit] = Graph.cycle(3)
    val mixed = (0 until 10_000).foldLeft(start): (graph, i) =>
      graph - (0 ~ 1) + Graph.path(i, i + 1)
    assert(mixed.hasVertex(10_000))
    assertEquals(mixed.size, 10_000)
    val grown = (0 until 10_000).foldLeft(start): (graph, i) =>
      graph.addVertex(-i - 1) | Graph.path(i, i + 1)
    assert(grown.hasVertex(-10_000))
    assertEquals(grown.order, 20_001)

  test("intersections and differences walk their smaller side"):
    val clique = Graph.complete(50_000)
    val small  = Graph(0 ~ 1)
    assertEquals((small & clique).size, 1)
    assertEquals((clique & small).size, 1)
    assertEquals(
      (small - Graph.complete(20_000).edgeInduced(_ => true)).size,
      0,
    )
    assertEquals((small & clique).order, 2)
    assertEquals((clique & small).order, 2)
    assertEquals((small - clique).size, 0)
    assertEquals((clique - small).size, 1_249_974_999)

  test(
    "chained edge-induced subgraphs merge, and only look at their own edges",
  ):
    assertEquals(
      Graph.complete(1_000).edgeInduced(_ => true).size,
      499_500,
    )
    val clique: Graph[Int, Unit] = Graph.complete(200)
    val chained                  = (0 until 20).foldLeft(clique): (graph, i) =>
      graph.edgeInduced(_.occurrences(i) == 0)
    assertEquals(chained.order, 180)
    assertEquals(chained.size, 180 * 179 / 2)
    chained.structure match
      case _: EdgeInduced[?, ?, ?] => ()
      case structure               => fail(s"$structure isn't one subgraph.")
    assertLazy(
      Graph
        .cycle(6)
        .edgeInduced(_.occurrences(0) == 0)
        .edgeInduced(_.occurrences(1) == 0),
      0 ~ 5,
      1 ~ 2,
      0,
      1,
    )

  test(
    "handles on lazy images, joins and products agree with their built graphs",
  ):
    assertHandles(Graph.cycle(4).mapVertices(_ % 2))
    assertHandles(Multigraph.cycle(4).mapVertices(_ % 2))
    assertHandles(Graph.path(3) :~: Graph.vertices(1, 5))
    assertHandles(Graph.cycle(3) □ Graph.path(3))
    assertDirectedHandles(DirectedGraph.cycle(4).mapVertices(_ % 2))
    assertDirectedHandles(DirectedMultigraph.cycle(4).mapVertices(_ % 2))
    assertDirectedHandles(
      DirectedGraph.path(3) :~>: DirectedGraph.vertices(1, 5),
    )
    assertDirectedHandles(DirectedGraph.cycle(3).coneTo(9))
    assertDirectedHandles(DirectedGraph.cycle(3) □ DirectedGraph.path(2))
    val transposed = DirectedGraph.star(50_000).transpose
    assertEquals(transposed(0).inDegree, 50_000)
    assertEquals(transposed(0).inEdges.size, 50_000)
    assertEquals(transposed(0).outDegree, 0)
    assertEquals(transposed(0).edges.size, 50_000)

  test("large shapes count and find their vertices without listing them"):
    val grid = Graph.grid(10_000, 10_000)
    assertEquals(grid.order, 100_000_000)
    assert(grid.hasVertex((9_999, 9_999)))
    assert(!grid.hasVertex((10_000, 0)))
    assert(grid.filterVertices(_ != (0, 0)).hasVertex((1, 1)))
    val union = grid | Graph.path(3)
    assert(union.hasEdge((0, 0) ~ (0, 1)))
    assert(union.hasEdge(0 ~ 1))
    assert(union.hasVertex(2))
    assertEquals(union.degree((5, 5)), 4)
    assertEquals(union.degree(1), 2)

  /**
    * Checks that the handles on the vertices of [[graph]] agree with those of
    * its materialisation.
    */
  private def assertHandles[M]
    (graph: GraphBase[Any, Any, M])
    (using Location)
    : Unit =
    assert(
      !graph.structure.stored,
      "The graph is stored.",
    )
    val built = graph.materialise
    graph
      .vertexValues
      .foreach: vertex =>
        assertEquals(
          graph(vertex).degree,
          built(vertex).degree,
        )
        assertEquals(
          graph(vertex).edges.map(_.value),
          built(vertex).edges.map(_.value),
        )
        assertEquals(
          graph(vertex).neighbours.map(_.value),
          built(vertex).neighbours.map(_.value),
        )
    assertEquals(
      graph.edges.map(_.value),
      built.edges.map(_.value),
    )

  /** Checks a directed graph's handles in the same way as [[assertHandles]]. */
  private def assertDirectedHandles[M]
    (graph: DirectedGraphBase[Any, Any, M])
    (using Location)
    : Unit =
    assert(
      !graph.structure.stored,
      "The graph is stored.",
    )
    val built = graph.materialise
    graph
      .vertexValues
      .foreach: vertex =>
        assertEquals(
          graph(vertex).outDegree,
          built(vertex).outDegree,
        )
        assertEquals(
          graph(vertex).inDegree,
          built(vertex).inDegree,
        )
        assertEquals(
          graph(vertex).outEdges.map(_.value),
          built(vertex).outEdges.map(_.value),
        )
        assertEquals(
          graph(vertex).inEdges.map(_.value),
          built(vertex).inEdges.map(_.value),
        )
        assertEquals(
          graph(vertex).edges.map(_.value),
          built(vertex).edges.map(_.value),
        )
    assertEquals(
      graph.edges.map(_.value),
      built.edges.map(_.value),
    )

  /**
    * @return
    *   The path through the given [[vertices]], built lazily from a shape, so
    *   that it's never few enough to copy.
    */
  private def lazyPath(vertices: Int*): Graph[Int, Unit] = Graph
    .path(vertices*)
    .mapVertices(identity)

  test("of equal vertices, every structure keeps the last, lazily or not"):
    val first  = Graph.path(1, 2)
    val second = Graph.path(1L, 3L)
    Seq[Graph[Any, Any]](
      first | second,
      first.mapVertices(identity) | second.mapVertices(identity),
      first + second.mapVertices(identity),
      first :~: second,
      Graph(first ~ second).flatten,
    ).foreach: graph =>
      assertEquals(
        graph.vertex(1).map(_.value.getClass),
        Some(classOf[java.lang.Long]),
      )
      assertEquals(
        graph.materialise.vertex(1).map(_.value.getClass),
        Some(classOf[java.lang.Long]),
      )

  test("joins, substitutions and unions count vertices and degrees cheaply"):
    val wheel = Graph.wheel(5_000_000)
    assertEquals(wheel.order, 5_000_001)
    assertEquals(wheel.degree(1), 3)
    assertEquals(wheel.degree(0), 5_000_000)
    assertEquals(
      (Graph.grid(3_000, 3_000) ⊔ Graph.path(3)).order,
      9_000_003,
    )
    assertEquals(
      (Graph.grid(3_000, 3_000) | Graph.torus(3_000, 3_000)).degree((0, 0)),
      4,
    )
    assertEquals(
      (Graph.complete(5_000) | Graph.path((5_000 until 15_000)*)).order,
      15_000,
    )

  test("equality compares orders first"):
    val clique = Graph.complete(50_000)
    assert(clique == clique)
    assert(clique != Graph.complete(50_001))
    assert(Graph.complete(300).materialise != Graph.complete(301))
    assert(clique != clique.filterVertices(_ != 7))

  test("mixed operations on large operands nest, but cost linearly in depth"):
    val start: Graph[Int, Unit] = Graph.cycle(3)
    val mixed                   = (0 until 500).foldLeft(start): (graph, i) =>
      graph - (0 ~ 1) + Graph.path((100 * i until 100 * i + 100)*)
    assert(mixed.hasVertex(49_999))
    assert(!mixed.hasEdge(0 ~ 1))
    assert(mixed.hasEdge(1 ~ 2))

  test("choosing how to count lists only what it needs"):
    val grid = Graph.grid(3_000, 3_000)
    assertEquals(
      (Graph((0, 0) ~ (0, 1)) - grid.edgeInduced(_ => true)).size,
      0,
    )
    assertEquals(Graph.fan(5_000_000).order, 5_000_001)
    assertEquals(
      Graph.wheel(5_000_000).order,
      5_000_001,
    )

  test("gathered edges keep their place among the operands"):
    val big     = Graph.complete(100)
    val gather  = (Graph.path(1, 2) | big) + Graph.vertices(1L)
    val later   = lazyPath(1, 3)
    val overlay = gather | later
    assertEquals(
      overlay.vertex(1).map(_.value.getClass),
      overlay.materialise.vertex(1).map(_.value.getClass),
    )
    assertEquals[Any, Any](
      overlay.vertex(1).map(_.value.getClass),
      Some(classOf[java.lang.Integer]),
    )
