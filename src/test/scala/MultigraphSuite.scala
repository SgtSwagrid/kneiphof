package com.alecdorrington.kneiphof

import munit.FunSuite

/** Tests for graphs with edge counts. */
class MultigraphSuite extends FunSuite:

  test("repeated edges are counted"):
    val graph = Multigraph(1 ~ 2, 1 ~ 2)
    assertEquals(graph.size, 2)
    assertEquals(graph.multiplicity(2 ~ 1), Count(2))
    assertEquals(graph(1).degree, 2)

  test("sum adds counts, while union takes the maximum"):
    val graph = Multigraph(1 ~ 2, 1 ~ 2, 2 ~ 3)
    assertEquals((graph + graph).size, 6)
    assertEquals(graph | graph, graph)
    assertEquals(graph & Multigraph(1 ~ 2), Multigraph(1 ~ 2))

  test("edge sets and multigraphs convert into each other explicitly"):
    val counts = Multigraph(1 ~ 2, 1 ~ 2, 2 ~ 3)
    val set    = Graph(1 ~ 2, 3 ~ 4)
    assertEquals(counts.support, Graph(1 ~ 2, 2 ~ 3))
    assertEquals(set.support, set)
    assertEquals(set.counted, Multigraph(1 ~ 2, 3 ~ 4))
    assertEquals(set.counted.size, 2)
    assertEquals(
      counts & set.counted,
      Multigraph(1 ~ 2).addVertex(3),
    )
    assertEquals(
      counts - set.counted,
      Multigraph(1 ~ 2, 2 ~ 3),
    )
    assertEquals(
      counts.filterEdges(edge => !set.hasEdge(edge)),
      Multigraph(2 ~ 3).addVertex(1),
    )
    assertEquals((1 ~ 2 ~ 1).support, Graph(1 ~ 2))
    assertEquals(
      DirectedMultigraph(1 ~> 2, 1 ~> 2).support,
      DirectedGraph(1 ~> 2),
    )
    assertEquals(
      DirectedGraph(1 ~> 2).counted,
      DirectedMultigraph(1 ~> 2),
    )
    assertEquals(
      Multihypergraph(Hyperedge(1, 2, 3), Hyperedge(1, 2, 3)).support,
      Hypergraph(Hyperedge(1, 2, 3)),
    )
    assert(compileErrors("Multigraph(1 ~ 2).counted").nonEmpty)
    assert(compileErrors("Multigraph(1 ~ 2) & Graph(1 ~ 2)").nonEmpty)
    val ownEmbedding = compileErrors(
      "new Embedding[Boolean, Count] { def apply(present: Boolean) = Count(1); def exact = true }",
    )
    assert(
      ownEmbedding.contains("sealed"),
      ownEmbedding,
    )

  test("difference removes copies, stopping at zero"):
    val graph = Multigraph(1 ~ 2, 1 ~ 2)
    assertEquals((graph - Multigraph(1 ~ 2)).size, 1)
    assertEquals((graph - graph - graph).size, 0)
    assertEquals((graph - graph).order, 2)
    assertEquals((graph - (1 ~ 2)).size, 1)

  test("merging vertices keeps parallel edges"):
    val merged = Multigraph(1 ~ 2, 2 ~ 3).mapVertices(_ => 0)
    assertEquals(merged.size, 2)
    assertEquals(merged(0).degree, 4)
    assertEquals(
      Graph(1 ~ 2, 2 ~ 3).mapVertices(_ => 0).size,
      1,
    )

  test("counts can't be negative"):
    intercept[IllegalArgumentException](Count(-1))

  test("multigraphs print their counts"):
    assertEquals(
      Multigraph(1 ~ 2, 1 ~ 2).toString,
      "Graph(1 ~ 2 × 2)",
    )
