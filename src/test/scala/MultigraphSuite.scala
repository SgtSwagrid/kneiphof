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
