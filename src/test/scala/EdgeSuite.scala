package com.alecdorrington.kneiphof

import munit.FunSuite

/** Tests for edge values and their equality. */
class EdgeSuite extends FunSuite:

  test("undirected edges ignore the order of their ends"):
    assertEquals(1 ~ 2, 2 ~ 1)
    assertEquals((1 ~ 2).hashCode, (2 ~ 1).hashCode)

  test("labels distinguish otherwise equal edges"):
    assertNotEquals[Any, Any](1 ~ 2 % "a", 1 ~ 2 % "b")
    assertEquals(1 ~ 2              % "a", 2 ~ 1 % "a")

  test("a hyperedge with two ends is an edge"):
    assertEquals[Any, Any](Hyperedge(1, 2), 1 ~ 2)
    assert(Hyperedge(1, 2).isInstanceOf[Edge[?, ?]])

  test("hyperedges count repeated ends"):
    assertNotEquals(Hyperedge(1, 1, 2), Hyperedge(1, 2, 2))
    assertEquals(Hyperedge(1, 1, 2), Hyperedge(2, 1, 1))
    assertEquals(Hyperedge(1, 1, 2).occurrences(1), 2)
    assertEquals((3 ~ 3).occurrences(3), 2)

  test("directed edges respect direction"):
    assertNotEquals(1 ~> 2, 2 ~> 1)
    assertEquals((1 ~> 2).reverse, 2 ~> 1)
    assertEquals[Any, Any](DirectedHyperedge(Seq(1), Seq(2)), 1 ~> 2)

  test("mapping an edge preserves its kind"):
    val edge: Edge[Int, Unit] = (1 ~ 2).mapVertices(_ * 10)
    assertEquals(edge, 10 ~ 20)
    assertEquals((1 ~> 2 % "x").mapLabel(_.length), 1 ~> 2 % 1)

  test("value sets are covariant and compare by equality"):
    val set: ValueSet[Any] = ValueSet(1, 2)
    assert(set.contains(1))
    assert(!set.contains("1"))
    assertEquals(set + 3, ValueSet(3, 2, 1))

  test("edges of different families are never equal"):
    assertNotEquals[Any, Any](1 ~ 2, 1 ~> 2)
    assertNotEquals[Any, Any](1 ~> 2, 1 ~ 2)
    assert(!Graph(1 ~ 2).hasEdge(1 ~> 2))
    assertNotEquals[Any, Any](Graph(1 ~ 2), DirectedGraph(1 ~> 2))

  test("& and - only take graphs of the same direction"):
    Seq(
      compileErrors("Graph(1 ~ 2) - (1 ~> 2)"),
      compileErrors("Graph(1 ~ 2) & DirectedGraph(1 ~> 2)"),
    ).foreach(errors =>
      assert(
        errors.contains("Required: com.alecdorrington.kneiphof.HypergraphBase"),
        errors,
      ),
    )
    Seq(
      compileErrors("DirectedGraph(1 ~> 2) - (1 ~ 2)"),
      compileErrors("DirectedGraph(1 ~> 2) & Hypergraph(Hyperedge(1, 2))"),
    ).foreach(errors =>
      assert(
        errors.contains(
          "Required: com.alecdorrington.kneiphof.DirectedHypergraphBase",
        ),
        errors,
      ),
    )
    assertEquals(
      Graph(1 ~ 2, 2 ~ 3) & Hypergraph(Hyperedge(1, 2)),
      Graph(1 ~ 2),
    )
    assertEquals(
      Graph(1 ~ 2, 2 ~ 3) - Hypergraph(Hyperedge(1, 2)),
      Graph(2 ~ 3).addVertex(1),
    )

  test("every edge has an end"):
    intercept[IllegalArgumentException](Hyperedge())
    intercept[IllegalArgumentException](DirectedHyperedge(Nil, Nil))
    assertEquals(
      DirectedHyperedge(Seq(1), Nil).vertexValues,
      ValueSet(1),
    )

  test("sizes and degrees fail rather than overflow"):
    val loop =
      Multigraph.fromMultiplicities(Nil, Seq((1 ~ 1) -> Count(1 << 30)))
    intercept[ArithmeticException](loop.degree(1))
