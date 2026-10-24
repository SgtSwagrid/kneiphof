package com.alecdorrington.kneiphof

/** Tests for edge values and their equality. */
class EdgeSuite extends munit.FunSuite:

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
