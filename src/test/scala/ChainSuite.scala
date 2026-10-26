package com.alecdorrington.kneiphof

/** Tests for edges and chains as multigraphs. */
class ChainSuite extends munit.FunSuite:

  test("an edge is the multigraph of itself"):
    assertEquals[Any, Any](1 ~ 2, Multigraph(1 ~ 2))
    assertNotEquals[Any, Any](1 ~ 2, Graph(1 ~ 2))
    assertEquals(
      Multigraph(1 ~ 2).hashCode,
      (1 ~ 2).hashCode,
    )
    assertEquals((1 ~ 2).size, 1)
    assertEquals(
      (1 ~ 2)(1).neighbours.map(_.value),
      Set(2),
    )

  test("chains hold their edges"):
    assertEquals[Any, Any](1 ~ 2 ~ 3, Multigraph(1 ~ 2, 2 ~ 3))
    assertEquals[Any, Any](1 ~ 2 ~ 3, 3 ~ 2 ~ 1)
    assertNotEquals[Any, Any](1 ~ 2 ~ 3, 1 ~ 3 ~ 2)
    assertEquals((1 ~ 2 ~ 1).size, 2)

  test("% labels the last edge of a chain"):
    assertEquals[Any, Any](
      (1 ~ 2 % "a") ~ 3 % "b",
      Multigraph(1 ~ 2 % "a", 2 ~ 3 % "b"),
    )
    assertEquals[Any, Any](
      1 ~ 2 ~ 3 % "x",
      Multigraph[Int, Unit | String](1 ~ 2, 2 ~ 3 % "x"),
    )

  test("directed chains follow their arrows"):
    assertEquals[Any, Any](1 ~> 2 ~> 3, DirectedMultigraph(1 ~> 2, 2 ~> 3))
    assertNotEquals[Any, Any](1 ~> 2 ~> 3, 3 ~> 2 ~> 1)

  test("chains build graphs of every kind"):
    assertEquals(
      Graph(1 ~ 2 ~ 3 ~ 4, 4 ~ 5),
      Graph(1 ~ 2, 2 ~ 3, 3 ~ 4, 4 ~ 5),
    )
    assertEquals(Graph(1 ~ 2 ~ 1).size, 1)
    assertEquals(Multigraph(1 ~ 2 ~ 1).size, 2)
    assertEquals(
      Hypergraph(Hyperedge(1, 2, 3), 3 ~ 4 ~ 5).size,
      3,
    )
    assertEquals(
      DirectedGraph(1 ~> 2 ~> 3),
      DirectedGraph(1 ~> 2) + (2 ~> 3),
    )

  test("edges and chains combine with graphs of any multiplicity"):
    assertEquals(Graph(1 ~ 2) + (2 ~ 3 ~ 4), Graph(1 ~ 2, 2 ~ 3, 3 ~ 4))
    assertEquals(Graph(1 ~ 2) | (2 ~ 3), Graph(1 ~ 2, 2 ~ 3))
    assertEquals(
      Graph(1 ~ 2 ~ 3) - (1 ~ 2 ~ 3),
      Graph.vertices(1, 2, 3),
    )
    assertEquals(Graph(1 ~ 2, 2 ~ 3) & (2 ~ 3), Graph(2 ~ 3))

  test("a closed chain joined to a hub is a wheel"):
    val wheel = 1 ~ 2 ~ 3 ~ 4 ~ 5 ~ 1 :~ 0
    assertEquals(wheel.size, 10)
    assertEquals(wheel(0).degree, 5)
    assert(wheel.vertices.filter(_.value != 0).forall(_.degree == 3))
    assertEquals(
      Graph(wheel),
      Graph(1 ~ 2 ~ 3 ~ 4 ~ 5 ~ 1) | (Graph.vertices(1, 2, 3, 4, 5) :~ 0),
    )

  test("edges and graphs can't label an edge with %"):
    Seq(
      compileErrors("1 ~ 2 % \"a\" ~ 3"),
      compileErrors("(1 ~> 2) % Graph(3 ~ 4)"),
      compileErrors("(Graph.path(3) :~ 9) % (1 ~ 2)"),
    ).foreach(errors =>
      assert(
        errors.contains("can't label an edge"),
        errors,
      ),
    )
    assertEquals[Any, Any]((1 ~ 2 % "a") ~ 3, Multigraph(1 ~ 2 % "a", 2 ~ 3))
    assertEquals(Edge(1, 2, Graph(3 ~ 4)).label, Graph(3 ~ 4))
