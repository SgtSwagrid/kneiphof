package com.alecdorrington.kneiphof

/** Tests for directed graphs and hypergraphs. */
class DirectedGraphSuite extends munit.FunSuite:

  val graph: DirectedGraph[Int, Unit] = DirectedGraph(1 ~> 2, 1 ~> 3, 3 ~> 1)

  test("vertices know their successors and predecessors"):
    assertEquals(
      graph(1).successors.map(_.value),
      Set(2, 3),
    )
    assertEquals(
      graph(1).predecessors.map(_.value),
      Set(3),
    )
    assertEquals(graph(1).outDegree, 2)
    assertEquals(graph(1).inDegree, 1)
    assertEquals(graph(1).degree, 3)

  test("edges know their ends"):
    val edge = graph.edge(1 ~> 2).get
    assertEquals(edge.tail.map(_.value), Set(1))
    assertEquals(edge.head.map(_.value), Set(2))

  test("transposing reverses every edge"):
    assertEquals(
      graph.transpose,
      DirectedGraph(2 ~> 1, 3 ~> 1, 1 ~> 3),
    )

  test("forgetting direction merges opposite edges of a set"):
    assertEquals(
      DirectedGraph(1 ~> 2, 2 ~> 1).underlying,
      Graph(1 ~ 2),
    )
    assertEquals(
      DirectedMultigraph(1 ~> 2, 2 ~> 1).underlying.size,
      2,
    )

  test("sources and targets can have different types"):
    sealed trait Node
    case class Place(name: String)      extends Node
    case class Transition(name: String) extends Node
    val (p, t, q) = (Place("p"), Transition("t"), Place("q"))
    val arc       = p ~> t
    val net: DirectedGraph[Node, Unit] = DirectedGraph(arc, t ~> q)
    assertEquals(
      net(t).successors.map(_.value),
      Set[Node](q),
    )

  test("directed hyperedges lead between sets of vertices"):
    val hypergraph = DirectedHypergraph(DirectedHyperedge(Seq(1, 2), Seq(3)))
    assertEquals(
      hypergraph(1).successors.map(_.value),
      Set(3),
    )
    assertEquals(
      hypergraph(3).predecessors.map(_.value),
      Set(1, 2),
    )
    assertEquals(
      hypergraph.underlying,
      Hypergraph(Hyperedge(1, 2, 3)),
    )

  test("adding a hyperedge to a digraph gives a directed hypergraph"):
    val grown = graph + DirectedHyperedge(Seq(1, 2), Seq(4))
    assertEquals(grown.size, 4)
    assert(grown.hasEdge(DirectedHyperedge(Seq(2, 1), Seq(4))))
