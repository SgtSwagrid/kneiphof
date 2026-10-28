package com.alecdorrington.kneiphof

import munit.FunSuite

/** Tests for directed graphs and hypergraphs. */
class DirectedGraphSuite extends FunSuite:

  test("directed hyperedges count repeated vertices in their tails and heads"):
    val edge = DirectedHyperedge(Seq(1, 1, 2), Seq(3))
    assertEquals(edge.occurrences(1), 2)
    assertEquals[Any, Any](
      edge,
      DirectedHyperedge(Seq(2, 1, 1), Seq(3)),
    )
    assertEquals(
      edge.##,
      DirectedHyperedge(Seq(1, 2, 1), Seq(3)).##,
    )
    assertNotEquals[Any, Any](
      edge,
      DirectedHyperedge(Seq(1, 2), Seq(3)),
    )
    assertEquals(
      DirectedMultihypergraph(edge)(1).outDegree,
      2,
    )
    val merge: Int => Int = vertex => if vertex == 3 then 3 else 0
    val graph  = DirectedMultihypergraph(DirectedHyperedge(Seq(1, 2), Seq(3)))
    val merged = graph.mapVertices(merge)
    assertEquals(merged.degree(0), 2)
    assertEquals(merged.materialise.degree(0), 2)
    assertEquals(
      merged.underlying,
      graph.underlying.mapVertices(merge),
    )

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

  test("transposing reverses every edge, and forgetting direction merges them"):
    assertEquals(
      graph.transpose,
      DirectedGraph(2 ~> 1, 3 ~> 1, 1 ~> 3),
    )
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

  test("directed hyperedges lead between sets of vertices, even in a digraph"):
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
    val grown = graph + DirectedHyperedge(Seq(1, 2), Seq(4))
    assertEquals(grown.size, 4)
    assert(grown.hasEdge(DirectedHyperedge(Seq(2, 1), Seq(4))))

  test("directed joins lead from left to right"):
    val sources                          = DirectedGraph.vertices(1, 2)
    val joined: DirectedGraph[Int, Unit] = sources :~>: DirectedGraph.vertex(3)
    assertEquals(joined, DirectedGraph(1 ~> 3, 2 ~> 3))
    assertEquals[Any, Any](sources :~> 3, joined)
    assertEquals[Any, Any](0 ~>: sources, DirectedGraph(0 ~> 1, 0 ~> 2))
    val labelled: DirectedGraph[Int, String] = (sources :~> 3) % "x"
    assertEquals(
      labelled,
      DirectedGraph(1 ~> 3 % "x", 2 ~> 3 % "x"),
    )
    assertEquals[Any, Any](
      DirectedGraph(1 ~> 2 % "a").join(DirectedGraph.vertex(3)) % "b",
      DirectedGraph(1 ~> 2 % "a", 1 ~> 3 % "b", 2 ~> 3 % "b"),
    )
