package com.alecdorrington.kneiphof

/** Assertions that lazy graphs agree with the graphs they build. */
trait LazyAssertions extends munit.Assertions:

  /**
    * Checks that the queries [[graph]] answers lazily agree with the graph it
    * builds, both for its own vertices and edges and for the [[absent]] ones.
    */
  def assertLazy[M : Multiplicity]
    (
      graph: GraphBase[Any, Any, M],
      absent: Any*,
    )
    (using munit.Location)
    : Unit =
    assert(!graph.eager, "The graph isn't lazy.")
    val built = GraphBase
      .kind[M]
      .fromMultiplicities(graph.vertexValues, graph.edgeValues)
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
      assertEquals(
        graph.degree(vertex),
        built.degree(vertex),
      )
    (built.edgeValues.keys ++ absent).foreach: edge =>
      assertEquals(
        graph.multiplicity(edge),
        built.multiplicity(edge),
      )

  /** Checks a directed graph in the same way as [[assertLazy]]. */
  def assertLazyDirected[M : Multiplicity]
    (
      graph: DirectedGraphBase[Any, Any, M],
      absent: Any*,
    )
    (using munit.Location)
    : Unit =
    assert(!graph.eager, "The graph isn't lazy.")
    val built = DirectedGraphBase
      .kind[M]
      .fromMultiplicities(graph.vertexValues, graph.edgeValues)
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
      assertEquals(
        graph.degree(vertex),
        built.degree(vertex),
      )
    (built.edgeValues.keys ++ absent).foreach: edge =>
      assertEquals(
        graph.multiplicity(edge),
        built.multiplicity(edge),
      )
