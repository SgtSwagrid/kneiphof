package com.alecdorrington.kneiphof

import munit.{Assertions, Location}

/** Assertions that lazy graphs agree with the graphs they build. */
trait LazyAssertions extends Assertions:

  /**
    * Checks that the queries [[graph]] answers lazily agree with the graph it
    * builds, both for its own vertices and edges and for the [[absent]] ones.
    */
  def assertLazy[M : Multiplicity]
    (
      graph: GraphBase[Any, Any, M],
      absent: Any*,
    )
    (using Location)
    : Unit = assertAgrees[M, Edge, [v, e] =>> GraphBase[v, e, M]](
    graph,
    GraphBase
      .kind[M]
      .fromMultiplicities(
        graph.materialise.vertexValues,
        graph.materialise.edgeValues,
      ),
    absent,
  )

  /** Checks a directed graph in the same way as [[assertLazy]]. */
  def assertLazyDirected[M : Multiplicity]
    (
      graph: DirectedGraphBase[Any, Any, M],
      absent: Any*,
    )
    (using Location)
    : Unit = assertAgrees[
    M,
    Arc,
    [v, e] =>> DirectedGraphBase[v, e, M],
  ](
    graph,
    DirectedGraphBase
      .kind[M]
      .fromMultiplicities(
        graph.materialise.vertexValues,
        graph.materialise.edgeValues,
      ),
    absent,
  )

  /**
    * Checks that the lazy [[graph]] answers every query as [[built]], which is
    * made independently from what it builds, does.
    */
  private def assertAgrees[
    M,
    X[+v, +e] <: AnyEdge[v, e, X],
    G[+v, +e] <: GraphOps[v, e, M, X, G],
  ]
    (
      graph: G[Any, Any],
      built: G[Any, Any],
      absent: Seq[Any],
    )
    (using Location)
    : Unit =
    assert(
      !graph.structure.stored,
      "The graph is stored.",
    )
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
      assertEquals(
        graph.edge(edge).map(_.value),
        built.edge(edge).map(_.value),
      )
