package com.alecdorrington.kneiphof

import munit.FunSuite

/** Tests that counts of vertices and edges fail rather than overflow. */
class ArithmeticSuite extends FunSuite:

  /** A multigraph with as many copies of one edge as an `Int` holds. */
  private val heavy =
    Multigraph.fromMultiplicities(Nil, Seq((1 ~ 2) -> Count(Int.MaxValue)))

  test("joins and cones fail rather than overflow"):
    assertEquals(heavy.degree(1), Int.MaxValue)
    intercept[ArithmeticException](heavy.cone(3).degree(1))
    intercept[ArithmeticException](heavy.cone(3).size)

  test("substitutions fail rather than overflow"):
    val product = heavy.lexicographicProduct(Multigraph.vertices(0, 1))
    intercept[ArithmeticException](product.degree((1, 0)))
    intercept[ArithmeticException](product.size)

  test("in- and out-degrees fail rather than overflow"):
    val graph = DirectedMultigraph.fromMultiplicities(
      Nil,
      Seq(
        (1 ~> 2) -> Count(Int.MaxValue),
        (1 ~> 3) -> Count(1),
      ),
    )
    intercept[ArithmeticException](graph(1).outDegree)
    intercept[ArithmeticException](graph.transpose(1).inDegree)
    intercept[ArithmeticException](graph.degree(1))

  test("large shapes fail rather than overflow"):
    intercept[ArithmeticException](Graph.petersen(800_000_000, 2).size)
    intercept[ArithmeticException]:
      DirectedGraph.complete(1_100_000_000, 1).degree(1_100_000_000)
    intercept[IllegalArgumentException](Graph.petersen(4, 1_073_741_826))
    intercept[IllegalArgumentException](Graph.petersen(5, 1_073_741_825))
    intercept[ArithmeticException](Graph.star(Int.MaxValue))
    intercept[ArithmeticException](Graph.petersen(1_100_000_000, 2))
