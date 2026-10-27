package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.shapes.Rule

/**
  * Constructors for common shapes of unlabelled directed graph, of kind [[G]],
  * including those which only make sense for directed graphs.
  */
trait DirectedShapes[+G[+v, +e]] extends Shapes[G]:

  override private[kneiphof] def directed: Boolean = true

  /**
    * @return
    *   The transitive tournament on the vertices `0 until order`, with an edge
    *   from each vertex to every later one.
    */
  def transitiveTournament(order: Int): G[Int, Unit] =
    onNumbers(order)(Rule.Clique(_))

  /**
    * @return
    *   The transitive tournament on the given [[vertices]], with an edge from
    *   each vertex to every later one.
    */
  def transitiveTournament[V : NotHandle](vertices: V*): G[V, Unit] =
    onVertices(vertices)(Rule.Clique(_))
