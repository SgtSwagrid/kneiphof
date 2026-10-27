package com.alecdorrington
package kneiphof

/**
  * A handle on a vertex or edge of a graph, such as `g(1)`, from which its
  * neighbourhood can be explored.
  *
  * A handle stands for its [[value]], but isn't it. Queries and updates such as
  * `hasVertex` and `induced` take vertex and edge values, and reject handles at
  * compile time (see [[NotHandle]]), since a handle would never be found as a
  * vertex or edge.
  */
trait Handle:

  /** The vertex or edge value this handle stands for. */
  def value: Any
