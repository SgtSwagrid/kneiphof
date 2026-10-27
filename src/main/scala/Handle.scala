package com.alecdorrington
package kneiphof

/**
  * A handle on a vertex or edge of a graph, such as `g(1)`, from which its
  * neighbourhood can be explored.
  *
  * A handle stands for its [[value]], but isn't it. Queries and updates such as
  * [[AnyGraph.hasVertex]], [[GraphOps.induced]] and [[GraphOps.addVertex]] take
  * vertex and edge values, and reject handles at compile time (see
  * [[NotHandle]]), since a handle would never be found as a vertex or edge. Two
  * handles are equal when they're on the same vertex, or the same edge, of the
  * same graph.
  */
trait Handle:

  /** The vertex or edge value this handle stands for. */
  def value: Any

  /** The graph this handle belongs to. */
  def graph: AnyGraph[?, ?, ?]

  /** `true` if and only if this handle is on an edge, rather than a vertex. */
  private[kneiphof] def onEdge: Boolean

  override final def equals(that: Any): Boolean = that match
    case that: Handle => (that.graph eq graph) && that.onEdge == onEdge &&
      that.value == value
    case _ => false

  override final def hashCode: Int = value.##

  override final def toString: String = value.toString
