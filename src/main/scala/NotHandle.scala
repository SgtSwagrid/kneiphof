package com.alecdorrington
package kneiphof

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/**
  * Evidence that [[A]] isn't a [[Handle]], so that it can be looked up as a
  * vertex or edge value.
  *
  * This catches a mistake that would otherwise fail silently: a handle such as
  * `g(1)` is never itself a vertex, so `g.hasVertex(g(1))` would be `false`,
  * and `g.removeVertices(g(1).neighbours)` would remove nothing.
  */
@implicitNotFound(
  "${A} is a handle on a vertex or edge of a graph, not a vertex or edge itself, so it would never be found as one. Pass its `.value` instead, as in `g.hasVertex(g(1).value)`, or `g.induced(g(1).neighbours.map(_.value))` for a set of handles.",
)
final class NotHandle[A] private ()

/** The companion object for [[NotHandle]]. */
object NotHandle:

  /** Every type which isn't a handle can be looked up as a vertex or edge. */
  given [A] => NotGiven[A <:< Handle] => NotHandle[A] = NotHandle()
