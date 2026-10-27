package com.alecdorrington
package kneiphof

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/**
  * Evidence that a value of type [[B]] can be looked up among values of type
  * [[A]]: [[B]] isn't a [[Handle]], or the values are handles of its type, as
  * in a set of handles.
  *
  * A handle such as `g(1)` is never itself a vertex, so looking it up among
  * vertex values would silently find nothing. [[NotHandle]] is the evidence
  * that a value can be looked up among any values.
  */
@implicitNotFound(
  "${B} is a handle on a vertex or edge of a graph, not a vertex or edge itself, so it would never be found as one. Pass its `.value` instead, as in `g.hasVertex(g(1).value)`, or `g.induced(g(1).neighbours.map(_.value))` for a set of handles.",
)
final class Lookup[B, -A] private ()

/** The companion object for [[Lookup]]. */
object Lookup:

  /** Every type which isn't a handle can be looked up among any values. */
  given values: [B] => NotGiven[B <:< Handle] => Lookup[B, Any] = Lookup()

  /**
    * The empty type has no values, so it isn't a handle, though it's a subtype
    * of every type. This lets empty collections and calls through, as in
    * `Graph.vertices()`.
    */
  given nothing: Lookup[Nothing, Any] = Lookup()

  /** Handles can be looked up among handles of their own type. */
  given handles: [B <: Handle] => Lookup[B, B] = Lookup()
