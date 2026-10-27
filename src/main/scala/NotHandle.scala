package com.alecdorrington
package kneiphof

/**
  * Evidence that [[A]] isn't a [[Handle]], so that it can be looked up as a
  * vertex or edge value, or become a vertex (see [[Lookup]]).
  *
  * This catches a mistake that would otherwise fail silently: a handle such as
  * `g(1)` is never itself a vertex, so `g.hasVertex(g(1))` would be `false`,
  * and `g.removeVertices(g(1).neighbours)` would remove nothing. Every method
  * which looks up a vertex or edge, or adds vertices, requires it, so handles
  * don't become vertices either. Only the type of a value is checked, so a
  * handle can still become a vertex as an [[Any]], in a collection whose
  * elements are only known to be [[Any]], or through an edge's own constructor,
  * such as `Edge(a, b)`.
  */
type NotHandle[A] = Lookup[A, Any]
