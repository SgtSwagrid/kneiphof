package com.alecdorrington
package kneiphof

import scala.annotation.implicitNotFound
import scala.util.NotGiven

/**
  * Evidence that [[F]] isn't a graph, or an edge (which is a graph), so that it
  * can label an edge with `%` or `labelled`.
  *
  * This catches a mistake of precedence: `%` binds more loosely than `~`, so
  * `1 ~ 2 % "a" ~ 3` means `(1 ~ 2) % ("a" ~ 3)`, which would label `1 ~ 2`
  * with the edge `"a" ~ 3`. An edge can still be labelled by a graph through
  * its constructor, as in `Edge(1, 2, graph)`.
  */
@implicitNotFound(
  "${F} is an edge or graph, so it can't label an edge with `%`. Since `%` binds more loosely than `~`, label an edge in the middle of a chain with brackets, as in `(1 ~ 2 % label) ~ 3`.",
)
final class NotGraph[F] private ()

/** The companion object for [[NotGraph]]. */
object NotGraph:

  /** Every type which isn't a graph can label an edge. */
  given [F] => NotGiven[F <:< AnyGraph[?, ?, ?]] => NotGraph[F] = NotGraph()
