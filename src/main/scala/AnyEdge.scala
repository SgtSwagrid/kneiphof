package com.alecdorrington
package kneiphof

/**
  * An edge of any kind, joining vertices of type [[V]] and carrying a label of
  * type [[E]].
  *
  * Edges are plain values: two edges are equal exactly when they join the same
  * vertices in the same way and carry equal labels. Every edge is also the
  * multigraph of its kind containing just itself, once.
  *
  * @tparam X
  *   The kind of edge, which transformations preserve.
  */
trait AnyEdge[+V, +E, +X[+v, +e]]:

  /** The distinct vertices incident to this edge. */
  def vertexValues: ValueSet[V]

  /** The label of this edge, which is `()` for unlabelled edges. */
  def label: E

  /** @return The number of times [[vertex]] occurs as an end of this edge. */
  def occurrences[A : NotHandle](vertex: A): Int

  /**
    * @return
    *   This edge, with each vertex transformed by [[vertexMap]] and the label
    *   transformed by [[labelMap]].
    */
  def map[W : NotHandle, F](vertexMap: V => W, labelMap: E => F): X[W, F]

  /** @return This edge, with each vertex transformed by [[transform]]. */
  def mapVertices[W : NotHandle](transform: V => W): X[W, E]

  /**
    * @return
    *   The edges obtained by replacing each end of this edge with one of its
    *   [[choices]].
    */
  def substitute[W : NotHandle](choices: V => Iterable[W]): Iterable[X[W, E]]

  /** @return This edge, with its label transformed by [[transform]]. */
  def mapLabel[F](transform: E => F): X[V, F] = map(identity, transform)

  /**
    * @return
    *   This edge, with its label replaced by [[label]], which can't be an edge
    *   or graph (see [[NotGraph]]).
    */
  def labelled[F : NotGraph](label: F): X[V, F] = mapLabel(_ => label)

  /** Alias of [[labelled]]. */
  infix def % [F : NotGraph](label: F): X[V, F] = labelled(label)

  /**
    * A hash of the ends and label of this edge, consistent with equality
    * between edges, which graphs use to hash their edges without hashing the
    * edges themselves.
    */
  def shapeHash: Int
