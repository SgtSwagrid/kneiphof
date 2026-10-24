package com.alecdorrington
package kneiphof

/**
  * An edge of any kind, joining vertices of type [[V]] and carrying a label of
  * type [[E]].
  *
  * Edges are plain values: two edges are equal exactly when they join the same
  * vertices in the same way and carry equal labels.
  *
  * @tparam X
  *   The kind of edge, which transformations preserve.
  */
trait AnyEdge[+V, +E, +X[+v, +e]]:

  /** The distinct vertices incident to this edge. */
  def vertices: ValueSet[V]

  /** The label of this edge, which is `()` for unlabelled edges. */
  def label: E

  /** @return The number of times [[vertex]] occurs as an end of this edge. */
  def occurrences(vertex: Any): Int

  /**
    * @return
    *   This edge, with each vertex transformed by [[vertexMap]] and the label
    *   transformed by [[labelMap]].
    */
  def map[W, F](vertexMap: V => W, labelMap: E => F): X[W, F]

  /**
    * @return
    *   Every edge obtained by replacing each end of this edge with one of its
    *   [[choices]].
    */
  def flatMapVertices[W](choices: V => Iterable[W]): Iterable[X[W, E]]

  /** @return This edge, with each vertex transformed by [[transform]]. */
  def mapVertices[W](transform: V => W): X[W, E] = map(transform, identity)

  /** @return This edge, with its label transformed by [[transform]]. */
  def mapLabel[F](transform: E => F): X[V, F] = map(identity, transform)

  /** @return This edge, with its label replaced by [[label]]. */
  infix def % [F](label: F): X[V, F] = mapLabel(_ => label)
