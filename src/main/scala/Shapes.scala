package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.shapes.{Indexing, Shape}

/**
  * Constructors for common shapes of unlabelled graph, of kind [[G]].
  *
  * A shape's edges are never stored: queries such as `hasEdge`, `size` and
  * vertex degrees are answered from the shape, and the edges are only built if
  * something needs all of them, such as iterating over them.
  *
  * Each shape can be built on the vertices `0, 1, ...`, given how many there
  * are, as in `clique(3)`, or on the given distinct vertices, as in
  * `clique("A", "B", "C")`.
  */
trait Shapes[+G[+v, +e]]:

  /**
    * @return
    *   The complete graph on the vertices `0 until order`, in which every two
    *   vertices are adjacent (in both directions, for directed graphs).
    */
  def clique(order: Int): G[Int, Unit] = counted(order)(complete)

  /** @return The complete graph on the given [[vertices]]. */
  def clique[V](vertices: V*): G[V, Unit] = listed(vertices)(complete)

  /** @return The path through the vertices `0 until order`, in order. */
  def path(order: Int): G[Int, Unit] = counted(order)(Shape.Path(_))

  /** @return The path through the given [[vertices]], in order. */
  def path[V](vertices: V*): G[V, Unit] = listed(vertices)(Shape.Path(_))

  /**
    * @return
    *   The cycle through the vertices `0 until order`, in order, of which there
    *   must be at least `3`.
    */
  def cycle(order: Int): G[Int, Unit] =
    counted(order)(order => Shape.Cycle(cyclic(order)))

  /**
    * @return
    *   The cycle through the given [[vertices]], in order, of which there must
    *   be at least `3`.
    */
  def cycle[V](vertices: V*): G[V, Unit] =
    listed(vertices)(order => Shape.Cycle(cyclic(order)))

  /**
    * @return
    *   The star with the centre `0` adjacent to each of the leaves
    *   `1 to leaves` (pointing outwards, for directed graphs).
    */
  def star(leaves: Int): G[Int, Unit] = counted(leaves + 1)(Shape.Star(_))

  /** @return The star with [[centre]] adjacent to each of the [[leaves]]. */
  def star[V](centre: V, leaves: V*): G[V, Unit] =
    listed(centre +: leaves)(Shape.Star(_))

  /**
    * @return
    *   The wheel with the hub `0` adjacent to each of the vertices `1 to rim`,
    *   which form a cycle. There must be at least `3` of them.
    */
  def wheel(rim: Int): G[Int, Unit] =
    counted(rim + 1)(order => Shape.Wheel(cyclic(order - 1) + 1))

  /**
    * @return
    *   The wheel with [[hub]] adjacent to each of the [[rim]] vertices, which
    *   form a cycle in order. There must be at least `3` of them.
    */
  def wheel[V](hub: V, rim: V*): G[V, Unit] =
    listed(hub +: rim)(order => Shape.Wheel(cyclic(order - 1) + 1))

  /**
    * @return
    *   The fan with the hub `0` adjacent to each of the vertices `1 to rim`,
    *   which form a path.
    */
  def fan(rim: Int): G[Int, Unit] = counted(rim + 1)(Shape.Fan(_))

  /**
    * @return
    *   The fan with [[hub]] adjacent to each of the [[rim]] vertices, which
    *   form a path in order.
    */
  def fan[V](hub: V, rim: V*): G[V, Unit] = listed(hub +: rim)(Shape.Fan(_))

  /**
    * @return
    *   The complete tree on the vertices `0 until order`, in which each vertex
    *   has up to [[branching]] children (pointing away from the root `0`, for
    *   directed graphs). The vertices are numbered in breadth-first order, so
    *   the children of `v` are `branching * v + 1` to `branching * v +
    *   branching`.
    */
  def tree(branching: Int, order: Int): G[Int, Unit] =
    counted(order)(Shape.Tree(_, branches(branching)))

  /**
    * @return
    *   The complete tree on the given [[vertices]] in breadth-first order, in
    *   which each vertex has up to [[branching]] children.
    */
  def tree[V](branching: Int, vertices: V*): G[V, Unit] =
    listed(vertices)(Shape.Tree(_, branches(branching)))

  /** @return The shape on the given [[vertices]], from their number. */
  private[kneiphof] def shaped[V](vertices: Indexing[V], shape: Shape)
    : G[V, Unit]

  /**
    * @return
    *   The shape of a complete graph of this kind on [[order]] vertices.
    */
  private[kneiphof] def complete(order: Int): Shape

  /** @return The shape [[shape]] on the vertices `0 until order`. */
  private def counted(order: Int)(shape: Int => Shape): G[Int, Unit] =
    shaped(Indexing.Counting(order), shape(order))

  /** @return The shape [[shape]] on the given [[vertices]]. */
  private def listed[V](vertices: Seq[V])(shape: Int => Shape): G[V, Unit] =
    shaped(
      Indexing.Listing(vertices.toIndexedSeq),
      shape(vertices.size),
    )

  /** @return [[order]], checking that a cycle of that order is simple. */
  private def cyclic(order: Int): Int =
    require(
      order >= 3,
      s"A cycle needs at least 3 vertices, but had $order.",
    )
    order

  /** @return [[branching]], checking that a tree can branch that much. */
  private def branches(branching: Int): Int =
    require(
      branching >= 1,
      s"A tree can't have $branching children per vertex.",
    )
    branching
