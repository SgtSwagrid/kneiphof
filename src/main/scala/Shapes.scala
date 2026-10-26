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
  * are, as in `path(3)`. Shapes whose vertices come in a single sequence can
  * also be built on the given distinct vertices, as in `path("A", "B", "C")`.
  * Complete graphs are the exception, with [[complete]] taking sizes and
  * [[clique]] taking vertices, so that `clique(3)` can't be mistaken for
  * `complete(3)`.
  */
trait Shapes[+G[+v, +e]]:

  /**
    * @return
    *   The clique on the given [[vertices]], in which every two are adjacent
    *   (in both directions, for directed graphs). The complete graph on the
    *   vertices `0 until n` is `complete(n)`.
    */
  def clique[V](vertices: V*): G[V, Unit] = listed(vertices)(allPairs)

  /**
    * @return
    *   The complete graph with parts of the given [[sizes]], following the
    *   notation `K_n`, `K_{m, n}`, and so on, on the vertices `0, 1, ...`. A
    *   single size `n` gives the complete graph on `n` vertices, in which every
    *   two vertices are adjacent. Several sizes give the complete multipartite
    *   graph, whose vertices are split into parts of those sizes in order, and
    *   in which every two vertices in different parts are adjacent. Adjacent
    *   vertices are joined both ways, for directed graphs. So `complete(3, 4)`
    *   is the complete bipartite graph, and `complete(1, 1, 1)` is
    *   `complete(3)`.
    */
  def complete(sizes: Int*): G[Int, Unit] = counted(total(sizes)): order =>
    sizes match
      case Seq(_) => allPairs(order)
      case _      => Shape.Multipartite(sizes, directed)

  /**
    * @return
    *   The hypercube of the given [[dimension]], on the vertices
    *   `0 until 2^dimension`, in which two vertices are adjacent when their
    *   binary forms differ in exactly one bit (pointing to the one with the bit
    *   set, for directed graphs).
    */
  def hypercube(dimension: Int): G[Int, Unit] =
    counted(1 << dimensions(dimension))(_ => Shape.Hypercube(dimension))

  /**
    * @return
    *   The circulant graph on the vertices `0 until order`, in which each
    *   vertex `i` is adjacent to `i + jump`, going round modulo [[order]], for
    *   each of the [[jumps]] (pointing from `i`, for directed graphs). So
    *   `circulant(n, 1)` is `cycle(n)`, and `circulant(2 * n, 1, n)` is the
    *   Möbius ladder.
    */
  def circulant(order: Int, jumps: Int*): G[Int, Unit] = counted(order):
    order => Shape.Circulant(order, loopless(order, jumps), directed)

  /** @return The Petersen graph, which is `petersen(5, 2)`. */
  def petersen: G[Int, Unit] = petersen(5, 2)

  /**
    * @return
    *   The generalised Petersen graph `GP(n, k)`, whose outer vertices
    *   `0 until n` form a cycle, each joined by a spoke to the inner vertex
    *   `n + i`, and in which each inner vertex `n + i` is adjacent to
    *   `n + i + k`, going round modulo [[n]]. [[k]] must be at least `1` and
    *   less than `n / 2`. So `petersen(n, 1)` is a prism, and `petersen(10, 2)`
    *   is the dodecahedron.
    */
  def petersen(n: Int, k: Int): G[Int, Unit] =
    counted(2 * n)(_ => Shape.Petersen(n, skips(n, k)))

  /**
    * @return
    *   The grid with the given numbers of [[rows]] and [[columns]], whose
    *   vertices `(row, column)` are adjacent to their neighbours in the same
    *   row or column (pointing down and right, for directed graphs).
    */
  def grid(rows: Int, columns: Int): G[(Int, Int), Unit] =
    paired(rows, columns)(Shape.Path(_), Shape.Path(_))

  /**
    * @return
    *   The torus with the given numbers of [[rows]] and [[columns]], which is
    *   the grid with each row and each column wrapping round into a cycle.
    *   There must be at least `3` rows and `3` columns.
    */
  def torus(rows: Int, columns: Int): G[(Int, Int), Unit] =
    paired(rows, columns)(
      order => Shape.Cycle(cyclic(order)),
      order => Shape.Cycle(cyclic(order)),
    )

  /**
    * @return
    *   The prism on two cycles of [[order]] vertices, `(i, 0)` and `(i, 1)` for
    *   each `i` in `0 until order`, with a rung from each `(i, 0)` to `(i, 1)`.
    *   There must be at least `3` vertices in each cycle.
    */
  def prism(order: Int): G[(Int, Int), Unit] = paired(order, 2)(
    order => Shape.Cycle(cyclic(order)),
    Shape.Path(_),
  )

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

  /** `true` if and only if this kind of graph is directed. */
  private[kneiphof] def directed: Boolean

  /** @return The shape [[shape]] on the vertices `0 until order`. */
  protected def counted(order: Int)(shape: Int => Shape): G[Int, Unit] =
    shaped(Indexing.Counting(order), shape(order))

  /** @return The shape [[shape]] on the given [[vertices]]. */
  protected def listed[V](vertices: Seq[V])(shape: Int => Shape): G[V, Unit] =
    shaped(
      Indexing.Listing(vertices.toIndexedSeq),
      shape(vertices.size),
    )

  /**
    * @return
    *   The Cartesian product of the shapes [[first]] and [[second]] make for
    *   the numbers of [[rows]] and [[columns]], on the pairs `(row, column)`.
    */
  private def paired
    (rows: Int, columns: Int)
    (first: Int => Shape, second: Int => Shape)
    : G[(Int, Int), Unit] = shaped(
    Indexing.Pairing(rows, columns),
    Shape.Cartesian(first(rows), second(columns)),
  )

  /**
    * @return
    *   The shape of a complete graph of this kind on [[order]] vertices, in
    *   which every two vertices are adjacent.
    */
  private def allPairs(order: Int): Shape =
    if directed then Shape.CompleteDigraph(order) else Shape.Clique(order)

  /** @return The number of vertices in parts of the given [[sizes]]. */
  private def total(sizes: Seq[Int]): Int =
    require(
      sizes.forall(_ >= 0),
      s"The parts of a graph can't have the sizes $sizes.",
    )
    Math.toIntExact(sizes.map(_.toLong).sum)

  /**
    * @return
    *   [[dimension]], checking that a hypercube of it fits in an `Int`.
    */
  private def dimensions(dimension: Int): Int =
    require(
      0 <= dimension && dimension <= 30,
      s"A hypercube can't have dimension $dimension.",
    )
    dimension

  /**
    * @return
    *   [[jumps]], checking that none of them would link a vertex of a circulant
    *   graph on [[order]] vertices to itself.
    */
  private def loopless(order: Int, jumps: Seq[Int]): Seq[Int] =
    require(
      jumps.forall(jump => order > 0 && jump % order != 0),
      s"A circulant graph on $order vertices can't jump by $jumps.",
    )
    jumps

  /**
    * @return
    *   [[k]], checking that the generalised Petersen graph `GP(n, k)` is
    *   simple.
    */
  private def skips(n: Int, k: Int): Int =
    require(
      0 < k && 2 * k < n,
      s"The generalised Petersen graph needs 0 < k < n / 2, but had n = $n and k = $k.",
    )
    k

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
    counted(order)(Shape.Clique(_))

  /**
    * @return
    *   The transitive tournament on the given [[vertices]], with an edge from
    *   each vertex to every later one.
    */
  def transitiveTournament[V](vertices: V*): G[V, Unit] =
    listed(vertices)(Shape.Clique(_))
