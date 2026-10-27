package com.alecdorrington
package kneiphof

import com.alecdorrington.kneiphof.shapes.{Index, Rule}

/**
  * Constructors for common shapes of unlabelled graph, of kind [[G]].
  *
  * A shape's edges are never stored: queries such as `hasEdge`, `size` and
  * vertex degrees are answered from the shape, and the edges are only computed
  * if something needs all of them, such as iterating over them.
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
      case _      => Rule.Multipartite(sizes, directed)

  /**
    * @return
    *   The hypercube of the given [[dimension]], on the vertices
    *   `0 until 2^dimension`, in which two vertices are adjacent when their
    *   binary forms differ in exactly one bit (pointing to the one with the bit
    *   set, for directed graphs).
    */
  def hypercube(dimension: Int): G[Int, Unit] =
    counted(1 << dimensions(dimension))(_ => Rule.Hypercube(dimension))

  /**
    * @return
    *   The circulant graph on the vertices `0 until order`, in which each
    *   vertex `i` is adjacent to `i + jump`, going round modulo [[order]], for
    *   each of the [[jumps]] (pointing from `i`, for directed graphs). So
    *   `circulant(n, 1)` is `cycle(n)` for `n >= 3`, and
    *   `circulant(2 * n, 1, n)` is the Möbius ladder.
    */
  def circulant(order: Int, jumps: Int*): G[Int, Unit] = counted(order): n =>
    Rule.Circulant(n, loopless(n, jumps), directed)

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
    counted(2 * n)(_ => Rule.Petersen(n, skips(n, k)))

  /**
    * @return
    *   The grid with the given numbers of [[rows]] and [[columns]], whose
    *   vertices `(row, column)` are adjacent to their neighbours in the same
    *   row or column (pointing down and right, for directed graphs).
    */
  def grid(rows: Int, columns: Int): G[(Int, Int), Unit] =
    paired(rows, columns)(Rule.Path(_), Rule.Path(_))

  /**
    * @return
    *   The torus with the given numbers of [[rows]] and [[columns]], which is
    *   the grid with each row and each column wrapping round into a cycle as
    *   for [[cycle]]. There must be at least `1` row and `1` column.
    */
  def torus(rows: Int, columns: Int): G[(Int, Int), Unit] =
    paired(rows, columns)(
      order => Rule.Cycle(cyclic(order)),
      order => Rule.Cycle(cyclic(order)),
    )

  /**
    * @return
    *   The prism on two cycles of [[order]] vertices, `(i, 0)` and `(i, 1)` for
    *   each `i` in `0 until order`, with a rung from each `(i, 0)` to `(i, 1)`.
    *   The cycles are as for [[cycle]], and need at least `1` vertex each.
    */
  def prism(order: Int): G[(Int, Int), Unit] = paired(order, 2)(
    order => Rule.Cycle(cyclic(order)),
    Rule.Path(_),
  )

  /** @return The path through the vertices `0 until order`, in order. */
  def path(order: Int): G[Int, Unit] = counted(order)(Rule.Path(_))

  /** @return The path through the given [[vertices]], in order. */
  def path[V](vertices: V*): G[V, Unit] = listed(vertices)(Rule.Path(_))

  /**
    * @return
    *   The cycle through the vertices `0 until order`, in order, of which there
    *   must be at least `1`. The cycle of `1` vertex is a loop, and the cycle
    *   of `2` vertices joins them twice, which is just once in undirected
    *   graphs whose edges form a set.
    */
  def cycle(order: Int): G[Int, Unit] =
    counted(order)(order => Rule.Cycle(cyclic(order)))

  /**
    * @return
    *   The cycle through the given [[vertices]], in order, of which there must
    *   be at least `1`, as for `cycle(order)`.
    */
  def cycle[V](vertices: V*): G[V, Unit] =
    listed(vertices)(order => Rule.Cycle(cyclic(order)))

  /**
    * @return
    *   The star with the centre `0` adjacent to each of the leaves
    *   `1 to leaves` (pointing outwards, for directed graphs).
    */
  def star(leaves: Int): G[Int, Unit] =
    counted(nonNegative(leaves) + 1)(Rule.Star(_))

  /** @return The star with [[centre]] adjacent to each of the [[leaves]]. */
  def star[V](centre: V, leaves: V*): G[V, Unit] =
    listed(centre +: leaves)(Rule.Star(_))

  /**
    * @return
    *   The wheel with the hub `0` adjacent to each of the vertices `1 to rim`,
    *   which form a cycle as for [[cycle]]. There must be at least `1` of them.
    */
  def wheel(rim: Int): G[Int, Unit] = coned(
    0,
    Index.Vertices(1 to cyclic(rim)),
    Rule.Cycle(rim),
  )

  /**
    * @return
    *   The wheel with [[hub]] adjacent to each of the [[rim]] vertices, which
    *   form a cycle in order as for [[cycle]]. There must be at least `1` of
    *   them.
    */
  def wheel[V](hub: V, rim: V*): G[V, Unit] = coned(
    hub,
    apart(hub, rim),
    Rule.Cycle(cyclic(rim.size)),
  )

  /**
    * @return
    *   The fan with the hub `0` adjacent to each of the vertices `1 to rim`,
    *   which form a path.
    */
  def fan(rim: Int): G[Int, Unit] = coned(
    0,
    Index.Vertices(1 to nonNegative(rim)),
    Rule.Path(rim),
  )

  /**
    * @return
    *   The fan with [[hub]] adjacent to each of the [[rim]] vertices, which
    *   form a path in order.
    */
  def fan[V](hub: V, rim: V*): G[V, Unit] = coned(
    hub,
    apart(hub, rim),
    Rule.Path(rim.size),
  )

  /**
    * @return
    *   The complete tree on the vertices `0 until order`, in which each vertex
    *   has up to [[branching]] children (pointing away from the root `0`, for
    *   directed graphs). The vertices are numbered in breadth-first order, so
    *   the children of `v` are `branching * v + 1` to `branching * v +
    *   branching`.
    */
  def tree(branching: Int, order: Int): G[Int, Unit] =
    counted(order)(Rule.Tree(_, branches(branching)))

  /**
    * @return
    *   The complete tree on the given [[vertices]] in breadth-first order, in
    *   which each vertex has up to [[branching]] children.
    */
  def tree[V](branching: Int, vertices: V*): G[V, Unit] =
    listed(vertices)(Rule.Tree(_, branches(branching)))

  /** @return The shape whose vertices [[index]] names, linked by [[rule]]. */
  private[kneiphof] def shaped[V](index: Index[V], rule: Rule): G[V, Unit]

  /**
    * @return
    *   The cone with apex [[hub]] over the shape [[rule]] makes on the vertices
    *   [[index]] names (pointing away from [[hub]], for directed graphs).
    */
  private[kneiphof] def coned[V](hub: V, index: Index[V], rule: Rule)
    : G[V, Unit]

  /**
    * @return
    *   The Cartesian product of the shapes [[first]] makes on the vertices
    *   [[left]] names and [[second]] makes on the vertices [[right]] names.
    */
  private[kneiphof] def product[V, W]
    (
      left: Index[V],
      first: Rule,
      right: Index[W],
      second: Rule,
    )
    : G[(V, W), Unit]

  /** `true` if and only if this kind of graph is directed. */
  private[kneiphof] def directed: Boolean

  /** @return The shape [[rule]] makes on the vertices `0 until order`. */
  protected def counted(order: Int)(rule: Int => Rule): G[Int, Unit] =
    shaped(Index.Numbers(order), rule(order))

  /** @return The shape [[rule]] makes on the given [[vertices]]. */
  protected def listed[V](vertices: Seq[V])(rule: Int => Rule): G[V, Unit] =
    shaped(
      Index.Vertices(vertices.toIndexedSeq),
      rule(vertices.size),
    )

  /**
    * @return
    *   The Cartesian product of the shapes the rules [[first]] and [[second]]
    *   make for the numbers of [[rows]] and [[columns]], on the pairs
    *   `(row, column)`.
    */
  private def paired
    (rows: Int, columns: Int)
    (first: Int => Rule, second: Int => Rule)
    : G[(Int, Int), Unit] = product(
    Index.Numbers(rows),
    first(rows),
    Index.Numbers(columns),
    second(columns),
  )

  /**
    * @return
    *   The shape of a complete graph of this kind on [[order]] vertices, in
    *   which every two vertices are adjacent.
    */
  private def allPairs(order: Int): Rule =
    if directed then Rule.CompleteDigraph(order) else Rule.Clique(order)

  /**
    * @return
    *   The vertices [[rim]], checking that they're distinct, and that [[hub]]
    *   isn't one of them.
    */
  private def apart[V](hub: V, rim: Seq[V]): Index[V] =
    require(
      !rim.contains(hub),
      s"The vertices of a shape must be distinct, but were ${ hub +: rim }.",
    )
    Index.Vertices(rim.toIndexedSeq)

  /** @return [[count]], checking that it isn't negative. */
  private def nonNegative(count: Int): Int =
    require(
      count >= 0,
      s"A shape can't have $count vertices.",
    )
    count

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

  /** @return [[order]], checking that a cycle can have that many vertices. */
  private def cyclic(order: Int): Int =
    require(
      order >= 1,
      s"A cycle needs at least 1 vertex, but had $order.",
    )
    order

  /** @return [[branching]], checking that a tree can branch that much. */
  private def branches(branching: Int): Int =
    require(
      branching >= 1,
      s"A tree can't have $branching children per vertex.",
    )
    branching
