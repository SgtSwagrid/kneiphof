package com.alecdorrington
package kneiphof

// Type aliases and edge syntax, available with `import com.alecdorrington.kneiphof.*`.

/** A directed hyperedge whose tail and head share the vertex type [[V]]. */
type Hyperarc[+V, +E] = DirectedHyperedge[V, V, E]

/** A directed edge whose source and target share the vertex type [[V]]. */
type Arc[+V, +E] = DirectedEdge[V, V, E]

/** A hypergraph whose edges form a set. */
type Hypergraph[+V, +E] = HypergraphBase[V, E, Boolean]

/** A hypergraph which may contain several copies of the same edge. */
type Multihypergraph[+V, +E] = HypergraphBase[V, E, Count]

/** A graph whose edges form a set. */
type Graph[+V, +E] = GraphBase[V, E, Boolean]

/** A graph which may contain several copies of the same edge. */
type Multigraph[+V, +E] = GraphBase[V, E, Count]

/** A directed hypergraph whose edges form a set. */
type DirectedHypergraph[+V, +E] = DirectedHypergraphBase[V, E, Boolean]

/** A directed hypergraph which may contain several copies of the same edge. */
type DirectedMultihypergraph[+V, +E] = DirectedHypergraphBase[V, E, Count]

/** A directed graph whose edges form a set. */
type DirectedGraph[+V, +E] = DirectedGraphBase[V, E, Boolean]

/** A directed graph which may contain several copies of the same edge. */
type DirectedMultigraph[+V, +E] = DirectedGraphBase[V, E, Count]

extension [V : NotHandle](first: V)

  /** Alias of [[Edge.apply]]. */
  infix def ~ [W >: V, A <: W : NotHandle](second: A): Edge[W, Unit] =
    Edge(first, second)

  /** Alias of [[DirectedEdge.apply]]. */
  infix def ~> [T : NotHandle](target: T): DirectedEdge[V, T, Unit] =
    DirectedEdge(first, target)
