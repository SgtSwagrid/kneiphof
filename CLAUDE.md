# CLAUDE.md

This file provides guidance to [Claude Code](https://claude.com/product/claude-code) when working with code in this repository.
It is not intended for human eyes.

### Maintenance

You (robot or human) have standing permission to update this file without asking.
Add important patterns, gotchas, or context that would help future sessions.
Keep it concise and actionable.

## Project overview

This is a Scala library of graphs and graph algorithms.

## Instructions

### Compilation and Diagnostics

- When the user asks for help with a compilation or type error, start by running `sbt compile` to see the error for yourself.
  If there are many errors, making it unclear which one the user is referring to, ask them to clarify, and then focus only on that issue.
- IntelliJ MCP integration is active. When a request seems to implicitly refer to something the user is looking at, always check
  `mcp__ide__getDiagnostics` first to see which file(s) are open and get associated diagnostics (errors, warnings, and info hints with line numbers).

#### Testing

- After making code changes, always run `sbt compile` to verify that issues are fixed and no new ones are introduced.
- Repeatedly retry upon failure until the build succeeds. If you are unsure how to fix an issue, ask for help or refer to existing code for examples.
- Before trying to fix an error, make sure you first understand it fully.
- You should never report that a feature is complete without testing it first.

#### sbt on this machine

- `sbt --client` (and plain `sbt`, which defaults to the client) fails to start a server here, because the Java path contains a space. Use `sbt --server --batch ...`.
- Free commit memory is often low; if the JVM dies with "insufficient memory", add `-mem 900 -J-Xms128m -J-XX:+UseSerialGC -J-XX:ReservedCodeCacheSize=96m`, and delete any `hs_err_pid*.log` / `replay_pid*.log` it leaves in the repo root.

### Code Style

- You must read the [Code Style Guidelines](docs/STYLE_GUIDE.md).

### Architecture

- Four graph kinds: `HypergraphBase` ⊃ `GraphBase`, and `DirectedHypergraphBase` ⊃ `DirectedGraphBase`, each `[+V, +E, M]` where `M` is the edge multiplicity (`Boolean` for edge sets, `Count` for multigraphs). The user-facing names (`Graph`, `Multigraph`, ...) are aliases in `syntax.scala`, plus factory objects.
- `GraphOps[V, E, M, X, G]` holds the kind-preserving operations. Operations that add edges (`+`, `|`, `flatMapVertices`, ...) are declared per kind with exact types and implemented once in `GraphKind`. They can't live in `GraphOps`, because a `Graph` widened to a `Hypergraph` must not store hyperedges.
- Never use `@uncheckedVariance` or casts. Covariant types expose `ValueSet`/`ValueMap` (keyed by `Any`, element stored as a value), never `Set[V]`/`Map[V, _]`. Handles (`g.Vertex`, `g.Edge`) are traits instantiated by each kind.
- Inside the graph traits, the handle type member `Edge` shadows the top-level `Edge` class: write `kneiphof.Edge` there.
- Every edge is the one-edge multigraph (`M = Count`) of its kind, and `~`/`~>` chains (`Chain`, `DirectedChain`) are multigraphs too, so graph operators work on them unchanged. Edges, chains and joins forward graph primitives to a built graph via `Delegating`; edges get their own edge map, incidence and hash from `SingleEdge`.
  - Edges hash via `AnyEdge.shapeHash` (ends and label), never through their own edge map, because an edge is a key in that map. Edge equality must agree with graph equality (`1 ~ 2 == Multigraph(1 ~ 2)`).
  - `+`, `|`, `&` and `-` accept any multiplicity `N` with an `Embedding[N, M]` (identity, or counts into any `M`), so `g + (1 ~ 2)` works on set graphs. Factories take `C[V, E]*`, the kind's multigraphs.
- Join operators (`g :~: h`, `v ~: g`, `g :~> v`, ...) return an `XBase.Join`, whose `Joined` is a lazy graph answering from `views.Joining` and remembering its operands, so that `%` can relabel only the new edges. A join is the flatten of the edge `g ~ h`, so overlapping operands are paired too: shared vertices gain loops, and that's the user's choice, documented on each `join`.
- Every `%` takes `[F : NotGraph]`, so labels can't be edges or graphs. `%` binds more loosely than `~`, so without this `1 ~ 2 % "a" ~ 3` would silently mean `(1 ~ 2) % ("a" ~ 3)`. Keep it invariant: a contravariant `NotGraph` would let inference pick `F = Any` and skip the check. `ChainSuite` checks it with `compileErrors`.
- Laziness: a lazy graph is a `views.LazyX` (all four share `LazyOps`) holding a kind-agnostic `views.Queries` (answering `hasVertex`, `multiplicity`, `find`, `incidentEdges`, and `size`/`degree` when cheap), a `() => G` build (`origin`), and a `Patch`. `Delegating` forwards everything else to the build with the patch applied. `AnyGraph.eager` is `false` only for these, and `GraphOps.built` exposes the build to tests.
  - The rule: operations no bigger than their operands (`+`, `|`, `&`, `-`, filters, `induced`, `map*`, `transpose`, conversions, single updates) are eager iff every operand is (`GraphKind.lazily(operands*)`). Generating operations (shapes, joins, `join`, lexicographic sums/products, `flatMapVertices`/`flatten`) are always lazy (`GraphKind.view`), since chaining them eagerly can blow up exponentially. `g.view` makes any graph lazy. Disjoint unions are additive, so follow the rule.
  - The `Queries` live in `views` (`Viewing`, `Combining`, `Refining` and `Imaging` in `Combinators.scala`; `Tagging`, `Substitution`, `Joining`, `Patching`) and `shapes.Shaping` (one class for both, via `Shaping.undirected` and `Shaping.directed`). Every graph gives its own queries with `GraphOps.viewing` (optionally through an `Embedding`), and `incidentEdges` with multiplicities. `&`, lazy `-` and the filters go through the `GraphOps.refined` hook. Single updates, and `+`, `|` or `-` with an eager operand, extend a lazy graph's `Patch` rather than nesting views, so long chains stay shallow and build in one pass.
  - Lazy graphs must override `empty` (and any other cheap query) rather than forwarding it, since kinds are built from `empty` in nearly every operator. Forwarding builds every edge (this blew the heap on a 50,000-vertex clique).
  - A lazy `size` that returns `None` builds the whole graph, so count it from the edges that change where possible (`Queries.changes`), or the smaller operand's edges, without gathering them into a list. `LazyAssertions.assertLazy` (and `assertLazyDirected`) check lazy queries against a graph made independently from what they build.
  - Degrees go through `GraphOps.degree(vertex)`, which lazy graphs answer from `Queries.degree` or `Queries.incidentEdges`, never with a lookup per incident edge (quadratic on large graphs). Sizes and degrees use exact arithmetic (`Queries.total`).
  - Lazy queries find vertices and edges by `==`, as hash maps do: `Indexing.number` matches `1L` to `1`, and `Queries.Pair`/`Arrow` match any edge class. Every edge has at least one end.
- Shapes (`Shapes`, plus `DirectedShapes` for directed-only ones like `transitiveTournament`) are a `shapes.Shape` rule on indices `0 until n` plus an `Indexing` naming the vertices. Undirected rules list each edge as one arc; rules that differ by kind take `directed`. `Shape.Cartesian` with `Indexing.Pairing` gives `(row, column)` vertices (`grid`, `torus`, `prism`).
  - Shapes on one sequence of vertices have a count overload (`path(3)`, vertices `0 until 3`) and a varargs overload (`path("A", "B", "C")`); Scala prefers the non-varargs one for `path(3)`. Structured shapes (`hypercube`, `circulant`, `petersen`, `grid`, ...) are count-only.
  - Cycles (and wheels, tori and prisms) allow 1 and 2 vertices, the user's choice: a loop and a double edge, which collapses to one edge in undirected edge sets (`Graph.cycle(2) == Graph.path(2)`). So arcs can repeat an edge, also via the two loops of `torus(1, 1)`. `Shape.arcs` counts them for multiplicities, and `Shape.repeats(directed)` (with `loops` for `Cartesian`) says how many collapse in edge sets, so `Shaping` sizes stay exact; its degrees fall back to incident edges there.
  - Complete graphs split by name instead (the user's choice): `complete(sizes*)` takes only sizes, following the notation (`complete(n)` is K_n, `complete(3, 4)` is K_{3,4}), and `clique(vertices*)` takes only vertices, so `clique(3)` is the single vertex `3`.
- The default representation is `incidence.IncidenceOps` (persistent hash tries). Other representations plug in by implementing the abstract members of a kind and passing their empty graph to `XBase.Kind(empty)`.

### Pull Requests

When asked to publish the code changes, your task is to open one or more pull requests (PRs) to merge the changes into `main` on GitHub:

- Use `git` to check what has changed as compared to the `main` branch on `origin`.
- If the changes are thematically linked, they can be published as a single PR.
- Otherwise, you'll need to divide the changes into multiple PRs using your own judgement.
- Each PR should have a singular focus, shouldn't break anything, and should be able to be merged independently.
- Ensure that all code is staged, committed and pushed. Ensure no new files are left uncommitted, and no debug code is left in the codebase.
- When creating a PR, ensure that the title and description are clear, informative, and comprehensive.
- All feature/bugfix/etc branch names should be formatted as "feature_<short description>" or "fix_<short description>" or similar.
- All PR titles should be formatted as "[<scope>] <Short summary>", e.g. "[renderer] Fixed colour inversion bug."
- You have GitHub MCP integration that can be used to do the above.
