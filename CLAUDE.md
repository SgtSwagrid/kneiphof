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
- Every edge is the one-edge multigraph (`M = Count`) of its kind, and `~`/`~>` chains (`Chain`, `DirectedChain`) are multigraphs too, so graph operators work on them unchanged. Edges, chains and joins forward graph primitives to a built graph via `Delegating`.
  - Edges hash via `AnyEdge.shapeHash` (ends and label), never through their own edge map, because an edge is a key in that map. Edge equality must agree with graph equality (`1 ~ 2 == Multigraph(1 ~ 2)`).
  - `+`, `|`, `&` and `-` accept any multiplicity `N` with an `Embedding[N, M]` (identity, or counts into any `M`), so `g + (1 ~ 2)` works on set graphs. Factories take `C[V, E]*`, the kind's multigraphs.
- Join operators (`g :~: h`, `v ~: g`, `g :~> v`, ...) return an `XBase.Join`: a graph that delegates to the materialised join (via `Delegating`) and remembers its operands, so that `%` can relabel only the new edges.
- Laziness: a lazy graph is a `views.LazyX` wrapping a kind-agnostic `views.Queries` (answering `hasVertex`, `multiplicity`, incidence, and `size`/`degree` when cheap) plus a by-name build, which `Delegating` uses for everything else. `AnyGraph.eager` is `false` only for these.
  - The rule: operations no bigger than their operands (`+`, `|`, `&`, `-`, filters, `induced`, `map*`, `transpose`, conversions, single updates) are eager iff every operand is (`GraphKind.lazily(operands*)`). Generating operations (shapes, joins, `join`, lexicographic sums/products, `flatMapVertices`/`flatten`) are always lazy (`GraphKind.view`), since chaining them eagerly can blow up exponentially. `g.view` makes any graph lazy. Disjoint unions are additive, so follow the rule.
  - The `Queries` live in `views` (`Viewing`, `Combining`, `Refining`, `Imaging`, `Tagging`, `Substitution`, `Joining`, `Patching`) and `shapes.Shaping`. `&`, `-` and the filters all go through the `GraphOps.refined` hook. Single updates on a lazy graph edit its `Patch` rather than nesting views.
  - Lazy graphs must override `empty` (and any other cheap query) rather than forwarding it, since kinds are built from `empty` in nearly every operator. Forwarding builds every edge (this blew the heap on a 50,000-vertex clique).
  - A lazy `size` that returns `None` builds the whole graph, so count it from the edges that change where possible (`Queries.changes`). `LazySuite.assertLazy` checks lazy queries against the graph they build.
  - Degrees go through `GraphOps.degree(vertex)` so that lazy graphs can answer them without a multiplicity lookup per incident edge, which was quadratic on large products.
- Shapes (`Shapes`, plus `DirectedShapes` for directed-only ones like `transitiveTournament`) are a `shapes.Shape` rule on indices `0 until n` plus an `Indexing` naming the vertices. Undirected rules list each edge as one arc; rules that differ by kind take `directed`. `Shape.Cartesian` with `Indexing.Pairing` gives `(row, column)` vertices (`grid`, `torus`, `prism`).
  - Shapes on one sequence of vertices have a count overload (`clique(3)`, vertices `0 until 3`) and a varargs overload (`clique("A", "B", "C")`); Scala prefers the non-varargs one for `clique(3)`. Structured shapes (`hypercube`, `circulant`, `petersen`, `grid`, ...) are count-only.
  - `complete(sizes: Int*)` and `complete(parts: Seq[V]*)` erase to the same signature, hence the `@targetName`. `complete(n)` is one part, so it has no edges; `clique(n)` is K_n.
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
