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
- Every graph is a kind wrapped around one kind-agnostic `incidence.IncidenceStructure[+V, M, +X]` (`GraphOps.structure`), which answers every primitive (`vertexValues`, `hasVertex`, `multiplicity`, `incidentEdges`, `find`, `edges`, `size`, `degree`, `loops`). `GraphOps.edgeValues` is a `lazy val` over `structure.edges`. Each kind's `Kind.wrap(structure)` makes a private `Wrapper` (anonymous classes there crashed at runtime with `NoSuchFieldError $outer`).
  - Everything is lazy by default: every operation wraps a new structure over its operands' structures, and nothing builds automatically. `materialise` (the user's choice: explicit, not automatic) stores a graph as an `incidence.Explicit` (hash tries); factories build `Explicit` directly. `structure.stored` is `true` only for `Explicit` (and a `Support` of one).
  - One structure per operation, with graph-theory names, one per file in `incidence`: `Explicit`, `Patch`, `Sum`/`Union` (sharing `Overlay`), `Intersection`, `Difference`, `Subgraph`, `Image`, `Copy` (injective tagging, for `⊔` and lexicographic products), `Substitution`, `Join`, `CartesianProduct`, `Support` (multiplicities read through an `Embedding`). `+` on edge sets builds a `Union`.
  - Single updates go through `structure.patched` (a `Patch` patches itself, so chains stay shallow). `+`, `|` and `-` with a stored operand fold its edges into a patch instead of nesting a structure. `toHypergraph`/`toDirectedHypergraph` wrap the same structure, since structures are covariant in `X`.
  - `GraphOps` can't build graphs of its own kind (a structure parameter would put `X` in a contravariant position), so `&`, `-` and the filters go through the per-kind hooks `intersected`, `subtracted` and `subgraph`, and each kind implements `empty`, `addVertex`, `removeVertex`, `withMultiplicity`, `materialise` and `map` with one-liners into its `Kind`.
  - Chains of one operation merge into one structure through each structure's companion `of`, so loops of `|`, `+`, `&`, `-`, maps or filters stay shallow (the user's idea). Covariant structures make the type tests there checkable, so no casts are needed. `Sum` and `Union` (sharing the `Overlay` trait) and `Intersection` take the operands of either side. `Difference` gathers further subtrahends, since `(g - a) - b = g - (a + b)`. `Subgraph` gathers predicates and checks them in a loop. `Image` composes its maps through `TailCalls`, because nested closures overflowed the stack at 10,000 maps. Overlays and differences index their operands by vertex (`IncidenceStructure.owners`), so a query only asks the operands containing its vertex. `Embedding.embedded` skips `Support` for the identity embedding, so right-hand operands merge too. Joins, products and disjoint unions don't merge: their vertex types nest, or `labelled` must still tell their new edges apart.
  - The default `edges` lists each edge at whichever end comes first in vertex order (by position, not by how the edge is written, since equal edges can list their ends differently). A `size` or `degree` with no cheap formula streams `edges`/`incidentEdges`; count from the edges that change where possible (`IncidenceStructure.changes`), and never look each incident edge up again (quadratic on large graphs). Sizes and degrees use exact arithmetic (`IncidenceStructure.total`).
  - Structures find vertices and edges by `==`, as hash maps do: `Index.number` matches `1L` to `1`, and `IncidenceStructure.Pair`/`Arrow` match any edge class. Every edge has at least one end.
  - `LazyAssertions.assertLazy` (and `assertLazyDirected`) check that a graph isn't stored and that its queries agree with its materialisation.
- Every edge is the one-edge multigraph (`M = Count`) of its kind, and `~`/`~>` chains (`Chain`, `DirectedChain`) are multigraphs too, so graph operators work on them unchanged. Edges get an `Explicit` structure, edge map, incidence and hash from `SingleEdge`; chains build theirs from their steps.
  - Edges hash via `AnyEdge.shapeHash` (ends and label), never through their own edge map, because an edge is a key in that map. Edge equality must agree with graph equality (`1 ~ 2 == Multigraph(1 ~ 2)`).
  - `+`, `|`, `&` and `-` accept any multiplicity `N` with an `Embedding[N, M]` (identity, or counts into any `M`), so `g + (1 ~ 2)` works on set graphs. Factories take `C[V, E]*`, the kind's multigraphs.
- Every symbolic operator is an alias of a named method, documented exactly "Alias of [[name]]." (`+` `sum`, `|` `union`, `&` `intersection`, `-` `difference`, `⊔` `disjointUnion`, `:~:` `join`, `:~`/`~:` `cone`, `:~>` `coneTo`, `~>:` `coneFrom`, `□` `cartesianProduct`, `%` `labelled`, chain `~`/`~>` `to`).
- Join operators return an `XBase.Join`, whose private `Joined` wraps an `incidence.Join` and remembers its operands, so that `labelled`/`%` can relabel only the new edges. A join is the flatten of the edge `g ~ h`, so overlapping operands are paired too: shared vertices gain loops, and that's the user's choice, documented on each `join`.
- Every `%` and `labelled` takes `[F : NotGraph]`, so labels can't be edges or graphs. `%` binds more loosely than `~`, so without this `1 ~ 2 % "a" ~ 3` would silently mean `(1 ~ 2) % ("a" ~ 3)`. Keep it invariant: a contravariant `NotGraph` would let inference pick `F = Any` and skip the check. `ChainSuite` checks it with `compileErrors`.
- Shapes (`Shapes`, plus `DirectedShapes` for directed-only ones like `transitiveTournament`) are a `shapes.Shape` structure: a `shapes.Rule` on indices `0 until n` plus an `Index` naming the vertices. Undirected rules list each edge as one arc; rules that differ by kind take `directed`. Shapes that are other operations are built from them, through the hooks `Shapes.coned` and `Shapes.product`: wheels and fans are cones over a cycle or path, and grids, tori and prisms are Cartesian products of paths and cycles on `(row, column)` vertices.
  - Shapes on one sequence of vertices have a count overload (`path(3)`, vertices `0 until 3`) and a varargs overload (`path("A", "B", "C")`); Scala prefers the non-varargs one for `path(3)`. Structured shapes (`hypercube`, `circulant`, `petersen`, `grid`, ...) are count-only.
  - Cycles (and wheels, tori and prisms) allow 1 and 2 vertices, the user's choice: a loop and a double edge, which collapses to one edge in undirected edge sets (`Graph.cycle(2) == Graph.path(2)`). So arcs can repeat an edge. `Rule.arcs` counts them for multiplicities, and `Rule.repeats(directed)` says how many collapse in edge sets, so `Shape` sizes stay exact; its degrees fall back to incident edges there. Tori built from such cycles get the same from `CartesianProduct`, whose loops coincide (`torus(1, 1)` has two loops in a multigraph and one in an edge set).
  - Complete graphs split by name instead (the user's choice): `complete(sizes*)` takes only sizes, following the notation (`complete(n)` is K_n, `complete(3, 4)` is K_{3,4}), and `clique(vertices*)` takes only vertices, so `clique(3)` is the single vertex `3`.
- Naming: one top-level type per file, named after it. `-Ops` only for traits that give a class its operations (`GraphOps`, `VertexOps`, `EdgeOps`). The user keeps `DirectedEdge`, `Arc` and `Hyperarc` as they are for now.
- Another representation is another `IncidenceStructure`.

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
