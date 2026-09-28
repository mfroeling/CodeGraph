# CodeGraph

Static call graph for Wolfram Language paclets. CodeGraph parses `.wl` and `.m` source with
[CodeParser](https://github.com/WolframResearch/codeparser) and never evaluates it. It finds every definition, which functions call which, and how
packages depend on each other. Use it to see what a change affects, which functions matter most, and where the dependency
cycles are.

## Use in a notebook

```wl
PacletDirectoryLoad["<path>/CodeGraph/CodeGraph"];  (* or PacletInstall once released *)
Needs["CodeGraph`"]

cg = BuildCodeGraph["<paclet>/Kernel"];
CodeGraphSummary[cg]                        (* counts, cyclic package groups, private symbols without callers *)

PackageGraph[cg]                            (* layered: callers on top, foundation at the bottom, cycles in color *)
PackageGraph[cg, "Condensed"]               (* each cycle as one box, only edges not implied by a longer path *)

SymbolGraph[cg, "MyFunction"]               (* callers left, callees right *)
SymbolGraph[cg, "MyFunction", 3, "In"]      (* everything reaching MyFunction within 3 steps; "In", "Out" or "Both" *)
```

Hover a box to see its package, file and line. Click it to open the file.

## Use from the command line (for coding agents)

```shell
wolframscript -file CodeGraph/Scripts/codegraph.wls <sourceDir> <outDir>
```

This writes two tab-separated files that can be searched with grep:

- `defs.tsv`: `symbol, package, line, public, file`
- `edges.tsv`: `caller, callerPackage, line, callee, calleePackage`

`ImportCodeGraph[outDir]` loads them back for the views.

## How packages are found

- A package is the context from `BeginPackage` or `Package`, so several packages in one file and one package over
  several files are both handled. Code outside any package counts as a package named after its file.
- A symbol is public when it appears in the public section (before `Begin["`Private`"]`) or in `PackageExport`.
- A call to a symbol with a full context (`` Pkg`Private`f ``) goes to that package. Otherwise it goes to the caller's own
  package first, then to the package that exports the symbol.
- Pattern names, `Block`/`Module`/`With` variables and iterators are not counted as calls.

## Limits

- Calls built at runtime (`ToExpression`, `Symbol["..."]`) and code sent to other kernels as data are not seen.
- Callers outside the parsed folder, such as notebooks or other paclets, are not seen. So "uncalled" only means
  nothing *in this source* calls it.
- Package labels are the last part of the context (`` MyPaclet`Utils` `` becomes `Utils`) unless two contexts share it.

## License

MIT, see [LICENSE](LICENSE).

## Tests

```shell
wolframscript -file Tests/test.wls
```
