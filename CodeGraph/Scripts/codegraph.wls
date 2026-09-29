#!/usr/bin/env wolframscript
(* Build:  wolframscript -file codegraph.wls <sourceDir> <outDir> [exclude ...]
           exclude: file base names or patterns to skip, like Legacy or "*Test*"
           writes defs.tsv, edges.tsv and overview.md to outDir and prints a summary
   Query:  wolframscript -file codegraph.wls <graphDir> callers|callees <symbol> [depth]
           reads a graph written by build *)

PacletDirectoryLoad[ParentDirectory[DirectoryName[$InputFileName]]];
Needs["CodeGraph`"];

args = Rest[$ScriptCommandLine];

Which[
	Length[args] >= 3 && MemberQ[{"callers", "callees"}, args[[2]]],
	Print[CodeGraphQuery[ImportCodeGraph[ExpandFileName[args[[1]]]], args[[2]], args[[3]], If[Length[args] > 3, FromDigits[args[[4]]], 1]]],

	Length[args] >= 2,
	cg = BuildCodeGraph[ExpandFileName[args[[1]]], "Exclude" -> Drop[args, 2]];
	ExportCodeGraph[ExpandFileName[args[[2]]], cg];
	KeyValueMap[Print[#1, ": ", #2] &, CodeGraphSummary[cg]],

	True,
	Print["build: wolframscript -file codegraph.wls <sourceDir> <outDir> [exclude ...]\nquery: wolframscript -file codegraph.wls <graphDir> callers|callees <symbol> [depth]"];
	Exit[1]
];
