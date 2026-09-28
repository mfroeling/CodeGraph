#!/usr/bin/env wolframscript
(* Build the call graph of a source folder and write defs.tsv and edges.tsv.
   Usage: wolframscript -file codegraph.wls sourceDir outDir *)

PacletDirectoryLoad[ParentDirectory[DirectoryName[$InputFileName]]];
Needs["CodeGraph`"];

args = Rest[$ScriptCommandLine];
If[Length[args] < 2, Print["usage: wolframscript -file codegraph.wls sourceDir outDir"]; Exit[1]];

cg = BuildCodeGraph[ExpandFileName[args[[1]]]];
ExportCodeGraph[ExpandFileName[args[[2]]], cg];
KeyValueMap[Print[#1, ": ", #2] &, CodeGraphSummary[cg]];
