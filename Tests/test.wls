#!/usr/bin/env wolframscript
(* Usage: wolframscript -file Tests/test.wls *)

PacletDirectoryLoad[FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "CodeGraph"}]];
Needs["CodeGraph`"];

cg = BuildCodeGraph[FileNameJoin[{DirectoryName[$InputFileName], "fixtures"}]];
defs = Association[{#[[1]], #[[2]]} -> #[[4]] & /@ cg["Definitions"]];
edges = cg["Edges"][[All, {1, 2, 4, 5}]];

checks = {
	(*two packages in one file, ambiguous label A becomes the full context*)
	"multi file split" -> KeyExistsQ[defs, {"fa", "Multi`A`"}] && KeyExistsQ[defs, {"fb", "B"}],
	"ambiguous label" -> KeyExistsQ[defs, {"oa", "Other`A`"}],
	(*one package over two files*)
	"split package" -> MemberQ[edges, {"sa", "Split", "sb", "Split"}],
	(*explicit context resolves to that package*)
	"qualified call" -> MemberQ[edges, {"sb", "Split", "helper", "Multi`A`"}],
	(*public from the public section, private from Begin*)
	"public" -> defs[{"fa", "Multi`A`"}] && !defs[{"helper", "Multi`A`"}],
	(*new-style Package and PackageExport*)
	"package export" -> defs[{"na", "New"}] && !defs[{"nb", "New"}] && MemberQ[edges, {"na", "New", "nb", "New"}],
	(*code outside any package is its own public package named after the file*)
	"script" -> defs[{"s1", "script"}] && MemberQ[edges, {"oa", "Other`A`", "s1", "script"}],
	(*Block local shadowing a function is not a call*)
	"locals" -> !MemberQ[edges, {"fb", "B", "fa", _}] && MemberQ[edges, {"fb2", "B", "fa", "Multi`A`"}],
	(*two steps up from helper: sb calls it directly, sa via sb*)
	"query" -> With[{q = CodeGraphQuery[cg, "callers", "helper", 2]},
		StringContainsQ[q, "1\tsb\tSplit\tsplit2.wl:4\thelper"] && StringContainsQ[q, "2\tsa\tSplit\tsplit1.wl:5\tsb"]],
	(*Split and B call Multi`A`, so they sit one layer above it*)
	"overview" -> StringContainsQ[CodeGraphOverview[cg], "- 1: B, Other`A`, Split"]
};

Print[If[#2, "ok    ", "FAIL  "], #1] & @@@ checks;
If[!And @@ checks[[All, 2]], Print[cg["Definitions"][[All, ;; 4]]]; Print[edges]; Exit[1]];
