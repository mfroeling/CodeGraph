(* ::Package:: *)

(* ::Title:: *)
(*CodeGraph*)


(* ::Subtitle:: *)
(*Static call graph of Wolfram Language source, nothing is evaluated*)


BeginPackage["CodeGraph`", {"CodeParser`"}];


(* ::Section:: *)
(*Usage Notes*)


BuildCodeGraph::usage =
"BuildCodeGraph[dir] parses all .wl and .m files under dir without evaluating them and gives the call graph as an Association with \"Definitions\" and \"Edges\".
Packages are the contexts from BeginPackage or Package, files without one are their own package.";

ExportCodeGraph::usage =
"ExportCodeGraph[dir, cg] writes the call graph cg as defs.tsv and edges.tsv to dir.";

ImportCodeGraph::usage =
"ImportCodeGraph[dir] reads a call graph written by ExportCodeGraph.";

CodeGraphSummary::usage =
"CodeGraphSummary[cg] gives the counts, the cyclic package groups and the private symbols without callers of the call graph cg.";

CodeGraphOverview::usage =
"CodeGraphOverview[cg] gives a markdown overview of the call graph cg: package layers, cycles, the 20 most used functions and private functions without callers.
CodeGraphOverview[cg, n] lists the n most used functions. ExportCodeGraph writes it as overview.md.";

CodeGraphQuery::usage =
"CodeGraphQuery[cg, \"callers\", sym] gives the functions that call sym as tab separated text with package and definition location.
CodeGraphQuery[cg, \"callees\", sym] gives the functions sym calls.
CodeGraphQuery[cg, type, sym, n] follows n steps.";

PackageGraph::usage =
"PackageGraph[cg] shows the package dependencies layered from callers (top) to foundation (bottom), cyclic groups in color.
PackageGraph[cg, \"Condensed\"] collapses each cycle into one box and only keeps edges not implied by a longer path.
PackageGraph[cg, \"Spring\"] uses a force-directed layout.";

SymbolGraph::usage =
"SymbolGraph[cg, sym] shows the direct callers (left) and callees (right) of the symbol named sym.
SymbolGraph[cg, sym, n] follows n steps.
SymbolGraph[cg, sym, n, dir] with dir \"In\", \"Out\" or \"Both\" limits to callers or callees.";


(* ::Section:: *)
(*Functions*)


Begin["`Private`"]


(* ::Subsection:: *)
(*Parsing*)


$skip = {"Options", "SyntaxInformation", "Attributes", "SetOptions", "Format", "Protect", "Unprotect", "MessageName"};

short[n_String] := Last[StringSplit[n, "`"]];
str[LeafNode[String, s_, _]] := ToExpression[s];
str[_] := "Unknown`";

(*head symbol of a definition lhs, f[..][..] /; cond -> f*)
lhsHead[CallNode[LeafNode[Symbol, "Condition", _], {l_, _}, _]] := lhsHead[l];
lhsHead[CallNode[h_, _, _]] := lhsHead[h];
lhsHead[LeafNode[Symbol, n_, _]] := n;
lhsHead[_] := Missing[];

(*names local to a definition: patterns, scoping variables and iterators*)
local[node_] := Flatten[{
	Cases[node, CallNode[LeafNode[Symbol, "Pattern", _], {LeafNode[Symbol, s_, _], _}, _] :> s, Infinity],
	Cases[node, CallNode[LeafNode[Symbol, "Block" | "Module" | "With" | "DynamicModule", _], {CallNode[LeafNode[Symbol, "List", _], vars_, _], ___}, _] :>
		Cases[vars, LeafNode[Symbol, s_, _] | CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {LeafNode[Symbol, s_, _], _}, _] :> s], Infinity],
	Cases[node, CallNode[LeafNode[Symbol, "Table" | "Do" | "Sum" | "Product" | "ParallelTable" | "ParallelDo", _], {_, its__}, _] :>
		Cases[{its}, CallNode[LeafNode[Symbol, "List", _], {LeafNode[Symbol, s_, _], ___}, _] :> s], Infinity]
}];

(*context stack: first element is the package, deeper means private*)
push[c_String] := AppendTo[stack, If[StringStartsQ[c, "`"], If[stack === {}, "Global`", Last[stack]] <> StringDrop[c, 1], c]];
pop[] := If[stack =!= {}, stack = Most[stack]];

mention[st_] := If[Length[stack] == 1, mentions = Join[mentions, {First[stack], short[#]} & /@ Cases[st, LeafNode[Symbol, n_, _] :> n, {0, Infinity}]]];

walk[l_List] := Scan[walk, l];
walk[CallNode[LeafNode[Symbol, "CompoundExpression", _], args_, _]] := walk[args];
walk[(PackageNode | ContextNode)[{c_, ___}, body_, _]] := (push[str[c]]; walk[body]; pop[]);
walk[CallNode[LeafNode[Symbol, "BeginPackage" | "Begin", _], {c_, ___}, _]] := push[str[c]];
walk[CallNode[LeafNode[Symbol, "EndPackage" | "End", _], {}, _]] := pop[];
walk[CallNode[LeafNode[Symbol, "Package", _], {c_}, _]] := (stack = {str[c], str[c] <> "PackageScope`"});
walk[CallNode[LeafNode[Symbol, "PackageExport", _], {c_}, _]] := AppendTo[mentions, {First[stack, None], str[c]}];
walk[st : CallNode[LeafNode[Symbol, "Set" | "SetDelayed", _], {lhs_, _}, data_]] := With[{h = lhsHead[lhs]},
	mention[st];
	If[StringQ[h] && !MemberQ[$skip, short[h]], AppendTo[defs, <|
		"name" -> short[h], "context" -> First[stack, None], "private" -> Length[stack] > 1,
		"line" -> data[Source][[1, 1]], "file" -> file, "node" -> st|>]]
];
walk[st_] := mention[st];

parseFile[f_] := Block[{file = f, stack = {}, defs = {}, mentions = {}},
	walk[CodeParse[File[file]][[2]]];
	{defs, mentions}
];


(* ::Subsection:: *)
(*BuildCodeGraph*)


SyntaxInformation[BuildCodeGraph] = {"ArgumentsPattern" -> {_}};

BuildCodeGraph[dir_String] := Block[{files, parsed, defs, mentions, key, label, pack, names, packsOf, publicPack, ctxPack, resolve, edges},
	files = FileNames[{"*.wl", "*.m"}, dir, Infinity];
	parsed = parseFile /@ files;
	defs = Flatten[parsed[[All, 1]]];
	mentions = Union @@ parsed[[All, 2]];

	(*package key is the context, or the file for code outside any package; label is the last context part unless ambiguous*)
	key[d_] := Replace[d["context"], None -> FileBaseName[d["file"]]];
	label = Association[Flatten[KeyValueMap[If[Length[#2] == 1, First[#2] -> #1, Thread[#2 -> #2]] &,
		GroupBy[Select[Union[key /@ defs], StringEndsQ["`"]], short]]]];
	label = Join[AssociationMap[Identity, Complement[Union[key /@ defs], Keys[label]]], label];
	pack[d_] := label[key[d]];
	defs = Append[#, <|"pack" -> pack[#], "public" -> (!#private || MemberQ[mentions, {#context, #name}])|>] & /@ defs;

	names = Union[defs[[All, "name"]]];
	packsOf = GroupBy[defs, #name &, Union[#[[All, "pack"]]] &];
	publicPack = Association[Cases[defs, d_ /; d["public"] :> d["name"] -> d["pack"]]];
	ctxPack = Association[Cases[Union[key /@ defs], c_ /; StringEndsQ[c, "`"] :> c -> label[c]]];

	(*callee package: explicit context first, then the caller's own package, then where it is public, else first definer*)
	resolve[raw_, callee_, own_] := With[{q = ctxPack[StringDelete[StringDrop[raw, -StringLength[callee]], "Private`" ~~ EndOfString]]},
		Which[
			StringQ[q] && MemberQ[packsOf[callee], q], q,
			MemberQ[packsOf[callee], own], own,
			KeyExistsQ[publicPack, callee], publicPack[callee],
			True, First[packsOf[callee]]]];

	edges = Union @@ (Function[d, With[{loc = Append[local[d["node"]], d["name"]]},
		DeleteDuplicates[Cases[d["node"], LeafNode[Symbol, raw_, _] /; MemberQ[names, short[raw]] && !MemberQ[loc, short[raw]] :>
			{d["name"], d["pack"], d["line"], short[raw], resolve[raw, short[raw], d["pack"]]}, Infinity]]
	]] /@ defs);

	<|"Source" -> dir,
		"Definitions" -> Union[{#name, #pack, #line, #public, #file} & /@ defs],
		"Edges" -> edges|>
];


(* ::Subsection:: *)
(*Export / Import*)


$defHeader = {"symbol", "package", "line", "public", "file"};
$edgeHeader = {"caller", "callerPackage", "line", "callee", "calleePackage"};

SyntaxInformation[ExportCodeGraph] = {"ArgumentsPattern" -> {_, _}};

ExportCodeGraph[dir_String, cg_Association] := (
	Quiet[CreateDirectory[dir]];
	Export[FileNameJoin[{dir, "defs.tsv"}], Prepend[cg["Definitions"], $defHeader], "TSV"];
	Export[FileNameJoin[{dir, "edges.tsv"}], Prepend[cg["Edges"], $edgeHeader], "TSV"];
	Export[FileNameJoin[{dir, "overview.md"}], CodeGraphOverview[cg], "Text"];
	dir
);

SyntaxInformation[ImportCodeGraph] = {"ArgumentsPattern" -> {_}};

ImportCodeGraph[dir_String] := <|"Source" -> dir,
	"Definitions" -> Rest[Import[FileNameJoin[{dir, "defs.tsv"}], "TSV"]] /. {"True" -> True, "False" -> False},
	"Edges" -> Rest[Import[FileNameJoin[{dir, "edges.tsv"}], "TSV"]]|>;


(* ::Subsection:: *)
(*CodeGraphSummary*)


SyntaxInformation[CodeGraphSummary] = {"ArgumentsPattern" -> {_}};

CodeGraphSummary[cg_Association] := Block[{packEdges, public},
	packEdges = Union[Cases[cg["Edges"], {_, a_, _, _, b_} /; a =!= b :> DirectedEdge[a, b]]];
	public = Union[Cases[cg["Definitions"], {s_, _, _, True, _} :> s]];
	<|
		"Files" -> Length[Union[cg["Definitions"][[All, 5]]]],
		"Definitions" -> Length[cg["Definitions"]],
		"Symbols" -> Length[Union[cg["Definitions"][[All, 1]]]],
		"Edges" -> Length[cg["Edges"]],
		"PackageDependencies" -> Length[packEdges],
		"CyclicGroups" -> Select[ConnectedComponents[Graph[packEdges]], Length[#] > 1 &],
		"Uncalled" -> Complement[Union[cg["Definitions"][[All, 1]]], public, Union[cg["Edges"][[All, 4]]]]
	|>
];


(* ::Subsection:: *)
(*Shared graph helpers*)


(*package dependency graph, all packages as vertices*)
packageGraph[cg_] := Graph[Union[cg["Definitions"][[All, 2]]], Union[Cases[cg["Edges"], {_, a_, _, _, b_} /; a =!= b :> DirectedEdge[a, b]]]];

(*each cycle as one vertex named by its sorted members, gives the member map and the acyclic graph*)
condense[g_] := Block[{name, of},
	name[c_] := If[Length[c] == 1, First[c], StringRiffle[Sort[c], "\n"]];
	of = Association[Flatten[Thread[# -> name[#]] & /@ ConnectedComponents[g]]];
	{of, Graph[Union[Values[of]], Union[Cases[EdgeList[g], DirectedEdge[a_, b_] /; of[a] =!= of[b] :> DirectedEdge[of[a], of[b]]]]]}
];

(*symbol graph with {name, package} vertices, and the first definition {line, file} per vertex*)
symbolGraph[cg_] := Block[{ed, defs},
	ed = Union[Cases[cg["Edges"], {c_, cp_, _, d_, dp_} /; c =!= d :> DirectedEdge[{c, cp}, {d, dp}]]];
	defs = GroupBy[cg["Definitions"], {#[[1]], #[[2]]} &, #[[1, {3, 5}]] &];
	{Graph[Union[Keys[defs], Flatten[List @@@ ed, 1]], ed], defs}
];

(*steps from the focus vertices, callers negative, callees positive, nearest side wins*)
steps[g_, focus_, n_, dir_] := Block[{in, out},
	in = If[dir === "Out", <||>, Association[Function[v, v -> -Min[GraphDistance[g, v, #] & /@ focus]] /@ Complement[VertexInComponent[g, focus, n], focus]]];
	out = If[dir === "In", <||>, Association[Function[v, v -> Min[GraphDistance[g, #, v] & /@ focus]] /@ Complement[VertexOutComponent[g, focus, n], focus]]];
	Join[Merge[{in, out}, First[SortBy[#, {Abs, Minus}]] &], AssociationThread[focus, 0]]
];

location[defs_, v_] := With[{d = Lookup[defs, Key[v], {"?", ""}]}, FileNameTake[d[[2]]] <> ":" <> ToString[d[[1]]]];

(*distinct callers and caller packages per function, most used first*)
usage[cg_] := ReverseSortBy[KeyValueMap[Join[#1, {Length[Union[#2[[All, {1, 2}]]]], Length[Union[#2[[All, 2]]]]}] &,
	GroupBy[cg["Edges"], #[[{4, 5}]] &]], #[[3 ;;]] &];


(* ::Subsection:: *)
(*CodeGraphOverview*)


SyntaxInformation[CodeGraphOverview] = {"ArgumentsPattern" -> {_, _.}};

CodeGraphOverview[cg_Association, top_Integer: 20] := Block[{sum, of, dag, h, layers},
	sum = CodeGraphSummary[cg];
	{of, dag} = condense[packageGraph[cg]];

	(*layer = longest path down to a package that depends on nothing*)
	h = <||>;
	Scan[(h[#] = Max[0, 1 + Lookup[h, VertexOutComponent[dag, #, {1}], -1]]) &, Reverse[TopologicalSort[dag]]];
	layers = KeySortBy[GroupBy[Keys[h], h], Minus];

	StringRiffle[Flatten[{
		"# Code graph overview",
		"",
		"Source: `" <> cg["Source"] <> "`, built " <> DateString["ISODate"] <> ". " <> ToString[sum["Files"]] <> " files, " <>
			ToString[sum["Definitions"]] <> " definitions, " <> ToString[sum["Edges"]] <> " call edges.",
		"Static analysis: runtime-built calls and callers outside the source are not seen. Rebuild after adding, removing or moving definitions.",
		"Query with `codegraph.wls <graphDir> callers|callees <symbol> [depth]`, or grep edges.tsv (caller, callerPackage, line, callee, calleePackage).",
		"",
		"## Package layers",
		"",
		"Packages only call packages in lower layers or in their own cycle (in brackets). Layer 0 depends on nothing.",
		"",
		KeyValueMap["- " <> ToString[#1] <> ": " <> StringRiffle[Sort[If[StringContainsQ[#, "\n"], "[" <> StringReplace[#, "\n" -> ", "] <> "]", #] & /@ #2], ", "] &, layers],
		"",
		"## Dependency cycles",
		"",
		If[sum["CyclicGroups"] === {}, "None.", "- " <> StringRiffle[Sort[#], ", "] & /@ sum["CyclicGroups"]],
		"",
		"## Most used functions",
		"",
		"| Function | Package | Callers | From packages |",
		"| --- | --- | --- | --- |",
		"| " <> StringRiffle[ToString /@ #, " | "] <> " |" & /@ Take[usage[cg], UpTo[top]],
		"",
		"## Private functions without callers in this source",
		"",
		If[sum["Uncalled"] === {}, "None.", StringRiffle[sum["Uncalled"], ", "]]
	}], "\n"]
];


(* ::Subsection:: *)
(*CodeGraphQuery*)


SyntaxInformation[CodeGraphQuery] = {"ArgumentsPattern" -> {_, _, _, _.}};

CodeGraphQuery[cg_Association, type : "callers" | "callees", sym_String, n_Integer: 1] := Block[{g, defs, focus, cols, sign, at, link},
	{g, defs} = symbolGraph[cg];
	focus = Select[VertexList[g], #[[1]] === sym &];
	If[focus === {}, Return["unknown symbol " <> sym]];

	sign = If[type === "callers", -1, 1];
	cols = steps[g, focus, n, If[sign < 0, "In", "Out"]];
	at[k_] := SortBy[Select[Keys[cols], cols[#] == sign k &], Reverse];
	(*for each function the ones in the previous step it calls or is called by*)
	link[v_, k_] := StringRiffle[Select[at[k - 1], EdgeQ[g, If[sign < 0, DirectedEdge[v, #], DirectedEdge[#, v]]] &][[All, 1]], ", "];

	StringRiffle[Flatten[{
		type <> " of " <> sym <> " (" <> StringRiffle[focus[[All, 2]], ", "] <> "), " <> ToString[n] <> " step(s), " <> ToString[Length[cols] - Length[focus]] <> " functions",
		"step\tfunction\tpackage\tdefined\t" <> If[sign < 0, "calls", "called by"],
		Table[StringRiffle[{k, #[[1]], #[[2]], location[defs, #], link[#, k]}, "\t"] & /@ at[k], {k, n}]
	}], "\n"]
];


(* ::Subsection:: *)
(*PackageGraph*)


(*light colors spread evenly over the packages in the view*)
colors[packs_] := AssociationThread[packs, Hue[#, 0.35, 1] & /@ (Range[0, Length[packs] - 1]/Length[packs])];

SyntaxInformation[PackageGraph] = {"ArgumentsPattern" -> {_, _.}};

PackageGraph[cg_Association, type_String: "Layered"] := Block[{ed, w, g, groups, col, of, cond},
	ed = Counts[Cases[cg["Edges"], {_, a_, _, _, b_} /; a =!= b :> DirectedEdge[a, b]]];
	g = packageGraph[cg];
	w = Max[ed, 1];
	groups = Select[ConnectedComponents[g], Length[#] > 1 &];
	col = AssociationThread[Range[Length[groups]], PadRight[{Red, Orange, Purple, Darker[Green]}, Length[groups], Brown]];

	If[type === "Condensed",
		(*each cycle becomes one box, then only edges not implied by a longer path*)
		{of, cond} = condense[g];
		cond = TransitiveReductionGraph[cond];
		Graph[cond,
			VertexShape -> None, VertexSize -> 0, ImageSize -> 1100,
			VertexLabels -> (# -> Placed[Framed[#, Background -> Lookup[Association[MapIndexed[of[First[#1]] -> Lighter[col[First[#2]], 0.6] &, groups]], #, LightGray],
				RoundingRadius -> 4, FrameStyle -> None, BaseStyle -> Black], Center] & /@ VertexList[cond]),
			EdgeStyle -> Directive[GrayLevel[0.5], Arrowheads[0.012]],
			GraphLayout -> {"LayeredDigraphEmbedding", "Orientation" -> Top}
		],

		Graph[g,
			VertexLabels -> "Name", VertexSize -> 0.4, ImageSize -> 1100,
			VertexStyle -> Join[Thread[VertexList[g] -> LightGray], Flatten[MapIndexed[Thread[#1 -> col[First[#2]]] &, groups]]],
			EdgeStyle -> KeyValueMap[#1 -> Directive[GrayLevel[0.5, 0.6], Arrowheads[0.015], AbsoluteThickness[0.5 + 4 #2/w]] &, ed],
			EdgeLabels -> KeyValueMap[#1 -> Placed[ToString[#2] <> " calls", Tooltip] &, ed],
			GraphLayout -> If[type === "Spring", "SpringElectricalEmbedding", {"LayeredDigraphEmbedding", "Orientation" -> Top}]
		]
	]
];


(* ::Subsection:: *)
(*SymbolGraph*)


SyntaxInformation[SymbolGraph] = {"ArgumentsPattern" -> {_, _, _., _.}};

SymbolGraph[cg_Association, sym_String, n_Integer: 1, dir_String: "Both"] := Block[{
		defs, g, focus, cols, pos, edges, col, box, dy = 24/260.
	},
	{g, defs} = symbolGraph[cg];
	focus = Select[VertexList[g], #[[1]] === sym &];
	If[focus === {}, Return[Missing["UnknownSymbol", sym]]];
	cols = steps[g, focus, n, dir];

	(*each column a vertical list sorted by package, only edges between neighboring columns*)
	pos = Association[KeyValueMap[Function[{c, vs}, MapIndexed[#1 -> {c, -dy (First[#2] - (Length[vs] + 1)/2)} &, SortBy[vs, Reverse]]], GroupBy[Keys[cols], cols]]];
	edges = Select[EdgeList[Subgraph[g, Keys[cols]]], cols[#[[2]]] == cols[#[[1]]] + 1 &];

	col = colors[Union[Keys[cols][[All, 2]]]];
	box[v_] := Button[Tooltip[
		Framed[Style[v[[1]], 11, Black, If[v[[1]] === sym, Bold, Plain]], Background -> col[v[[2]]], FrameStyle -> If[v[[1]] === sym, Directive[Black, Thick], None],
			RoundingRadius -> 3, FrameMargins -> {{4, 4}, {1, 1}}],
		v[[2]] <> "  " <> location[defs, v]], SystemOpen[Lookup[defs, Key[v], {"", ""}][[2]]], Appearance -> None];

	Legended[Graph[Keys[pos], edges,
		VertexCoordinates -> Normal[pos],
		VertexShapeFunction -> (# -> With[{b = box[#]}, Inset[b, #1] &] & /@ Keys[pos]),
		EdgeStyle -> GrayLevel[0.5, 0.6], EdgeShapeFunction -> "Line", AspectRatio -> Automatic, PlotRangePadding -> {{0.5, 0.5}, {dy, dy}},
		ImageSize -> 260 (Max[cols] - Min[cols] + 1)
	], SwatchLegend[Values[col], Keys[col]]]
];


(* ::Section:: *)
(*End Package*)


End[]

EndPackage[]
