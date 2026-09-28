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
(*PackageGraph*)


(*light colors spread evenly over the packages in the view*)
colors[packs_] := AssociationThread[packs, Hue[#, 0.35, 1] & /@ (Range[0, Length[packs] - 1]/Length[packs])];

SyntaxInformation[PackageGraph] = {"ArgumentsPattern" -> {_, _.}};

PackageGraph[cg_Association, type_String: "Layered"] := Block[{ed, w, g, groups, col, of, cond, name},
	ed = Counts[Cases[cg["Edges"], {_, a_, _, _, b_} /; a =!= b :> DirectedEdge[a, b]]];
	g = Graph[Union[cg["Definitions"][[All, 2]]], Keys[ed]];
	w = Max[ed, 1];
	groups = Select[ConnectedComponents[g], Length[#] > 1 &];
	col = AssociationThread[Range[Length[groups]], PadRight[{Red, Orange, Purple, Darker[Green]}, Length[groups], Brown]];

	If[type === "Condensed",
		(*each cycle becomes one box, then only edges not implied by a longer path*)
		name[c_] := If[Length[c] == 1, First[c], StringRiffle[Sort[c], "\n"]];
		of = Association[Flatten[Thread[# -> name[#]] & /@ ConnectedComponents[g]]];
		cond = TransitiveReductionGraph[Graph[Union[Values[of]],
			Union[Cases[EdgeList[g], DirectedEdge[a_, b_] /; of[a] =!= of[b] :> DirectedEdge[of[a], of[b]]]]]];
		Graph[cond,
			VertexShape -> None, VertexSize -> 0, ImageSize -> 1100,
			VertexLabels -> (# -> Placed[Framed[#, Background -> Lookup[Association[MapIndexed[name[#1] -> Lighter[col[First[#2]], 0.6] &, groups]], #, LightGray],
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
		ed, defs, g, focus, in, out, cols, pos, edges, col, def, box, dy = 24/260.
	},
	ed = Union[Cases[cg["Edges"], {c_, cp_, _, d_, dp_} /; c =!= d :> DirectedEdge[{c, cp}, {d, dp}]]];
	defs = GroupBy[cg["Definitions"], {#[[1]], #[[2]]} &, #[[1, {3, 5}]] &];
	g = Graph[Union[Keys[defs], Flatten[List @@@ ed, 1]], ed];
	focus = Select[VertexList[g], #[[1]] === sym &];
	If[focus === {}, Return[Missing["UnknownSymbol", sym]]];

	(*column = steps from the symbol, callers negative, callees positive, nearest side wins*)
	in = If[dir === "Out", <||>, Association[Function[v, v -> -Min[GraphDistance[g, v, #] & /@ focus]] /@ Complement[VertexInComponent[g, focus, n], focus]]];
	out = If[dir === "In", <||>, Association[Function[v, v -> Min[GraphDistance[g, #, v] & /@ focus]] /@ Complement[VertexOutComponent[g, focus, n], focus]]];
	cols = Join[Merge[{in, out}, First[SortBy[#, {Abs, Minus}]] &], AssociationThread[focus, 0]];

	(*each column a vertical list sorted by package, only edges between neighboring columns*)
	pos = Association[KeyValueMap[Function[{c, vs}, MapIndexed[#1 -> {c, -dy (First[#2] - (Length[vs] + 1)/2)} &, SortBy[vs, Reverse]]], GroupBy[Keys[cols], cols]]];
	edges = Select[EdgeList[Subgraph[g, Keys[cols]]], cols[#[[2]]] == cols[#[[1]]] + 1 &];

	col = colors[Union[Keys[cols][[All, 2]]]];
	def[v_] := Lookup[defs, Key[v], {"?", ""}];
	box[v_] := Button[Tooltip[
		Framed[Style[v[[1]], 11, Black, If[v[[1]] === sym, Bold, Plain]], Background -> col[v[[2]]], FrameStyle -> If[v[[1]] === sym, Directive[Black, Thick], None],
			RoundingRadius -> 3, FrameMargins -> {{4, 4}, {1, 1}}],
		v[[2]] <> "  " <> FileNameTake[def[v][[2]]] <> ":" <> ToString[def[v][[1]]]], SystemOpen[def[v][[2]]], Appearance -> None];

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
