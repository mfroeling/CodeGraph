(* two packages in one file *)
BeginPackage["Multi`A`"];
fa::usage = "fa[x]";
Begin["`Private`"];
fa[x_] := helper[x];
helper[x_] := x;
End[];
EndPackage[];

BeginPackage["Multi`B`", {"Multi`A`"}];
fb::usage = "fb[x]";
Begin["`Private`"];
fb[x_] := Block[{fa = 1}, fa + helperB[x]];
fb2[x_] := fa[x];
helperB[x_] := x;
defaultB = {1, 2};
Options[fb2] = {"Mode" -> defaultB};
End[];
EndPackage[];
