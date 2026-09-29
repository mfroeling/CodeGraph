(* one package over two files *)
BeginPackage["Split`"];
sa::usage = "sa[]";
Begin["`Private`"];
sa[] := sb[];
registered[x_] := x;
ImportExport`RegisterImport["Fx", {"Data" -> registered}];
unused[] := 1;
SetAttributes[unused, HoldFirst];
deadTop[] := deadMid[];
deadMid[] := 1;
End[];
EndPackage[];
