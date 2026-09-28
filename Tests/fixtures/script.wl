(* no package, and a second context ending in A` *)
s1[] := s2[];
s2[] := 1;

BeginPackage["Other`A`"];
oa::usage = "oa[]";
Begin["`Private`"];
oa[] := s1[];
End[];
EndPackage[];
