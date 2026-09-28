(* a second context ending in A`, with loader code before it *)
$otherLoaded = True;

BeginPackage["Other`A`"];
oa::usage = "oa[]";
Begin["`Private`"];
oa[] := s1[];
End[];
EndPackage[];
