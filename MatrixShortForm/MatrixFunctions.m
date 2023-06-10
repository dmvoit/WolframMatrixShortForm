(* ::Package:: *)
(* :Title: MatrixFunctions *)
(* :Mathematica Version: 13.2 *)


BeginPackage["MatrixFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"];

MatrixConcat::matform = "can't concatenate matrices";
MatrixConcat[mat_] := Module[{maxdim, rmax, cmax, joinCols,result},
  (* fid max dimensions of all sub matrices  *)
  maxdim = Replace[mat, {x_List :> Dimensions@x, x_ -> {1, 1}}, {2}];
  (* separate max rows and cols *)
  {rmax, cmax} = maxdim[[All, All, #]]& /@ {1, 2};

  (*check if row sizes mach*)
  rMatch = MatchQ[Length /@ Union /@ rmax, {1..}];

  (*check if col sizes mach*)
  cMatch = MatchQ[{1..}]@MapThread[Length@*Union@*List, cmax];

(*  {{rmax,rMatch},{cmax,cMatch}}*)
  If[Not@And[rMatch, cMatch],
    Message[MatrixConcat::matform, "dimensions dont match"];
    Return[$Failed]];

  joinCols[col__]:=Join[col,2];
(*  final join *)
  result=List@*joinCols@@#&/@mat;
  Join@(Join@@(Flatten[result,1]))
]

End[]; (* `Private` *)

EndPackage[]