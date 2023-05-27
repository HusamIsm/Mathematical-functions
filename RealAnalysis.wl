(* ::Package:: *)

BeginPackage["RealAnalysis`"];


cantorSet::usage = "given an interval {a,b},divisor div, and the degree, return the cantor set of degree n, using 1/div instead of the usual third";


Begin["`Private`"];


divtest[{a_,b_},div_Integer /; Mod[div,2]==1]:= Module[{m= (b-a)/div,l = {},x=a,y,i=0},y=a+m; While[i<div, AppendTo[l,{x,y}];x+= 2*m;y+= 2*m;
i+=2];l];


divAll[o_List,div_Integer]:=Module[{l={}},l= Flatten[divtest[#,div]& /@ o,1]];


cantorSet[{a_,b_},div_Integer /;Mod[div,2]==1, degree_Integer]:=Module[{l = {{a,b}}},For[i=0,i<degree,i++,l = divAll[l,div ];];l ];


End[];


EndPackage[];
