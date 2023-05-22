(* ::Package:: *)

BeginPackage["VectorCalc`"];


jacobian::usage = "jacobian[function,variables] gives you the jacobian matrix";


detJacobian::usage = "Detjacobian[function, variables] gives you the determinant of the jacobian";


Begin["`Private`"];


jacobian[f_,v__]:= D[f[v],{{v}}]


detJacobian[f_,v__]:= 
Module[{divMat}, divMat = D[f[v],{{v}}]; Det[divMat]]


End[];


EndPackage[];
