(* ::Package:: *)

BeginPackage["AbstractAlgebra`"];


modMat::usage = "modMat[matrix x, matrix y,p] multiplies the matrices x,y using modp arithmetic";


Begin["`Private`"];


modMat[x_,y_,p_]:= MatrixForm[Inner[#1*#2&, x,y, Mod[Plus[##],p]&]]


End[];


EndPackage[];
