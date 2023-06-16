(* ::Package:: *)

BeginPackage["AbstractAlgebra`"];


cyclicSubGroup::usage = "cyclicSubGroup[generator,degree] returns the subgroup of cyclic group of degree degree, generated by generator";
modMat::usage = "modMat[matrix x, matrix y,p] multiplies the matrices x,y using modp arithmetic";


Begin["`Private`"];


modMat[x_,y_,p_]:= MatrixForm[Inner[#1*#2&, x,y, Mod[Plus[##],p]&]]


cyclicSubGroup[gen_,degree_]:= Module[{list = {gen},curr = gen},
While[curr!= 0,
curr = Mod[curr+gen,degree];
AppendTo[list,curr]];
list]


End[];


EndPackage[];
