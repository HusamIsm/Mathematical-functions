(* ::Package:: *)

BeginPackage["AbstractAlgebra`"];


modMat::usage = "modMat[matrix x, matrix y,p] multiplies the matrices x,y using modp arithmetic";
cyclicSubGroup::usage = "cyclicSubGroup[generator,degree] returns the subgroup of cyclic group of degree degree, generated by generator";
subgroupEquivalence::usage = "subgroupEquivalence[n,i,r] shows which subgroups <x^b> of cyclic group of degree n are equivalent to <x^i>. r is the range of powers checked";
matOrder::usage = "matOrder[matrix a, p,n] finds the order of an element of a nxn matrix mod p";


Begin["`Private`"];


modMat[x_,y_,p_]:= MatrixForm[Inner[#1*#2&, x,y, Mod[Plus[##],p]&]]


cyclicSubGroup[gen_,degree_]:= Module[{list = {gen},curr = gen},
While[curr!= 0,
curr = Mod[curr+gen,degree];
AppendTo[list,curr]];
list]


subgroupEquivalence[n_,i_,r_]:= Select[Range[r],GCD[i,n] == GCD[#,n]&]


matOrder[a_,p_,n_]:= DeleteDuplicates@FixedPointList[modMat[First@#,a,3]&,MatrixForm[a],SameTest -> (First@# == IdentityMatrix[n]&)];


End[];


EndPackage[];
