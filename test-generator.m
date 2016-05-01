(* ::Package:: *)

(* ::Input:: *)
(**)


BeginPackage["testGenerator`"]

generateA::usage = "generate A matrix"
generateT::usage = "generate T matrix"

Begin["Private`"]

generateT[\[Lambda]_] := (
    n:=Length[\[Lambda]];
	\:041b := DiagonalMatrix[\[Lambda]];
	B = Table[RandomInteger[{0,100}],{i,n},{j,n}];
	Do[
		lineSum := 0;
		Do[
			lineSum = lineSum + Abs[B[[i]][[j]]],{j,n}
		];
		 B[[i]][[i]] = lineSum + RandomInteger[{0,n}]
		,{i,n}
	];
	c[i_,j_] := B[[All,i]].t[j];
	v[i_] := B[[All,i]] - \!\(
\*SubsuperscriptBox[\(\[Sum]\), \(j = 1\), \(i - 1\)]\(c[i, j]*t[j]\)\);
	t[i_] := N[v[i]/Sqrt[v[i].v[i]]];
	TT := Table[t[i],{i,n}];
	T := Transpose[TT];
	T
)

generateA[\:041b_, T_]:= (
	(* (a,b) = a^Tb *)
	A := T.\:041b.Transpose[T];
    A
)

End[]

EndPackage[]









