(* ::Package:: *)

BeginPackage["REuler`"]

yMuREuler::usage = "return with yMuREuler"
yMuREulerStatistics::usage = "statistics for yMuREuler"
muREulerWithDeclaredR::usage = "yMuREuler With Declared R"

Begin["Private`"]

mu[y_,A_] := A.y.y/y.y;

yMuREuler[t_,\[Tau]_,t0_,u0_,A_,R_] := If[t <= t0
, counter = -1; u0
,
  prevEuler = yMuREuler[t-\[Tau],\[Tau],t0,u0,A,R];
  counter++;
  prevEuler - \[Tau]  R[\[Tau],mu[prevEuler,A]] A.prevEuler
]

muREulerWithDeclaredR[R_] := (
yMuREulerNoR[t_,\[Tau]_,t0_,u0_,A_] := If[t <= t0
, u0
, prevEuler = yMuREulerNoR[t-\[Tau],\[Tau],t0,u0,A];
  prevEuler - \[Tau]  R[\[Tau],mu[prevEuler,A]] A.prevEuler
];
Return[yMuREulerNoR])

yMuREulerStatistics[t_,\[Tau]_,t0_,u0_,A_,R_, exactAnswer_] := <|
	"Discrepancy" -> Abs[exactAnswer - yMuREuler[t,\[Tau],t0,u0,A,R]], 
	"Name" -> "Euler Method With " <> ToString[R], 
	"Result" -> yMuREuler[t,\[Tau],t0,u0,A,R],
	"MaxStep" -> \[Tau],
	"MinStep" -> \[Tau],
	"Step counts" -> counter
|>

End[]

EndPackage[]



