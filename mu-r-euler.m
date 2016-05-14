(* ::Package:: *)

BeginPackage["REuler`"]

yMuREuler::usage = "return with REuler"
yMuREulerStatistics::usage = "statistics for REuler"
muREulerWithDeclaredR::usage = "REuler With Declared R"

Begin["Private`"]

mu[y_,A_] := A.y.y/y.y;

yMuREuler[t_,\[Tau]_,t0_,u0_,A_,R_] := If[t <= t0
,u0
,
  prevEuler = yMuREuler[t-\[Tau],\[Tau],t0,u0,A,R];
  prevEuler - \[Tau]  R[\[Tau],mu[prevEuler,A]] A.prevEuler
]

REulerWithDeclaredR[R_] := (
yREulerNoR[t_,\[Tau]_,t0_,u0_,A_] := If[t <= t0
,u0
, yREulerNoR[t-\[Tau],\[Tau],t0,u0,A] - \[Tau]  R A.yREulerNoR[t-\[Tau],\[Tau],t0,u0,A]
];
Return[yREulerNoR])

yREulerStatistics[t_,\[Tau]_,t0_,u0_,A_,R_, exactAnswer_] := <|
	"Discrepancy" -> Abs[exactAnswer - yREuler[t,\[Tau],t0,u0,A,R]], 
	"Name" -> "Euler Method With R", 
	"Result" -> yREuler[t,\[Tau],t0,u0,A,R],
	"MaxStep" -> \[Tau],
	"MinStep" -> \[Tau]
|>

End[]

EndPackage[]



