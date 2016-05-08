(* ::Package:: *)

BeginPackage["REuler`"]

yREuler::usage = "return with REuler"
yREulerStatistics::usage = "statistics for REuler"
REulerWithDeclaredR::usage = "REuler With Declared R"

Begin["Private`"]

yREuler[t_,\[Tau]_,t0_,u0_,A_,R_] := If[t <= t0
,u0
, yREuler[t-\[Tau],\[Tau],t0,u0,A,R] - \[Tau]  R A.yREuler[t-\[Tau],\[Tau],t0,u0,A,R]
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



