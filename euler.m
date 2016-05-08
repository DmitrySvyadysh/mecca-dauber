(* ::Package:: *)

BeginPackage["euler`"]

yEuler::usage = "return result with euler method"
yEulerStatistics::usage = "statistics for euler"

Begin["Private`"]

yEuler[t_,\[Tau]_,t0_,u0_,A_] := If[t <= t0
,u0
, yEuler[t-\[Tau],\[Tau],t0,u0,A] - \[Tau] A.yEuler[t-\[Tau],\[Tau],t0,u0,A]
]

yEulerStatistics[t_,\[Tau]_,t0_,u0_,A_, exactAnswer_] := <|
	"Discrepancy" -> Abs[exactAnswer - yEuler[t,\[Tau],t0,u0,A]], 
	"Name" -> "Euler Method", 
	"Result" -> yEuler[t,\[Tau],t0,u0,A],
	"MaxStep" -> \[Tau],
	"MinStep" -> \[Tau]
|>


End[]

EndPackage[]



