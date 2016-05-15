(* ::Package:: *)

BeginPackage["euler`"]

yEuler::usage = "return result with euler method"
yEulerStatistics::usage = "statistics for euler"

Begin["Private`"]



yEuler[t_,\[Tau]_,t0_,u0_,A_] := If[t <= t0
, counter = -1; u0
, prevEuler = yEuler[t-\[Tau],\[Tau],t0,u0,A];
  counter++; 
  prevEuler - \[Tau] A.prevEuler
]

yEulerStatistics[t_,\[Tau]_,t0_,u0_,A_, exactAnswer_] := <|
	"Discrepancy" -> Abs[exactAnswer - yEuler[t,\[Tau],t0,u0,A]], 
	"Name" -> "Euler Method", 
	"Result" -> yEuler[t,\[Tau],t0,u0,A],
	"MaxStep" -> \[Tau],
	"MinStep" -> \[Tau],
	"Step counts" -> counter
|>


End[]

EndPackage[]



