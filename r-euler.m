(* ::Package:: *)

BeginPackage["REuler`"]

yREuler::usage = "return with REuler"
yREulerStatistics::usage = "statistics for REuler"

Begin["Private`"]

yREuler[t_,\[Tau]_,t0_,u0_,A_,R_] := If[t <= t0
,u0
, yREuler[t-\[Tau],\[Tau],t0,u0,A,R] - \[Tau]  R A.yREuler[t-\[Tau],\[Tau],t0,u0,A,R]
]


yREulerStatistics[ans_] := {"yREuler", ans}

End[]

EndPackage[]



