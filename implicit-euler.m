(* ::Package:: *)

BeginPackage["implicitEuler`"]

yImplicitEuler::usage = "return result with implicit euler "
yImplicitEulerStatistics::usage = "statistics for implicit euler"
Begin["Private`"]

EM := IdentityMatrix[n]
yImplicitEuler[t_,\[Tau]_,t0_,u0_,A_] := If[ t <= t0
,u0
, yImplicitEuler[t-\[Tau],\[Tau],t0,u0,A] .Inverse[( 
EM + \[Tau] A
)]
]

yImplicitEulerStatistics[ans_] := {"yImplicitEuler", ans}

End[]

EndPackage[]






