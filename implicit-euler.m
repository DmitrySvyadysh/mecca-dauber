(* ::Package:: *)

BeginPackage["implicitEuler`"]

yImplicitEuler::usage = "return result with implicit euler "
yImplicitEulerStatistics::usage = "statistics for implicit euler"
getTime2::usage = "get time"

Begin["Private`"]

EM := IdentityMatrix[n]
yImplicitEuler[t_,\[Tau]_,t0_,u0_,A_] := If[ t <= t0
, counter = -1; u0
, prevImplicitEuler = yImplicitEuler[t-\[Tau],\[Tau],t0,u0,A];
  counter++;
  prevImplicitEuler .Inverse[( 
     EM + \[Tau] A
  )]
]

yImplicitEulerStatistics[t_, \[Tau]_, t0_, u0_, A_, exactAnswer_] := <|
	"Discrepancy" -> Abs[exactAnswer - yImplicitEuler[t,\[Tau],t0,u0,A]], 
	"Name" -> "Implicit Euler Method", 
	"Result" -> yImplicitEuler[t,\[Tau],t0,u0,A],
	"MaxStep" -> \[Tau],
	"MinStep" -> \[Tau],
	"Step counts" -> counter,
    "Time" -> getTime2[t,\[Tau],t0,u0,A]
|>

getTime2[t_, \[Tau]_, t0_, u0_, A_] := (
	startT=AbsoluteTime[];
    yImplicitEuler[t,\[Tau],t0,u0,A];
	AbsoluteTime[]-startT
);

End[]

EndPackage[]






