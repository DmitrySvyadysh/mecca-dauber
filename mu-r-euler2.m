(* ::Package:: *)

BeginPackage["REuler2`"]

yMuREuler2::usage = "return with yMuREuler2"
yMuREulerStatistics2::usage = "statistics for yMuREuler2"
muREulerWithDeclaredR2::usage = "yMuREuler2 With Declared R"
getTime4::usage = "get time"

Begin["Private`"]

d[k_,y_] := A^k .y. y;


mu2[y_,A_] := 2/((d[3,y] d[0,y]-d[2,y] d[1,y] )/(d[2,y] d[0,y] - d[1,y] d[1,y]));

yMuREuler2[t_,\[Tau]_,t0_,u0_,A_,R_] := If[t <= t0
, counter = -1; u0
,
  prevEuler = yMuREuler2[t-\[Tau],\[Tau],t0,u0,A,R];
  counter++;
  prevEuler - \[Tau]  R[\[Tau],mu2[prevEuler,A]] A.prevEuler
]

muREulerWithDeclaredR2[R_] := (
yMuREulerNoR2[t_,\[Tau]_,t0_,u0_,A_] := If[t <= t0
, u0
, prevEuler = yMuREulerNoR2[t-\[Tau],\[Tau],t0,u0,A];
  prevEuler - \[Tau]  R[\[Tau],mu2[prevEuler,A]] A.prevEuler
];
Return[yMuREulerNoR2])

yMuREulerStatistics2[t_,\[Tau]_,t0_,u0_,A_,R_, exactAnswer_] := <|
	"Discrepancy" -> Abs[exactAnswer - yMuREuler2[t,\[Tau],t0,u0,A,R]], 
	"Name" -> "Euler Method With " <> ToString[R], 
	"Result" -> yMuREuler2[t,\[Tau],t0,u0,A,R],
	"MaxStep" -> \[Tau],
	"MinStep" -> \[Tau],
	"Step counts" -> counter,
	"Time" -> getTime4[t,\[Tau],t0,u0,A,R]
|>

getTime4[t_,\[Tau]_,t0_,u0_,A_,R_] := (
	startT=AbsoluteTime[];
    yMuREuler2[t,\[Tau],t0,u0,A,R];
	AbsoluteTime[]-startT
);

End[]

EndPackage[]









