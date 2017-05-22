(* ::Package:: *)

BeginPackage["viaAccessoryProblem`"]

viaAccessoryProblem::usage = "get viaAccessoryProblem"
viaAccessoryProblemStatistics::usage = "get viaAccessoryProblemStatistics"

Begin["Private`"]


viaAccessoryProblem[t_,\[Tau]_,t0_,u0_,A_,eps_]:= ( 
	tt0=t0;uu0=u0;\[Tau]\[Tau]0 = \[Tau];
    steps = {};
	counterRecount = -1;
	recountNumberRecount = 0;
	mu=getDelta2[A,u0,1]/getDelta2[A,u0,0];
	While[
		tt0 < t
		,
		\[Tau]\[Tau]0 = goodStep2[eps,1.1*\[Tau]\[Tau]0,uu0,A,mu];
		uu0 = getNextY2[uu0,\[Tau]\[Tau]0,A,mu];
		mu = getMu2[A,uu0,\[Tau]\[Tau]0,mu];
        tt0 = tt0 + \[Tau]\[Tau]0;
        steps = Append[steps, \[Tau]\[Tau]0];
        recountNumberRecount += recountNumber;
		counterRecount++;
	];
Return[uu0])

goodStep2[eps_,\[Tau]\[Tau]0_,prevY_,A_,mu_] := ( 
	\[Tau]\[Tau]\[Tau]0= \[Tau]\[Tau]0;
    recountNumber = 0;
	While[
    first = getNextY2[prevY,\[Tau]\[Tau]\[Tau]0,A,mu];
    halfSecond = getNextY2[prevY,\[Tau]\[Tau]\[Tau]0/2,A,mu];
    second = getNextY2[halfSecond,\[Tau]\[Tau]\[Tau]0/2,A,getMu2[A,halfSecond,\[Tau]\[Tau]\[Tau]0/2,mu]];

	Max[Abs[first - second]] > eps
	, recountNumber++;
      \[Tau]\[Tau]\[Tau]0 = \[Tau]\[Tau]\[Tau]0/2
	];
Return[\[Tau]\[Tau]\[Tau]0])

getNextY2[prevY_,\[Tau]_,A_,mu_] := (
  result = (Exp[-mu*\[Tau]]prevY - \[Tau] (A - mu IdentityMatrix[Dimensions[A]]).prevY Exp[-mu*\[Tau]]);
  Return[result];
)

getMu2[A_,y_,\[Tau]_,prevMu_] := (
  mu = prevMu - 2*\[Tau]*(getDelta2[A, y, 2]*(getDelta2[A, y, 0]) - getDelta2[A, y, 1]* getDelta2[A, y, 1])/(getDelta2[A, y, 0]*getDelta2[A, y, 0]);
  Return[mu]
)

getDelta2[A_,y_,k_] := (
  if[k == 0, matr = IdentityMatrix[Dimensions[A]], matr = MatrixPower[A,k]];
  result = (matr.y).y;
  minResult = 0.0000000000000000000000000003;
  if[result < minResult, result = minResult, result = result];
  Return[result]
)

viaAccessoryProblemStatistics[t_,\[Tau]_,t0_,u0_,A_,eps_, exactAnswer_]  := ( 
startT=AbsoluteTime[];
	currentAnswer = viaAccessoryProblem[t,\[Tau],t0,u0,A,eps];
Return[
	<|
		"Discrepancy" -> Abs[exactAnswer - currentAnswer], 
		"Name" -> "viaAccessoryProblem", 
		"Result" -> uu0,
		"MaxStep" -> Max[steps],
		"MinStep" -> Min[steps],
		"Step counts" -> counterRecount,
		"Recounts number" -> recountNumberRecount,
        "Time" -> AbsoluteTime[]-startT
	|>
]);


End[]

EndPackage[]



