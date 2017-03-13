(* ::Package:: *)

BeginPackage["myTests2`"]

myTest2::usage = "return with myTest"
goodStep2::usage = "fdfd"
getNextY2::usage = "rftfdd"
getMu2::usage = "gre"
getDelta2::usage = "gr"

Begin["Private`"]


myTest2[t_,\[Tau]_,t0_,u0_,A_,eps_]:= ( 
	tt0=t0;uu0=u0;\[Tau]\[Tau]0 = \[Tau];
	mu=getDelta2[A,u0,1]/getDelta2[A,u0,0];
	While[
		tt0 < t
		,Print[\[Tau]\[Tau]0];
		\[Tau]\[Tau]0 = goodStep2[eps,1.1*\[Tau]\[Tau]0,uu0,A,mu];	
		Print[\[Tau]\[Tau]0];	
		uu0 = getNextY2[uu0,\[Tau]\[Tau]0,A,mu];
		mu = getMu2[A,uu0,\[Tau]\[Tau]0,mu];
        tt0 = tt0 + \[Tau]\[Tau]0;
	];
Return[uu0])

goodStep2[eps_,\[Tau]\[Tau]0_,prevY_,A_,mu_] := ( 
	\[Tau]\[Tau]\[Tau]0= \[Tau]\[Tau]0;
	While[
	Max[Abs[getNextY2[prevY,\[Tau]\[Tau]\[Tau]0,A,mu]-getNextY2[getNextY2[prevY,\[Tau]\[Tau]\[Tau]0/2,A,mu],\[Tau]\[Tau]\[Tau]0/2,A,getMu2[A,getNextY2[prevY,\[Tau]\[Tau]\[Tau]0/2,A,mu],\[Tau]\[Tau]\[Tau]0/2,mu]]]]>eps
	, \[Tau]\[Tau]\[Tau]0 = \[Tau]\[Tau]\[Tau]0/2; 
	Print[\[Tau]\[Tau]\[Tau]0];
	];
Return[\[Tau]\[Tau]\[Tau]0])

getNextY2[prevY_,\[Tau]_,A_,mu_] := (
  Return[Exp[-mu*\[Tau]]prevY - \[Tau](A - mu IdentityMatrix[Dimensions[A]]) prevY Exp[-mu*\[Tau]]]
)

getMu2[A_,y_,\[Tau]_,prevMu_] := (
  mu = prevMu - 2*\[Tau]*(getDelta2[A, y, 2]* getDelta2[A, y, 0] - getDelta2[A, y, 1]* getDelta2[A, y, 1])/(getDelta2[A, y, 0]* getDelta2[A, y, 0]);
  Return[mu]
)

getDelta2[A_,y_,k_] := (
  mat = if[k == 0, A, MatrixPower[A, k]];
  Return[(mat.y).y]
)


End[]

EndPackage[]



