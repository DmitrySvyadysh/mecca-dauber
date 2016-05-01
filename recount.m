(* ::Package:: *)

BeginPackage["recount`"]

solveWithRecount::usage = "solve for f with recount"
stepValStatistics::usage = "statistics for f with recount"

Begin["Private`"]
	
solveWithRecount[f_,eps_,t_,\[Tau]0_,t0_,u0_,A_]:= ( 
	tt0=t0;uu0=u0;\[Tau]\[Tau]0 = \[Tau]0;
	While[
		tt0 < t
		,\[Tau]\[Tau]0= goodStep[f,eps,1.1\[Tau]\[Tau]0,tt0,uu0,A]; 
		uu0= f[tt0+\[Tau]\[Tau]0,\[Tau]\[Tau]0,tt0,uu0,A];
        tt0 = tt0 + \[Tau]\[Tau]0;
	];
Return[uu0])

(* yEuler[t_,\[Tau]_,t0_,u0_,A_] *)
goodStep[f_,eps_,\[Tau]\[Tau]0_,tt0_,uu0_,A_] := ( 
	\[Tau]\[Tau]\[Tau]0= \[Tau]\[Tau]0;
	While[
	diff[f,\[Tau]\[Tau]\[Tau]0,tt0,uu0,A]>eps
	, \[Tau]\[Tau]\[Tau]0 = \[Tau]\[Tau]\[Tau]0/2
	];
Return[\[Tau]\[Tau]\[Tau]0])

diff[f_,\[Tau]_,t0_,u0_,A_] := Max[Abs[f[\[Tau] + t0,\[Tau],t0,u0,A]-f[\[Tau]+ t0,\[Tau]/2,t0,u0,A]]];

dimon[k_,\[Tau]_,t0_,u0_,A_] := ( 
		If[ k ==0
		,u0
		, dimon[k-1,\[Tau],t0,u0,A] - \[Tau] A.dimon[k-1,\[Tau],t0,u0,A]
	]
)

stepValStatistics[name_, ans_]  := {name, ans}

End[]

EndPackage[]


