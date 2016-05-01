(* ::Package:: *)

BeginPackage["analSol`"]

analyticSolution::usage = "analytic solution for our diff eq"
analyticSolutionStatistics::usage = "get statistics for analytic solution"

Begin["Private`"]

analyticSolution[t_,t0_,u0_,\:041b_,T_] := T.MatrixExp[- \:041b (t-t0)].Transpose[T].u0

analyticSolutionStatistics[ans_] := {"analyticSolution", ans}

End[]

EndPackage[]



