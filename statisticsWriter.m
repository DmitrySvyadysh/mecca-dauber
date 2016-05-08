(* ::Package:: *)

BeginPackage["statisticsWriter`"]

createXLS::usage = "create xls with methods information"

Begin["Private`"]

createXLS[statisticsTable_] := (
date = DateList[];
filename = ToString[
	date[[3]]]<>"."<>ToString[date[[2]]]<>"."<>ToString[date[[1]]]<>"_"
	<>ToString[date[[4]]]<>"-"<>ToString[date[[5]]] <> "-"<>ToString[Round[date[[6]]]
];
dir = NotebookDirectory[]<>"statistics\\" <> filename;
CreateDirectory[dir];
Export[dir<>"\\data.xls", statisticsTable, "XLS"];
)

End[]

EndPackage[]
