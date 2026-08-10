(* run_incremental.wls
   Ejecutar en terminal:
   wolframscript -file run_incremental.wls
*)

Get["ParserV7.wl"];

p = ParserV7`CreateParser[
  FileNameJoin[{NotebookDirectory[], "resources", "grammar.json"}],
  FileNameJoin[{NotebookDirectory[], "resources", "lexicon.json"}],
  8
];

Print["Modo incremental real (Step[token])."];
Print["Pegá una oración. Muestro el mejor árbol tras cada token. Línea vacía para salir.\n"];

While[True,
  line = InputString["> "];
  If[line === "" || line === $Canceled, Break[]];

  ParserV7`ResetIncremental[p];
  parts = StringSplit[line, Whitespace];

  Do[
    tree = ParserV7`Step[p, parts[[i]]];
    Print["[", i, "] +", parts[[i]], " => ", If[tree === "", "(NO-PARSE)", tree]],
  {i, Length[parts]}];

  final = ParserV7`BestTree[p];
  Print["\nFinal => ", If[final === "", "(NO-PARSE)", final], "\n"];
];
