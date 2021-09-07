"use strict";

const readline = require("readline");
const path = require("path");
const { ParserV7 } = require("./parserV7");

const grammarPath = process.argv.includes("--grammar")
  ? process.argv[process.argv.indexOf("--grammar") + 1]
  : path.join(__dirname, "resources", "grammar.json");

const lexiconPath = process.argv.includes("--lexicon")
  ? process.argv[process.argv.indexOf("--lexicon") + 1]
  : path.join(__dirname, "resources", "lexicon.json");

const beam = process.argv.includes("--beam")
  ? Number(process.argv[process.argv.indexOf("--beam") + 1])
  : 8;

const p = new ParserV7({ grammarPath, lexiconPath, beam });

console.log("Ready. Pegá 1 oración por línea (CTRL+D para salir).");

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: true,
});

rl.on("line", (line) => {
  const s = line.trim();
  if (!s) return;
  const tree = p.parseSentence(s);
  console.log(tree ? tree : "(NO-PARSE)");
});

rl.on("close", () => process.exit(0));
