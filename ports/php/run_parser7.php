<?php
declare(strict_types=1);

require __DIR__ . '/ParserV7.php';

$grammar = __DIR__ . '/resources/grammar.json';
$lexicon = __DIR__ . '/resources/lexicon.json';
$beam = 8;

$args = $argv;
for ($i = 1; $i < count($args); $i++) {
    if ($args[$i] === '--grammar' && isset($args[$i+1])) $grammar = $args[++$i];
    elseif ($args[$i] === '--lexicon' && isset($args[$i+1])) $lexicon = $args[++$i];
    elseif ($args[$i] === '--beam' && isset($args[$i+1])) $beam = (int)$args[++$i];
}

$p = new ParserV7($grammar, $lexicon, $beam);

fwrite(STDOUT, "Ready. Pegá 1 oración por línea (CTRL+D para salir).\n");

while (($line = fgets(STDIN)) !== false) {
    $line = trim($line);
    if ($line === '') continue;

    $tree = $p->parseSentence($line);
    if ($tree === '') fwrite(STDOUT, "(NO-PARSE)\n");
    else fwrite(STDOUT, $tree . "\n");
}
