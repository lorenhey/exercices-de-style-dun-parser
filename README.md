# Exercices de style d'un parser

Una colección de ports del Parser V7: el mismo gesto formal, implementado en lenguajes convencionales, clásicos y esotéricos.

El repo contiene dos familias:

- parsers generalistas CKY incrementales con beam, cierre unario, backpointers, rasgos y recursos neutrales JSON;
- recognizers mínimos `q/v/.` para lenguajes esotéricos donde un parser completo no es práctico.

## Recursos comunes

Los ports modernos comparten:

- `resources/lexicon.json`
- `resources/grammar.json`
- `resources/corpus.txt`

Muchos ports aceptan argumentos como `--lex`, `--grammar`, `--file`, `--text`, `--beam`, `--print`, `--trees` y `--json`. Si el port busca los JSON en el directorio actual, ejecutalo desde su carpeta o pasale rutas explícitas hacia `resources/`.

## Estructura

- `ports/python/`: parser V7 canónico autocontenido.
- `ports/java/`, `ports/c/`, `ports/cpp/`, `ports/csharp/`, `ports/rust/`: ports compilables modernos.
- `ports/erlang/`, `ports/haskell/`, `ports/ruby/`, `ports/prolog/`, `ports/lisp/`, `ports/perl/`, `ports/javascript/`, `ports/php/`, `ports/r/`, `ports/wolfram/`, `ports/matlab/`: ports interpretados o de runtime.
- `ports/basic/`, `ports/pascal/`, `ports/fortran/`, `ports/cobol/`, `ports/algol60/`, `ports/algol68/`, `ports/apl/`, `ports/ada/`, `ports/logo/`: ports clásicos.
- `ports/asm_x86_64/`, `ports/vhdl/`, `ports/intercal/`, `ports/brainfuck/`, `ports/befunge/`, `ports/malbolge/`, `ports/short_code/`: ports de bajo nivel o esotéricos.

## Ejemplos rápidos

```bash
python ports/python/parser_v7.py
javac ports/java/ParserV7.java && java -cp ports/java ParserV7
make -C ports/c
make -C ports/cpp
cargo run --manifest-path ports/rust/Cargo.toml -- --text "El filósofo que escribió el tratado murió en el exilio."
node ports/javascript/run_parser7.js --beam 8
php ports/php/run_parser7.php --beam 8
ruby ports/ruby/parser_v7.rb --file resources/corpus.txt --print
```

Para lenguajes esotéricos, el modo común es un stream `q/v/.`:

```text
qv.v.
```

donde `q = QUE`, `v = VERB`, `.` = fin de oración. La salida mínima esperada es una traza por token: `state accept done`.

## Notas de extracción

El material fue extraído del share público de ChatGPT indicado en la conversación. Algunos artefactos estaban en enlaces `sandbox:/mnt/data/...` dentro del share y no estaban embebidos como código fuente; cuando ocurrió, el repo conserva el port fuente disponible o una nota en su carpeta.