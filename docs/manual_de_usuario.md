# Manual de usuario

## Contrato comun

Los ports completos implementan un parser CKY incremental con beam, cierre unario y backpointers. Los ports minimalistas implementan el reconocedor incremental `q/v/.`.

### Stream minimalista

- `q`: relativizador QUE
- `v`: verbo
- `.`: fin de oración

Salida minimalista por token:

```text
state accept done
```

## Recursos

Los JSON comunes viven en `resources/`.

```text
resources/grammar.json
resources/lexicon.json
resources/corpus.txt
```

## Ejecucion por port

### Python

```bash
python ports/python/parser_v7.py
```

### Java

```bash
javac ports/java/ParserV7.java
java -cp ports/java ParserV7
```

### C

```bash
make -C ports/c
ports/c/parser_v7 --lex resources/lexicon.json --grammar resources/grammar.json --file resources/corpus.txt --print
```

### C++

```bash
make -C ports/cpp
ports/cpp/parser_v7 --lex resources/lexicon.json --grammar resources/grammar.json --file resources/corpus.txt --print
```

### Rust

```bash
cargo run --manifest-path ports/rust/Cargo.toml -- --lex resources/lexicon.json --grammar resources/grammar.json --file resources/corpus.txt --print
```

### JavaScript / PHP / Perl / R / Wolfram / MATLAB

Cada carpeta incluye archivo principal y runner cuando aplica. Ejecutar desde la carpeta del port o pasar rutas absolutas/relativas hacia `resources/`.

### Esolangs

Usar `ports/brainfuck/parser_v7.bf`, `ports/befunge/parser_v7.b98` o `ports/intercal/parser_v7.i` con entrada `q/v/.` o la codificacion indicada en el README del port.
