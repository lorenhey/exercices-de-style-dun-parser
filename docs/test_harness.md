# Test harness

El harness común registra hasta qué punto los ports conservan comportamientos compartidos.

No pretende certificar equivalencia perfecta entre implementaciones.

## Estructura

```text
tests/
├── qv_streams/
├── spanish_sentences/
├── spanish_streams/
├── golden/
└── run_tests.py
```

## Niveles

### Traza

Secuencias reducidas `q/v/.` producen filas:

```text
state accept done
```

### Reconocimiento

El test registra aceptación o rechazo de una secuencia.

### Estructura

En ports completos se puede comparar:

- árbol;
- spans;
- best-1;
- n-best cuando el port lo soporte.

## Uso

```bash
python tests/run_tests.py
```

El script base valida fixtures y recursos compartidos. Los runners específicos de cada lenguaje pueden agregarse progresivamente.
