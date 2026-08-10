# Cómo agregar un port

Un nuevo port debe conservar una relación reconocible con el Parser V7, aunque la forma final dependa del lenguaje elegido.

## Ubicación

Cada ejercicio vive en:

```text
ports/<lenguaje>/
```

La carpeta debe incluir al menos:

- fuente principal;
- `README.md` con instrucciones de ejecución;
- indicación del estado del port.

## Estados

Usar una de estas categorías:

```text
REFERENCE      implementación de referencia
FULL           parser estructural
ADAPTATION     adaptación al paradigma del lenguaje
REDUCED        recognizer o proyección mínima
GENERATED      artefacto generado
EXPERIMENTAL   trabajo todavía no completamente validado
```

## Recursos

Cuando sea razonable, el port debe consumir:

```text
resources/grammar.json
resources/lexicon.json
resources/corpus.txt
```

Si el lenguaje no puede consumir esos recursos directamente, agregar el generador o el deck nativo que corresponda.

## Tests

Agregar al menos una forma de comparar el comportamiento con el harness común:

- stream `q/v/.` para reducciones;
- oración del corpus español para ports completos;
- salida esperada cuando sea estable.

## Criterio

No se exige que todos los ports sean idénticos.

Sí se exige que la diferencia sea explícita.
