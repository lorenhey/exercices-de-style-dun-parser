# Exercices de style (d’un parser)

> «Un mismo parser, reescrito una y otra vez bajo las restricciones de lenguajes de programación distintos.»

---

## De qué se trata

Este repositorio nace de una idea deliberadamente poco práctica: tomar un mismo parser sintáctico incremental y reescribirlo en la mayor variedad posible de lenguajes de programación que se me ocurran.

Python, Java, C, C++, C#, Rust, Erlang, Haskell, Prolog, Lisp, BASIC, Pascal, Fortran, COBOL, ALGOL, APL, Assembly, Brainfuck, Befunge, INTERCAL, Shakespeare Programming Language y Malbolge terminaron en la bolsa (y en la mala, saludos a Ulises y a Diomedes que andan por ahí).

Este proyecto no tiene un propósito utilitario.

Es un ejercicio de estilo, el parser es solamente el motivo que permanece mientras cambia la escritura.

El título remite, naturalmente, a *Exercices de style* de Raymond Queneau.

Publicado en 1947, el libro toma un episodio insignificante y lo vuelve a contar noventa y nueve veces. Cambian el registro, la estructura, la voz, el procedimiento, el género, la retórica. La anécdota permanece.

Eso es lo que me interesa: no reproducir literalmente el procedimiento de Queneau, sino trasladar su principio a otro tipo de escritura.

En este repositorio:

```text
INVARIANTE      -> un parser sintáctico incremental
VARIACIÓN       -> el lenguaje en el que está escrito
```

La situación narrativa de Queneau es reemplazada por un algoritmo. Los estilos son reemplazados por lenguajes de programación. Pero la pregunta permanece casi intacta: ¿cuántas formas distintas puede adoptar una misma cosa sin dejar de ser reconocible?

Me interesa especialmente la idea de *contrainte*: la restricción como principio generador de forma.

En literatura, una restricción puede ser un lipograma, una estructura matemática, una combinatoria, una regla sintáctica o cualquier otro dispositivo capaz de reducir deliberadamente el espacio de lo posible. No hace falta explicarlo mucho, leé *Exercices de style* y va a quedar claro.

Lo bueno para un vago sin talento es que acá no necesito inventar las restricciones, los lenguajes ya vienen con ellas.

Cada port es, en ese sentido, una nueva *contrainte*.

---

## Programar como escribir

En programación se habla constantemente de "estilo", pero casi siempre en un sentido menor:

- nombres de variables;
- sangría;
- ubicación de llaves;
- convenciones;
- organización de archivos.

Acá eso me importa poco; el uso de la palabra es en un sentido más fuerte. Un lenguaje de programación no cambia solamente la sintaxis superficial de un algoritmo, cambia lo que resulta natural decir, cambia lo que resulta incómodo, cambia lo que puede darse por supuesto, cambia qué estructuras aparecen primero en la cabeza, cambia qué partes del problema quedan expuestas.

El mismo parser escrito en Prolog no es simplemente el parser de Python con otra puntuación.

El mismo parser escrito en Assembly tampoco.

Hay algo del lenguaje de destino que termina interviniendo necesariamente en la forma final.

Eso es, justamente, lo que quiero conservar.

---

## ¿Por qué un parser?

Podría haber elegido casi cualquier algoritmo, pero elegí un parser porque el objeto tiene una cualidad que me resulta especialmente apropiada para el experimento: trabaja sobre lenguaje mediante lenguaje.

Un lenguaje de programación intenta analizar una lengua natural. La forma que analiza y la forma desde la cual se analiza pertenecen a órdenes distintos, pero ambas son sistemas formales.

Eso produce un juego de espejos bastante fértil. El objeto de partida es un parser sintáctico incremental generalista. Recibe una oración progresivamente y actualiza su análisis a medida que llegan nuevas palabras.

Por ejemplo:

```text
El filósofo que escribió el tratado murió en el exilio.
```

El parser no necesita esperar necesariamente al punto final para comenzar a construir estructura.

Puede recibir:

```text
El
El filósofo
El filósofo que
El filósofo que escribió
...
```

y modificar sus hipótesis en cada paso.

De ahí la operación conceptual que recorre buena parte del proyecto:

```text
step(token)
```

Una palabra llega.

El estado cambia.

Otra palabra llega.

El estado vuelve a cambiar.

El análisis completo es el resultado de esa sucesión.

---

## El Parser V7

La implementación de referencia del proyecto es el Parser V7, es el primero publicado tras 6 versiones menores que fui haciendo cada muerte de obispo, medio adrede, medio de vago.

En las versiones más completas utiliza una arquitectura de parsing incremental basada en chart, con elementos como:

- reglas léxicas;
- reglas unarias y binarias;
- spans;
- cierre unario;
- múltiples hipótesis;
- beam por celda;
- scores;
- backpointers;
- reconstrucción de árboles;
- actualización incremental.

El funcionamiento general puede pensarse así:

```text
token₁ -> estado₁
token₂ -> estado₂
token₃ -> estado₃
...
tokenₙ -> análisis
```

Pero este repositorio no pretende convertir esa arquitectura en un estándar. El Parser V7 *es* el tema. Los ports son las variaciones.

---

## El mismo parser, hasta donde sea posible

Una parte importante del proyecto apareció cuando la traducción empezó a llegar a lenguajes cada vez más restrictivos.

Con Python, Java, C o Rust, todavía resulta razonable intentar conservar casi toda la estructura original.

Con Brainfuck la afirmación empieza a volverse ridícula.

Con Malbolge, es como con Elvira: es otra cosa.

Y ahí aparece una pregunta que me interesa más que una fidelidad artificial: ¿qué es lo mínimo que debe sobrevivir para que todavía pueda reconocer el ejercicio original?

Por eso las implementaciones no se presentan todas como equivalentes.

Hay distintas clases de variación.

### I. Parser estructural

Conserva la mayor parte del mecanismo original:

```text
chart
+ reglas
+ cierre unario
+ beam
+ scores
+ backpointers
+ incrementalidad
```

Es el caso de los lenguajes donde la traducción completa sigue siendo razonable.

### II. Adaptación

Conserva el problema y el comportamiento general, pero deja que el paradigma del lenguaje modifique la solución.

Esto es especialmente importante en lenguajes funcionales, lógicos, matriciales o declarativos.

No quiero escribir Python utilizando sintaxis de Prolog, no es interesante. Quiero ver qué hace Prolog con el problema.

### III. Reducción

Para lenguajes muy restrictivos uso una proyección mínima del parser.

El alfabeto común es:

```text
q = relativo "que"
v = núcleo verbal relevante
. = fin de oración
```

Una oración puede quedar reducida, por ejemplo, a:

```text
qvv.
```

Y el programa produce una traza:

```text
state accept done
```

Eso ya no es el Parser V7 completo. Es una reducción deliberada y no pretendo ocultarlo, obviamente: la deformación es parte del ejercicio.

### IV. Artefacto generado

En otros casos el código final se obtiene mediante una transformación intermedia.

Por ejemplo:

```text
modelo
  ↓
representación intermedia
  ↓
compilación
  ↓
target
```

El código máquina pertenece naturalmente a esta categoría.

Malbolge también puede requerirla.

En esos casos, el proceso de transformación forma parte de la pieza.

---

## Lingüística computacional no utilitarista

El proyecto está claramente situado en la lingüística computacional: El objeto es un parser, hay gramática, hay lexicón, hay tokenización, hay reglas sintácticas, hay árboles, hay ambigüedad, hay incrementalidad, hay representación formal.

Todo eso es real, sí, pero no utilizo la lingüística computacional como justificación instrumental, el conocimiento técnico es el material del ejercicio, no su excusa. Si me pongo en místico delirante, diría que es un manifiesto antitecnócrata, pero con una definición definición de tecnocracia media tangencial: la dictadura del utilitarismo.

---

## Recursos comunes

En los ports que lo permiten, intento separar el modelo lingüístico de su implementación.

La gramática y el lexicón viven en:

```text
resources/
├── grammar.json
└── lexicon.json
```

La intención es que diferentes lenguajes puedan partir del mismo material.

Cuando un lenguaje no puede consumir esos archivos directamente, por eso genero una representación apropiada:

```text
grammar.json + lexicon.json
              ↓
          generador
              ↓
          deck nativo
```

Por ejemplo:

```text
JSON -> Ada
JSON -> APL
JSON -> Standard ML
JSON -> VHDL
JSON -> NASM
```

Eso permite mantener relativamente estable lo que quiero variar menos:

```text
gramática
lexicón
corpus
```

mientras dejo variar lo que constituye el centro del proyecto:

```text
la escritura del programa
```

---

## Corpus

Usé un corpus pequeño de oraciones en español para recorrer fenómenos sintácticos diferentes.

Entre ellas:

```text
El filósofo que escribió el tratado murió en el exilio.
La teoría que Kuhn propuso transformó la epistemología contemporánea.
Los científicos lo estudiaron durante décadas sin comprenderlo.
El paradigma que dominaba la física colapsó repentinamente.
La evidencia le sugiere al investigador una hipótesis alternativa.
El manuscrito que descubrieron contiene anotaciones marginales extensas.
La revolución industrial transformó las estructuras sociales que prevalecían.
El argumento que desarrolla el autor lo refuta en capítulos posteriores.
Las consecuencias que previeron los economistas nunca se materializaron.
El fenómeno emerge espontáneamente en sistemas complejos.
La crítica que formularon los empiristas le pareció insuficiente a Kant.
El concepto que introduce Foucault desestabiliza las categorías tradicionales.
```

No pretendo que estas doce oraciones constituyan un corpus representativo del español. Son escenas de prueba.

Como la anécdota de Queneau, sirven para volver una y otra vez sobre algo suficientemente estable, no tienen nada esotérico detrás.

---

## Tests

El repositorio incluye un pequeño harness común:

```text
tests/
├── qv_streams/
├── spanish_sentences/
├── spanish_streams/
├── golden/
└── run_tests.py
```

Su función no es certificar una supuesta identidad perfecta entre todos los ports. Sirve para registrar hasta qué punto distintas versiones conservan comportamientos comunes.

Hay varios niveles posibles:

### Traza

```text
state accept done
```

### Reconocimiento

Aceptación o rechazo de la misma secuencia.

### Estructura

En los ports completos:

- árbol;
- spans;
- best-1;
- eventualmente n-best.

El test harness es, en ese sentido, menos una herramienta de calidad que un instrumento para medir la distancia entre variaciones.

---

## Los lenguajes

La colección incluye, entre otros:

### De propósito general

- Python
- Java
- C
- C++
- C#
- Rust
- Ruby
- Perl
- JavaScript
- PHP

### Funcionales, lógicos y científicos

- Erlang
- Haskell
- Standard ML
- Lisp
- Prolog
- R
- MATLAB
- Wolfram Language
- APL

### Históricos

- BASIC
- Pascal
- Fortran
- COBOL
- Short Code
- ALGOL 60
- ALGOL 68
- Logo
- Ada

### Bajo nivel y hardware

- VHDL
- x86-64 Assembly

### Esotéricos

- INTERCAL
- Shakespeare Programming Language
- Brainfuck
- Befunge
- Malbolge

No hay un número final previsto y tampoco encuentro una razón convincente para que lo haya.

---

## Estructura del repositorio

```text
exercices-de-style-dun-parser/
│
├── README.md
├── LICENSE
├── CITATION.cff
├── CONTRIBUTING.md
│
├── resources/
│   ├── grammar.json
│   └── lexicon.json
│
├── ports/
│   ├── python/
│   ├── java/
│   ├── c/
│   ├── cpp/
│   ├── rust/
│   ├── ...
│   ├── brainfuck/
│   ├── befunge/
│   └── malbolge/
│
├── tools/
│
├── tests/
│
└── docs/
```

---

## Cómo leer el repositorio

No hay un recorrido obligatorio. Una posibilidad es empezar por Python y avanzar hacia lenguajes cada vez menos hospitalarios:

```text
Python
↓
C
↓
Fortran
↓
Lisp
↓
Prolog
↓
APL
↓
Assembly
↓
Brainfuck
↓
Befunge
↓
Malbolge
```

Otra es comparar dos extremos directamente: Abrir Python. Después Brainfuck.

Y tratar de encontrar en ambos la misma criatura.

---

## Estado de las implementaciones

No todos los ejercicios tienen el mismo grado de completitud.

Cada port puede quedar clasificado como:

```text
REFERENCE      implementación de referencia
FULL           parser estructural
ADAPTATION     adaptación al paradigma del lenguaje
REDUCED        recognizer o proyección mínima
GENERATED      artefacto generado
EXPERIMENTAL   trabajo todavía no completamente validado
```

Parafrasendo a Sir Isaac: *Uniformitatem non fingo*.

La diferencia entre las versiones *es* el proyecto.

---

## Documentación

- `docs/manual_de_usuario.md` — ejecución y uso de las distintas variantes.
- `docs/como_agregar_un_port.md` — convención para agregar nuevos ejercicios.
- `docs/test_harness.md` — comparación entre ports.
- `CONTRIBUTING.md` — guía de contribución.
- `CITATION.cff` — información de citación.

---

## Licencia

El código se distribuye bajo licencia CC0, salvo indicación específica en algún port.

---

## Referencias

- Queneau, Raymond. *Exercices de style*. Paris: Gallimard, 1947.
- Oulipo. Textos y documentos sobre literatura potencial y *contrainte*.
- Documentación específica de cada lenguaje de programación utilizado.

