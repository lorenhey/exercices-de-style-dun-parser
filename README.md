# Exercices de style (d’un parser)

> «Un mismo parser, reescrito una y otra vez bajo las restricciones de lenguajes de programación distintos.»

## De qué se trata

Este repositorio nace de una idea deliberadamente poco práctica: tomar un mismo parser sintáctico incremental y reescribirlo en la mayor variedad posible de lenguajes de programación.

Python, Java, C, C++, C#, Rust, Erlang, Haskell, Prolog, Lisp, BASIC, Pascal, Fortran, COBOL, ALGOL, APL, Assembly, Brainfuck, Befunge, INTERCAL, Shakespeare Programming Language, Malbolge y otros.

No porque haga falta.

No porque alguno de esos ports vaya a resolver mejor un problema real.

No porque exista una necesidad técnica de disponer de un parser de lenguaje natural escrito en Brainfuck.

Precisamente al contrario.

Este proyecto no tiene un propósito utilitario.

Es un ejercicio de estilo.

El parser es solamente el motivo que permanece mientras cambia la escritura.

---

## Raymond Queneau

El título remite, naturalmente, a *Exercices de style* de Raymond Queneau.

Publicado en 1947, el libro toma un episodio insignificante y lo vuelve a contar noventa y nueve veces. Cambian el registro, la estructura, la voz, el procedimiento, el género, la retórica. La anécdota permanece.

Eso es lo que me interesa.

No reproducir literalmente el procedimiento de Queneau, sino trasladar su principio a otro tipo de escritura.

En este repositorio:

```text
INVARIANTE      -> un parser sintáctico incremental
VARIACIÓN       -> el lenguaje en el que está escrito
```

La situación narrativa de Queneau es reemplazada por un algoritmo.

Los estilos son reemplazados por lenguajes de programación.

La pregunta permanece casi intacta:

¿cuántas formas distintas puede adoptar una misma cosa sin dejar de ser reconocible?

---

## Oulipo

*Exercices de style* es anterior a la fundación del Oulipo. Queneau publicaría el libro en 1947; el Ouvroir de littérature potentielle aparecería en 1960.

No considero por eso este proyecto una aplicación literal de un procedimiento oulipiano.

La relación es más amplia.

Me interesa especialmente la idea de *contrainte*: la restricción como principio generador de forma.

En literatura, una restricción puede ser un lipograma, una estructura matemática, una combinatoria, una regla sintáctica o cualquier otro dispositivo capaz de reducir deliberadamente el espacio de lo posible.

Acá no necesito inventar las restricciones.

Los lenguajes ya vienen con ellas.

Python permite ciertas abstracciones.

C obliga a otras.

Prolog propone otra manera de pensar la relación entre datos y reglas.

Haskell empuja hacia la composición funcional.

APL comprime operaciones enteras en unas pocas expresiones.

Assembly obliga a abandonar casi toda comodidad.

Brainfuck reduce el universo a una cinta, un puntero y ocho instrucciones.

Befunge convierte el programa en un espacio bidimensional.

Malbolge lleva la hostilidad del lenguaje hasta el absurdo.

Cada port es, en ese sentido, una nueva *contrainte*.

El interés no está en vencerla.

Está en observar qué forma produce.

---

## Programar como escribir

En programación se habla constantemente de estilo, pero casi siempre en un sentido menor:

- nombres de variables;
- sangría;
- ubicación de llaves;
- convenciones;
- organización de archivos.

Acá uso la palabra en un sentido más fuerte.

Un lenguaje de programación no cambia solamente la sintaxis superficial de un algoritmo.

Cambia lo que resulta natural decir.

Cambia lo que resulta incómodo.

Cambia lo que puede darse por supuesto.

Cambia qué estructuras aparecen primero en la cabeza.

Cambia qué partes del problema quedan expuestas.

El mismo parser escrito en Prolog no es simplemente el parser de Python con otra puntuación.

El mismo parser escrito en Assembly tampoco.

Hay algo del lenguaje de destino que termina interviniendo necesariamente en la forma final.

Eso es, justamente, lo que quiero conservar.

No me interesa producir traducciones mecánicas que escondan las particularidades de cada lenguaje.

Me interesa que cada implementación tenga algo de su idioma.

---

## ¿Por qué un parser?

Podría haber elegido casi cualquier algoritmo.

Elegí un parser porque el objeto tiene una cualidad que me resulta especialmente apropiada para el experimento: trabaja sobre lenguaje mediante lenguaje.

Un lenguaje de programación intenta analizar una lengua natural.

La forma que analiza y la forma desde la cual se analiza pertenecen a órdenes distintos, pero ambas son sistemas formales.

Eso produce un juego de espejos bastante fértil.

El objeto de partida es un parser sintáctico incremental generalista.

Recibe una oración progresivamente y actualiza su análisis a medida que llegan nuevas palabras.

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

La implementación de referencia del proyecto es el Parser V7.

En sus versiones más completas utiliza una arquitectura de parsing incremental basada en chart, con elementos como:

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

Pero este repositorio no pretende convertir esa arquitectura en un estándar.

El Parser V7 es el tema.

Los ports son las variaciones.

---

## El mismo parser, hasta donde sea posible

Una parte importante del proyecto apareció cuando la traducción empezó a llegar a lenguajes cada vez más restrictivos.

Mientras se trabaja con Python, Java, C o Rust, todavía resulta razonable intentar conservar casi toda la estructura original.

Con Brainfuck la afirmación empieza a volverse ridícula.

Con Malbolge, directamente cómica.

Ahí aparece una pregunta que me interesa más que una fidelidad artificial:

¿qué es lo mínimo que debe sobrevivir para que todavía pueda reconocer el ejercicio original?

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

No quiero escribir Python utilizando sintaxis de Prolog.

Quiero ver qué hace Prolog con el problema.

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

Eso ya no es el Parser V7 completo.

Es una reducción deliberada.

No intento ocultarlo.

La deformación es parte del ejercicio.

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

Código máquina pertenece naturalmente a esta categoría.

Malbolge también puede requerirla.

En esos casos, el proceso de transformación forma parte de la pieza.

---

## No es una colección de benchmarks

Este repositorio no intenta decidir qué lenguaje es mejor.

No mide productividad.

No compara velocidad.

No busca optimizar memoria.

No pretende demostrar que un paradigma sea superior a otro.

Tampoco pretende ofrecer una biblioteca NLP lista para usar.

No hay aquí una carrera entre Python y Rust.

Mucho menos entre Rust y Brainfuck.

La comparación es formal y estética.

Lo que me interesa es observar la distancia entre las soluciones.

A veces esa distancia es pequeña.

A veces dos versiones parecen pertenecer a especies distintas.

Ahí empieza a ponerse interesante.

---

## Lingüística computacional, pero sin coartada utilitaria

El proyecto está claramente situado en la lingüística computacional.

El objeto es un parser.

Hay gramática.

Hay lexicón.

Hay tokenización.

Hay reglas sintácticas.

Hay árboles.

Hay ambigüedad.

Hay incrementalidad.

Hay representación formal.

Todo eso es real.

Pero no utilizo la lingüística computacional como justificación instrumental.

No estoy construyendo una aplicación.

No estoy intentando resolver un problema comercial.

No estoy preparando un parser para producción.

El conocimiento técnico es el material del ejercicio, no su excusa.

Así como Queneau necesitaba conocer profundamente las posibilidades de la lengua para deformarlas con precisión, acá necesito que el parser sea suficientemente real como para que sus variaciones también lo sean.

Si el objeto inicial fuera una caricatura, las transformaciones perderían interés.

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

Cuando un lenguaje no puede consumir esos archivos directamente, genero una representación apropiada:

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

Eso permite mantener relativamente estable aquello que quiero variar menos:

```text
gramática
lexicón
corpus
```

mientras dejo variar aquello que constituye el centro del proyecto:

```text
la escritura del programa
```

---

## Corpus

Uso un corpus pequeño de oraciones en español para recorrer fenómenos sintácticos diferentes.

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

No pretendo que estas doce oraciones constituyan un corpus representativo del español.

Son escenas de prueba.

Como la anécdota de Queneau, sirven para volver una y otra vez sobre algo suficientemente estable.

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

Su función no es certificar una supuesta identidad perfecta entre todos los ports.

Sirve para registrar hasta qué punto distintas versiones conservan comportamientos comunes.

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

No hay un número final previsto.

Tampoco encuentro una razón convincente para que lo haya.

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

No hay un recorrido obligatorio.

Una posibilidad es empezar por Python y avanzar hacia lenguajes cada vez menos hospitalarios:

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

Otra es comparar dos extremos directamente.

Abrir Python.

Después Brainfuck.

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

No veo ninguna necesidad de fingir uniformidad.

La diferencia entre las versiones es el proyecto.

---

## Lo que no quiero hacer con esto

No quiero convertirlo en una librería.

No quiero convertirlo en un framework.

No quiero convertirlo en una suite de benchmarks.

No quiero encontrar el lenguaje más eficiente para implementar un parser.

No quiero reducir todas las versiones hasta obtener una API perfectamente homogénea.

No quiero que los ports extremos sean juzgados por su utilidad práctica.

Un parser en Brainfuck es absurdo si se lo evalúa como software de producción.

Ese juicio es correcto y, al mismo tiempo, irrelevante.

Su existencia acá responde a otra lógica.

---

## Un homenaje

Este repositorio es, antes que nada, un homenaje a *Exercices de style*.

No intento reproducir las noventa y nueve piezas de Queneau ni establecer correspondencias uno a uno entre procedimientos literarios y lenguajes de programación.

Me interesa algo más sencillo.

Tomar una forma.

Mantenerla reconocible.

Someterla a restricciones sucesivas.

Ver qué queda.

Ver qué cambia.

Ver qué aparece únicamente porque una determinada restricción obligó a encontrar una solución que de otro modo no habría existido.

El parser podría haber sido otro objeto.

Pero una vez elegido, deja de importar demasiado si existe una manera mejor de implementarlo.

La pregunta ya no es:

«¿cómo debería escribirse este parser?»

sino:

«¿cómo se ve este parser cuando tiene que ser escrito así?»

Ahí termina la ingeniería como finalidad y empieza el ejercicio de estilo.

---

## Documentación

- `docs/manual_de_usuario.md` — ejecución y uso de las distintas variantes.
- `docs/como_agregar_un_port.md` — convención para agregar nuevos ejercicios.
- `docs/test_harness.md` — comparación entre ports.
- `CONTRIBUTING.md` — guía de contribución.
- `CITATION.cff` — información de citación.

---

## Licencia

El código se distribuye bajo licencia MIT, salvo indicación específica en algún port.

---

## Referencias

- Queneau, Raymond. *Exercices de style*. Paris: Gallimard, 1947.
- Oulipo. Textos y documentos sobre literatura potencial y *contrainte*.
- Documentación específica de cada lenguaje de programación utilizado.

---

## En una línea

Queneau tomó una anécdota y la escribió de noventa y nueve maneras. Yo tomé un parser.
