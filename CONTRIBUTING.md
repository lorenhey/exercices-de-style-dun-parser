# Contributing — Exercices de style (d’un parser)

Gracias por interesarte en contribuir.  
Este repositorio es una colección de **ejercicios de estilo**: múltiples ports de un mismo gesto formal
(un parser incremental generalista) en lenguajes muy distintos.

El objetivo no es “mejor parser posible”, sino **máxima expresividad comparativa**.

---

## 1) Filosofía del proyecto

Este repo vive en el cruce de:

- **Lingüística / Letras**: sintaxis, incrementalidad, proyección formal
- **Computación**: parsers, estados, trazas, equivalencias
- **Estilo**: límites de cada lenguaje como forma

Contribuir acá significa respetar ese espíritu.

---

## 2) Qué tipo de contribuciones se aceptan

### A) Nuevos ports (lo más importante)
Un port nuevo es bienvenido si:
- implementa el Parser V7 (completo o minimalista),
- o preserva explícitamente el gesto `step(token)`.

### B) Mejoras de documentación
- README por lenguaje
- manuales de ejecución
- notas teóricas (Letras-friendly)
- ejemplos y corpus de prueba

### C) Suite de tests / harness
- nuevos casos de prueba `q/v/.`
- ampliación de la suite español
- normalizadores de output
- runners más portables

### D) Herramientas (`tools/`)
- generadores de deck
- conversores de formatos
- scripts de validación

---

## 3) Estructura recomendada para un port

Cada port debería vivir en:
