#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import sys
import unicodedata

# Heurística mínima (sin diccionario):
# - "que" -> q
# - verbos probables por sufijos frecuentes (con acentos ya normalizados)
VERB_SUFFIXES = (
    # infinitivos / gerundios / participios
    "ar", "er", "ir", "ando", "iendo", "ado", "ido",
    # pretérito / imperfecto / condicional (formas comunes)
    "aron", "ieron", "aba", "aban", "ia", "ian", "ias", "iamos",
    "aste", "iste", "o", "e", "a", "an", "en", "amos", "emos", "imos",
)

# Para reducir falsos positivos con terminaciones "a/o/e":
# - exigimos longitud mínima si el sufijo es muy corto
SHORT_SUFFIX_MINLEN = {
    "o": 5, "a": 5, "e": 5, "an": 5, "en": 5
}

def strip_accents(s: str) -> str:
    return "".join(
        ch for ch in unicodedata.normalize("NFKD", s)
        if not unicodedata.combining(ch)
    )

def looks_like_verb(w: str) -> bool:
    if len(w) < 3:
        return False
    for suf in VERB_SUFFIXES:
        if w.endswith(suf):
            minlen = SHORT_SUFFIX_MINLEN.get(suf, 0)
            return len(w) >= max(3, minlen)
    return False

def main() -> None:
    txt = sys.stdin.read()
    txt = strip_accents(txt.lower())

    # Tokenización ultraligera: palabras o signos de fin
    tokens = re.findall(r"[a-zñ]+|[.!?]", txt)

    out = []
    for t in tokens:
        if t in ".!?":
            out.append(".")
        elif t == "que":
            out.append("q")
        elif looks_like_verb(t):
            out.append("v")

    sys.stdout.write("".join(out))

if __name__ == "__main__":
    main()
