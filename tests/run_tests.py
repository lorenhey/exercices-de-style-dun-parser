#!/usr/bin/env python3
"""Minimal repository-level harness for shared parser fixtures."""

from __future__ import annotations

import json
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def load_json(path: Path) -> object:
    with path.open("r", encoding="utf-8") as fh:
        return json.load(fh)


def validate_resources() -> None:
    grammar = load_json(ROOT / "resources" / "grammar.json")
    lexicon = load_json(ROOT / "resources" / "lexicon.json")

    assert isinstance(grammar, dict), "grammar.json must contain an object"
    assert isinstance(grammar.get("rules"), list), "grammar.json must contain rules[]"
    assert isinstance(lexicon, dict), "lexicon.json must contain an object"
    assert isinstance(lexicon.get("entries"), dict), "lexicon.json must contain entries{}"


def reduced_accepts(stream: str) -> tuple[bool, bool]:
    """Small q/v/. recognizer used as a common reduced reference."""
    seen_verb = False
    done = False
    for ch in stream.strip():
        if done:
            return False, True
        if ch == "v":
            seen_verb = True
        elif ch == "q":
            continue
        elif ch == ".":
            done = True
        else:
            return False, done
    return seen_verb and done, done


def validate_qv_streams() -> None:
    golden = load_json(ROOT / "tests" / "golden" / "qv_streams.json")
    assert isinstance(golden, dict), "qv golden file must contain an object"

    streams_path = ROOT / "tests" / "qv_streams" / "basic.txt"
    streams = [
        line.strip()
        for line in streams_path.read_text(encoding="utf-8").splitlines()
        if line.strip()
    ]

    for stream in streams:
        accept, done = reduced_accepts(stream)
        expected = golden.get(stream)
        assert expected is not None, f"missing golden entry for {stream}"
        assert accept == expected["accept"], f"accept mismatch for {stream}"
        assert done == expected["done"], f"done mismatch for {stream}"


def validate_corpus_alignment() -> None:
    sentences = (ROOT / "tests" / "spanish_sentences" / "corpus.txt").read_text(
        encoding="utf-8"
    ).splitlines()
    streams = (ROOT / "tests" / "spanish_streams" / "corpus.qv").read_text(
        encoding="utf-8"
    ).splitlines()
    assert len(sentences) == len(streams), "spanish corpus and qv streams must align"


def main() -> None:
    validate_resources()
    validate_qv_streams()
    validate_corpus_alignment()
    print("ok")


if __name__ == "__main__":
    main()
