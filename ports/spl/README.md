# Shakespeare Programming Language

The shared ChatGPT page references `parser_v7_spl.spl` as a `sandbox:/mnt/data/...` download, but the SPL source itself is not embedded in the public share HTML. The repo keeps this note so the missing artifact is explicit.

The chat described the interface:

- input: numeric tokens via `Listen to your heart.`
- `6` = VERB
- `9` = QUE
- `10` = END
- output per token: `step`, `token`, `state`, `accept`, `done`

Run command intended by the original answer:

```bash
python -m pip install shakespearelang
shakespeare run parser_v7_spl.spl
```