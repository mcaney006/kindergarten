# Changelog

## 0.1.0.0 (2026-09-30)

Initial release.

- Exact arithmetic over ℕ, ℤ and ℚ with `+`, `-`, `*`, `/`, `^`, unary minus and parentheses.
- Integer and decimal literals; decimal literals are read as exact rationals.
- Sort inference. Exponents must have sort ℕ or ℤ; other exponents are rejected before evaluation.
- Exponentiation is bounded by binary height, 100,000 bits by default.
- Decimal notation with a configurable number of fractional digits (`--precision`), and exact fractions (`--fraction`).
- One-shot mode (`calculator EXPRESSION`) and an interactive session with `:type`, `:history`, `:help` and `:quit`.
