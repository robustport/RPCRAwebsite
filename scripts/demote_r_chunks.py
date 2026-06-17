#!/usr/bin/env python3
"""Convert executable ```{r} chunks to static ```r fences (no R engine on render)."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
TARGETS = [
    ROOT / "vignette-1.qmd",
    ROOT / "vignette-2.qmd",
    ROOT / "vignette-3.qmd",
]


def demote_r_chunks(text: str) -> str:
    text = text.replace("\r\n", "\n")
    text = re.sub(r"```\{r[^`]*\}", "\n\n```r\n", text)
    # Close fences glued to following prose (e.g. ```The dimension...)
    text = re.sub(r"```([A-Z])", r"```\n\n\1", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text


def main() -> int:
    paths = [Path(p) for p in sys.argv[1:]] if len(sys.argv) > 1 else TARGETS
    for path in paths:
        if not path.exists():
            print(f"SKIP missing: {path}")
            continue
        original = path.read_text(encoding="utf-8")
        updated = demote_r_chunks(original)
        if updated != original:
            path.write_text(updated, encoding="utf-8")
            print(f"Updated {path.name}")
        else:
            print(f"No changes: {path.name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
