#!/usr/bin/env python3
"""Convert static ```r fences to executable Quarto ```{r} chunks."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_TARGETS = sorted(ROOT.glob("vignette*.qmd"))

SETUP_CHUNK = """```{r}
#| include: false
library(PCRA)
library(data.table)
library(xts)
dir.create("vignette-assets", showWarnings = FALSE)
```

"""

CHUNK_RE = re.compile(r"```r\n(.*?)```", re.DOTALL)


def chunk_options(code: str) -> str:
    stripped = code.strip()
    if re.search(r"\bgetPCRAData\s*\(", stripped):
        return "{r echo=TRUE, warning=FALSE, eval=FALSE}"
    if re.search(r"\bhelp\s*\(", stripped):
        return "{r eval=FALSE, echo=TRUE, warning=FALSE}"
    if re.search(r"\bpng\s*\(", stripped):
        return '{r echo=2, warning=FALSE, results="hide"}'
    if re.search(r"\bdata\s*\(\s*package\s*=", stripped):
        return '{r echo=TRUE, warning=FALSE, fig.cap="PCRA package data sets and their corresponding descriptions"}'
    return "{r echo=TRUE, warning=FALSE}"


def normalize_code(code: str) -> str:
    code = code.replace("Plots/", "vignette-assets/")
    return code.strip() + "\n"


def promote_r_chunks(text: str, *, add_setup: bool = True) -> str:
    text = text.replace("\r\n", "\n")

    if not re.search(r"^execute:\s*\n\s*enabled:\s*true", text, re.MULTILINE):
        text = re.sub(
            r"(^toc-title: \"\"\s*\n)",
            r"\1execute:\n  enabled: true\n",
            text,
            count=1,
        )

    # Remove legacy per-page TOC alignment scripts (now in _margin-toc.html).
    text = re.sub(
        r"```\{=html\}\s*</div>\s*<script>.*?</script>\s*```",
        "```{=html}\n</div>\n```",
        text,
        flags=re.DOTALL,
    )

    def repl(match: re.Match[str]) -> str:
        code = normalize_code(match.group(1))
        opts = chunk_options(code)
        return f"```{opts}\n{code}```"

    text = CHUNK_RE.sub(repl, text)

    if add_setup and "library(PCRA)" not in text.split("---", 2)[-1][:800]:
        marker = '<div class="margin-toc-page">'
        html_open = "```{=html}\n" + marker + "\n```"
        text = text.replace(
            html_open,
            html_open + "\n\n" + SETUP_CHUNK,
            1,
        )

    text = re.sub(r"\n{3,}", "\n\n", text)
    return text


def main() -> int:
    paths = [Path(p) for p in sys.argv[1:]] if len(sys.argv) > 1 else DEFAULT_TARGETS
    for path in paths:
        if not path.exists():
            print(f"SKIP missing: {path}")
            continue
        original = path.read_text(encoding="utf-8")
        updated = promote_r_chunks(original)
        if updated != original:
            path.write_text(updated, encoding="utf-8")
            print(f"Updated {path.name}")
        else:
            print(f"No changes: {path.name}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
