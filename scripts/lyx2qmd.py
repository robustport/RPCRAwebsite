#!/usr/bin/env python3
"""Convert RPCRA vignette .lyx sources to Quarto .qmd pages."""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

from lyx2tex import lyx_to_latex

ROOT = Path(__file__).resolve().parents[1]
PANDOC = Path(r"C:\Users\aakar\tools\pandoc-3.10\pandoc-3.10\pandoc.exe")
LYX_SRC = Path(r"c:\ProfDM_Rproject\Book_Website\PCRA Website\Vignettes")

VIGNETTES = [
    {
        "lyx": "PCRA Package and Data Overview.lyx",
        "qmd": ROOT / "vignette-1.qmd",
        "pdf": "pdfs/vignette-1.pdf",
        "title": "Vignette 1: RPCRA Package and Data Overview",
        "authors": "**Authors:** Doug Martin, Tom Philips, Jon Spinney, and Kirk Li",
        "download": "Vignette 1 — RPCRA Package and Data Overview",
    },
    {
        "lyx": "CRSP Stocks and SPGMI Factors in PCRA.lyx",
        "qmd": ROOT / "vignette-2.qmd",
        "pdf": "pdfs/vignette-2.pdf",
        "title": "Vignette 2: CRSP Stocks and SPGMI Factors in PCRA",
        "authors": "**Authors:** Doug Martin and Jon Spinney",
        "download": "Vignette 2 — CRSP Stocks and SPGMI Factors in PCRA",
    },
    {
        "lyx": "PCRA Reproducibility.lyx",
        "qmd": ROOT / "vignette-3.qmd",
        "pdf": "pdfs/vignette-3.pdf",
        "title": "Vignette 3: RPCRA Reproducibility",
        "authors": None,
        "download": "Vignette 3 — RPCRA Reproducibility",
    },
]

TOC_JS = r"""
<script>
document.addEventListener("DOMContentLoaded", function () {
  const sidebar = document.querySelector("#quarto-margin-sidebar");
  const firstSection = document.querySelector("#quarto-document-content h2, #quarto-document-content h3");

  function alignMarginToc() {
    if (!sidebar || !firstSection) return;
    if (window.innerWidth <= 991) {
      sidebar.style.marginTop = "";
      return;
    }
    const offset = firstSection.getBoundingClientRect().top - sidebar.getBoundingClientRect().top;
    sidebar.style.marginTop = Math.max(0, Math.round(offset)) + "px";
  }

  alignMarginToc();
  window.addEventListener("resize", alignMarginToc);

  const toc = document.querySelector("#quarto-margin-sidebar #TOC, #TOC");
  if (!toc) return;

  const links = [...toc.querySelectorAll('a[href^="#"]')];
  const sections = links
    .map(function (link) { return document.querySelector(link.getAttribute("href")); })
    .filter(Boolean);
  if (!sections.length) return;

  const setActive = function (id) {
    links.forEach(function (link) {
      link.classList.toggle("active", link.getAttribute("href") === "#" + id);
    });
  };

  const observer = new IntersectionObserver(
    function (entries) {
      entries.forEach(function (entry) {
        if (entry.isIntersecting) setActive(entry.target.id);
      });
    },
    { rootMargin: "-15% 0px -65% 0px", threshold: 0 }
  );

  sections.forEach(function (section) { observer.observe(section); });
  if (sections[0]) setActive(sections[0].id);
});
</script>
"""


def clean_caption(text: str) -> str:
    text = re.sub(r"\\[a-zA-Z]+\{([^}]*)\}", r"\1", text)
    text = re.sub(r"\\[a-zA-Z]+", "", text)
    text = re.sub(r"\s+", " ", text).strip()
    return text


def tex_prepare_for_pandoc(tex: str) -> tuple[str, dict[str, str]]:
    placeholders: dict[str, str] = {}

    def stash_verbatim(match: re.Match[str]) -> str:
        key = f"ZZZCODE{len(placeholders)}ZZZ"
        code = match.group(1).strip()
        opts = (
            match.group(2)
            .replace("echo = TRUE", "echo=true")
            .replace("warning = FALSE", "warning=false")
            .replace("eval = FALSE", "eval=false")
            .replace("echo=T", "echo=true")
            .replace("warning=F", "warning=false")
        )
        placeholders[key] = f"\n\n```{{r {opts}}}\n{code}\n```\n\n"
        return key

    tex = re.sub(
        r"\\begin\{verbatim\}\s*(.*?)\\end\{verbatim\}\s*% knitr-options: ([^\n]+)",
        stash_verbatim,
        tex,
        flags=re.DOTALL,
    )

    def stash_figure(match: re.Match[str]) -> str:
        key = f"ZZZFIG{len(placeholders)}ZZZ"
        fname = Path(match.group(1)).name
        caption = clean_caption(match.group(2).strip() if match.lastindex >= 2 and match.group(2) else fname)
        placeholders[key] = f'\n\n![{caption}](vignette-assets/{fname})\n\n'
        return key

    tex = re.sub(
        r"\\begin\{figure\}.*?\\includegraphics\[.*?\]\{vignette-assets/([^\}]+)\}.*?\\caption\{([^\}]*)\}.*?\\end\{figure\}",
        stash_figure,
        tex,
        flags=re.DOTALL,
    )
    return tex, placeholders


def restore_placeholders(md: str, placeholders: dict[str, str]) -> str:
    for key, value in placeholders.items():
        md = md.replace(key, value)
    return md


def pandoc_latex_to_md(tex: str) -> str:
    if not PANDOC.exists():
        raise FileNotFoundError(f"Pandoc not found at {PANDOC}")
    tex, placeholders = tex_prepare_for_pandoc(tex)
    proc = subprocess.run(
        [str(PANDOC), "-f", "latex", "-t", "gfm", "--wrap=none", "--markdown-headings=atx"],
        input=tex,
        text=True,
        capture_output=True,
        encoding="utf-8",
    )
    if proc.returncode != 0:
        raise RuntimeError(proc.stderr or proc.stdout)
    return restore_placeholders(proc.stdout, placeholders)


def normalize_markdown(md: str) -> str:
    md = md.replace("\r\n", "\n")
    md = md.replace(r"\textbackslash{}", "\\")
    md = md.replace(r"\textregistered{}", "®")
    md = md.replace(r"\texttrademark{}", "™")
    md = re.sub(r"\\url\{([^}]+)\}", r"[\1](\1)", md)

    # Remove leaked LaTeX commands
    md = re.sub(r"^\\noindent.*$", "", md, flags=re.MULTILINE)
    md = re.sub(r"^printbibliography\s*$", "", md, flags=re.MULTILINE)
    md = re.sub(r"^newpage\s*$", "", md, flags=re.MULTILINE)
    md = re.sub(r"\\series bold\s+", "**", md)
    md = re.sub(r"\\series default", "**", md)
    md = re.sub(r"\\color blue\s+", "", md)
    md = re.sub(r"\\color inherit", "", md)
    md = re.sub(r"\\begin_inset Quotes eld\\end_inset", '"', md)
    md = re.sub(r"\\begin_inset Quotes erd\\end_inset", '"', md)

    # Convert HTML figures from pandoc to markdown images
    def html_figure(match: re.Match[str]) -> str:
        src = match.group(1)
        cap = clean_caption(match.group(2))
        return f"\n\n![{cap}]({src})\n\n"

    md = re.sub(
        r"<figure[^>]*>\s*<img src=\"([^\"]+)\"[^>]*/>\s*<figcaption>([^<]*)</figcaption>\s*</figure>",
        html_figure,
        md,
        flags=re.DOTALL,
    )

    # Fix broken markdown images from malformed captions
    md = re.sub(
        r"!\[[^\]]*\\textbackslash\{?\]\((vignette-assets/[^)]+)\)",
        r"![](\1)",
        md,
    )

    # Ensure blank lines around fenced code blocks
    md = re.sub(r"([^\n])```\{r", r"\1\n\n```{r", md)
    md = re.sub(r"```\n([A-Za-z#*])", r"```\n\n\1", md)
    md = re.sub(r"```([A-Za-z])", r"```\n\n\1", md)
    md = re.sub(r"([.!?])```\{r", r"\1\n\n```{r", md)

    # Fix spacing inside inline code and broken list items
    md = re.sub(r"`\s+([^`]+?)\s+`", r"`\1`", md)
    md = re.sub(r"\n-\`", "\n- `", md)
    md = re.sub(r"-` ", "- `", md)
    md = re.sub(r"`data\.fram`\s*e", "`data.frame`", md)
    md = re.sub(r"data\.fram e", "data.frame", md)
    md = re.sub(r"\*\*([^*]+)\*\* \*\*", r"**\1**", md)

    md = re.sub(r"\n{3,}", "\n\n", md)
    return md.strip() + "\n"


def wrap_qmd(meta: dict, body: str) -> str:
    authors_block = ""
    if meta.get("authors"):
        authors_block = f"""
::: {{.copyright-notice}}
{meta["authors"]}
:::
"""

    return f"""---
title: "{meta["title"]}"
page-layout: article
toc: true
toc-location: right
toc-depth: 2
toc-title: ""
---

```{{=html}}
<div class="margin-toc-page">
```

{authors_block}
::: {{.pdf-callout}}
::: {{.pdf-icon}}
&#128196;
:::
::: {{.pdf-text}}
**Download vignette**
[{meta["download"]}]({meta["pdf"]}){{target="_blank"}}
:::
:::

{body}

```{{=html}}
</div>
{TOC_JS}
```
"""


def figure_names_from_lyx(lyx_path: Path) -> list[str]:
    text = lyx_path.read_text(encoding="utf-8", errors="replace")
    names = re.findall(r"filename\s+(\S+)", text)
    return [Path(n).name for n in names]


def link_figure_assets(lyx_path: Path, pdf_path: Path, assets_dir: Path) -> None:
    import fitz

    assets_dir.mkdir(parents=True, exist_ok=True)
    targets = figure_names_from_lyx(lyx_path)
    if not targets:
        return
    doc = fitz.open(pdf_path)
    extracted: list[Path] = []
    seen = set()
    for page_index, page in enumerate(doc):
        for img_index, img in enumerate(page.get_images(full=True)):
            xref = img[0]
            if xref in seen:
                continue
            seen.add(xref)
            pix = fitz.Pixmap(doc, xref)
            if pix.n >= 5:
                pix = fitz.Pixmap(fitz.csRGB, pix)
            tmp = assets_dir / f"__tmp_{pdf_path.stem}_{page_index}_{img_index}.png"
            pix.save(str(tmp))
            extracted.append(tmp)
    for target, src in zip(targets, extracted):
        dest = assets_dir / target
        dest.parent.mkdir(parents=True, exist_ok=True)
        if src.exists():
            dest.write_bytes(src.read_bytes())
    for src in extracted:
        if src.exists():
            src.unlink()
    for stale in assets_dir.glob("__tmp_*"):
        stale.unlink(missing_ok=True)


def extract_pdf_images(pdf_path: Path, out_dir: Path) -> None:
    try:
        import fitz  # pymupdf
    except ImportError:
        print(f"Skipping image extraction (pymupdf not installed): {pdf_path.name}")
        return
    out_dir.mkdir(parents=True, exist_ok=True)
    doc = fitz.open(pdf_path)
    seen = set()
    for page_index, page in enumerate(doc):
        for img_index, img in enumerate(page.get_images(full=True)):
            xref = img[0]
            if xref in seen:
                continue
            seen.add(xref)
            pix = fitz.Pixmap(doc, xref)
            if pix.n >= 5:
                pix = fitz.Pixmap(fitz.csRGB, pix)
            name = f"{pdf_path.stem}-p{page_index + 1}-img{img_index + 1}.png"
            pix.save(str(out_dir / name))


def convert_vignette(spec: dict) -> None:
    lyx_path = LYX_SRC / spec["lyx"]
    build_dir = ROOT / "build" / "vignettes"
    build_dir.mkdir(parents=True, exist_ok=True)
    slug = spec["qmd"].stem
    tex_path = build_dir / f"{slug}.tex"
    tex_path.write_text(lyx_to_latex(lyx_path), encoding="utf-8")
    md = normalize_markdown(pandoc_latex_to_md(tex_path.read_text(encoding="utf-8")))
    spec["qmd"].write_text(wrap_qmd(spec, md), encoding="utf-8")
    pdf_src = LYX_SRC / Path(spec["pdf"]).name.replace("pdfs/", "")
    # Map to numbered pdf names in Vignettes folder
    pdf_map = {
        "vignette-1.qmd": "1. PCRA Package and Data Overview.pdf",
        "vignette-2.qmd": "2. CRSP Stocks and SPGMI Factors in PCRA.pdf",
        "vignette-3.qmd": "3. PCRA Reproducibility.pdf",
    }
    pdf_name = pdf_map.get(spec["qmd"].name)
    if pdf_name:
        pdf_path = LYX_SRC / pdf_name
        if pdf_path.exists():
            link_figure_assets(lyx_path, pdf_path, ROOT / "vignette-assets")
    print(f"Converted {lyx_path.name} -> {spec['qmd']}")


def main() -> int:
    if not LYX_SRC.exists():
        print(f"LyX source folder not found: {LYX_SRC}", file=sys.stderr)
        return 1
    for spec in VIGNETTES:
        convert_vignette(spec)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
