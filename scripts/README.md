# Vignette conversion scripts

Automated pipeline (no manual rewrite):

```
.lyx  →  [lyx2tex.py]  →  .tex  →  [Pandoc]  →  .md  →  [lyx2qmd.py]  →  vignette-*.qmd
```

LyX GUI is **not** required. `lyx2tex.py` implements a focused LyX → LaTeX exporter for the RPCRA vignette sources.

## Regenerate vignette pages

```powershell
python scripts/lyx2qmd.py
```

Sources are read from:

`c:\ProfDM_Rproject\Book_Website\PCRA Website\Vignettes\`

Outputs:

- `vignette-1.qmd`, `vignette-2.qmd`, `vignette-3.qmd`
- `vignette-assets/*.png` (figures extracted from the vignette PDFs)

## Requirements

- Python 3
- Pandoc 3.x (`C:\Users\aakar\tools\pandoc-3.10\pandoc-3.10\pandoc.exe`)
- `pymupdf` (`pip install pymupdf`) for figure extraction
