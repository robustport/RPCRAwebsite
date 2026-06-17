#!/usr/bin/env python3
"""Verify converted vignette QMD files against LyX/PDF sources."""

from __future__ import annotations

import re
from pathlib import Path

import pdfplumber

LYX = Path(r"c:\ProfDM_Rproject\Book_Website\PCRA Website\Vignettes")
ROOT = Path(__file__).resolve().parents[1]

CHECKS = [
    ("vignette-1-v2.qmd", "1. PCRA Package and Data Overview.pdf", [
        "PCRA-1.3", "factorsSPGMIr", "dataNamesPCRA", "NOT Open Source", "devtools",
        "The PCRA Package", "Development Version", "Employment Trends Index",
        "ConferenceBoardETI", "81,144", "Section 2.2", "Table 2.2", "Figure 2",
        "less than about 1%", "1/2%", "fourteen factors", "Rounded to 4 Sig. Digits",
    ]),
    ("vignette-2-v2.qmd", "2. CRSP Stocks and SPGMI Factors in PCRA.pdf", [
        "selectCRSPandSPGMI", "stocksCRSPmonthly", "CapGroup", "getPCRAData",
        "stocksMicroAll", "eKRstocksMonthly", "microcap stocks, the Market",
        "Section 7", "Example 1", "Example 2", "Figure 1", "Figure 2",
        "stocksCRSPmonthly Name", "TickerLast",
    ]),
    ("vignette-3-v2.qmd", "3. PCRA Reproducibility.pdf", [
        "Ch2_Foundations_Demo", "rstudioapi", "PortfolioAnalytics", "demo folder", "Devtools",
        "distinctive reproducibility", "LyX", "lyx.org",
    ]),
]

BREAK_PATTERNS = {
    "broken_image": r"!\[[^\]]*\\textbackslash",
    "truncated_caption": r"!\[[^\]]*\{[^\]]*\]\(",
    "fence_before_image": r"```!\[",
    "bad_chunk_option": r"warning=falseALSE",
    "raw_latex": r"\\(noindent|series bold|color blue|begin_inset|printbibliography)",
    "inset_in_code": r"label = \\begin_inset",
    "html_figure": r"<figure",
    "broken_data_frame": r"data\.frame with",
    "unresolved_ref": r"\b(Table|Figure|Section|Example)\s+\.",
    "hyphenation_break": r"distincti ve|parameter s|constructio n|performan ce",
    "lyx_typo": r"\bLXY\b|LYX document",
    "leaked_layout": r"\\begin_layout|\\end_layout",
    "missing_row_count": r"total number of rows of.*is ,",
}


def qmd_body(text: str) -> str:
    text = re.sub(r"^---.*?---", "", text, flags=re.S)
    text = re.sub(r"```=html.*?```", " ", text, flags=re.S)
    text = re.sub(r"```.*?```", " ", text, flags=re.S)
    return text


def main() -> int:
    failures = 0
    for qmd_name, pdf_name, phrases in CHECKS:
        qmd = (ROOT / qmd_name).read_text(encoding="utf-8")
        body = qmd_body(qmd)
        print(f"=== {qmd_name} ===")

        missing = [p for p in phrases if p.lower() not in qmd.lower()]
        if missing:
            failures += 1
            print(f"  FAIL missing phrases: {missing}")
        else:
            print("  OK key phrases present")

        for label, pattern in BREAK_PATTERNS.items():
            count = len(re.findall(pattern, qmd if label != "unresolved_ref" else body))
            if count:
                failures += 1
                print(f"  FAIL {label}: {count}")
            else:
                print(f"  OK {label}")

        pdf = LYX / pdf_name
        if pdf.exists():
            pdf_words = len(re.findall(r"[A-Za-z0-9']+", " ".join(
                p.extract_text() or "" for p in pdfplumber.open(pdf).pages
            )))
            qmd_words = len(re.findall(r"[A-Za-z0-9']+", body))
            ratio = qmd_words / max(pdf_words, 1)
            print(f"  INFO word ratio qmd/pdf: {ratio:.2f}")
            if ratio < 0.85:
                failures += 1
                print("  WARN substantially shorter than PDF")

        figs = re.findall(r"vignette-assets/([A-Za-z0-9_.-]+)", qmd)
        for fig in figs:
            path = ROOT / "vignette-assets" / fig
            if path.exists():
                print(f"  OK figure asset: {fig}")
            else:
                failures += 1
                print(f"  FAIL missing figure: {fig}")

        print()

    print("FAILED" if failures else "PASSED", f"({failures} issues)")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
