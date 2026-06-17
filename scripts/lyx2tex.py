#!/usr/bin/env python3
"""Minimal LyX -> LaTeX exporter for RPCRA vignette sources."""

from __future__ import annotations

import re
import sys
from pathlib import Path


LATEX_SPECIAL = str.maketrans({
    "\\": r"\textbackslash{}",
    "&": r"\&",
    "%": r"\%",
    "$": r"\$",
    "#": r"\#",
    "_": r"\_",
    "{": r"\{",
    "}": r"\}",
    "~": r"\textasciitilde{}",
    "^": r"\textasciicircum{}",
})


def esc(text: str) -> str:
    return text.translate(LATEX_SPECIAL)


class LyXParser:
    def __init__(self, text: str, base_dir: Path):
        self.text = text
        self.base_dir = base_dir
        start = text.find("\\begin_body")
        end = text.rfind("\\end_body")
        if start == -1 or end == -1:
            raise ValueError("LyX file missing begin_body/end_body")
        self.body = text[start + len("\\begin_body") : end]
        self.pos = 0

    def parse(self) -> str:
        layouts = self._parse_layouts(self.body)
        return self._layouts_to_latex(layouts)

    def _parse_layouts(self, chunk: str) -> list[dict]:
        layouts: list[dict] = []
        i = 0
        n = len(chunk)
        while i < n:
            m = re.search(r"\\begin_layout\s+([^\n]+)", chunk[i:])
            if not m:
                break
            layout_type = m.group(1).strip()
            content_start = i + m.end()
            depth = 1
            j = content_start
            while j < n and depth:
                if chunk.startswith("\\begin_layout", j):
                    depth += 1
                    j = chunk.find("\n", j) + 1
                    continue
                if chunk.startswith("\\end_layout", j):
                    depth -= 1
                    if depth == 0:
                        break
                    j += len("\\end_layout")
                    continue
                j += 1
            content = chunk[content_start:j]
            layouts.append({"type": layout_type, "content": content.strip()})
            i = j + len("\\end_layout")
        return layouts

    def _layouts_to_latex(self, layouts: list[dict]) -> str:
        parts: list[str] = []
        i = 0
        while i < len(layouts):
            layout = layouts[i]
            t = layout["type"]
            content = layout["content"]

            if t in {"Title", "Author"}:
                i += 1
                continue

            if self._is_setup_layout(content):
                i += 1
                continue

            if t == "Itemize":
                items = []
                while i < len(layouts) and layouts[i]["type"] == "Itemize":
                    items.append(self._inline(layouts[i]["content"]))
                    i += 1
                parts.append("\\begin{itemize}\n" + "\n".join(f"\\item {item}" for item in items) + "\n\\end{itemize}\n\n")
                continue

            if t == "Enumerate":
                items = []
                while i < len(layouts) and layouts[i]["type"] == "Enumerate":
                    items.append(self._inline(layouts[i]["content"]))
                    i += 1
                parts.append("\\begin{enumerate}\n" + "\n".join(f"\\item {item}" for item in items) + "\n\\end{enumerate}\n\n")
                continue

            if t == "Section":
                parts.append(f"\\section{{{self._inline(content, plain=True)}}}\n")
            elif t == "Subsection":
                parts.append(f"\\subsection{{{self._inline(content, plain=True)}}}\n")
            elif t == "Subsubsection":
                parts.append(f"\\subsubsection{{{self._inline(content, plain=True)}}}\n")
            elif t == "Standard":
                rendered = self._inline(content)
                if rendered.strip():
                    parts.append(f"{rendered}\n\n")
            else:
                rendered = self._inline(content)
                if rendered.strip():
                    parts.append(f"{rendered}\n\n")
            i += 1
        return "".join(parts)

    def _inline(self, text: str, plain: bool = False) -> str:
        out: list[str] = []
        i = 0
        emph = False
        tt = False
        while i < len(text):
            if text.startswith("\\begin_inset", i):
                inset, i = self._parse_inset(text, i)
                if inset is not None:
                    out.append(inset)
                continue
            if text.startswith("\\emph on", i):
                if not plain:
                    out.append("\\emph{")
                    emph = True
                i += len("\\emph on")
                continue
            if text.startswith("\\emph default", i):
                if emph and not plain:
                    out.append("}")
                    emph = False
                i += len("\\emph default")
                continue
            if text.startswith("\\family typewriter", i):
                if not plain:
                    out.append("\\texttt{")
                    tt = True
                i += len("\\family typewriter")
                continue
            if text.startswith("\\family default", i):
                if tt and not plain:
                    out.append("}")
                    tt = False
                i += len("\\family default")
                continue
            if text.startswith("\\family sans", i) or text.startswith("\\size ", i):
                nl = text.find("\n", i)
                i = nl + 1 if nl != -1 else len(text)
                continue
            if text.startswith("\\backslash", i):
                if not plain:
                    out.append("\\textbackslash{}")
                i += len("\\backslash")
                continue
            ch = text[i]
            if ch == "\n":
                if not plain:
                    out.append(" ")
                i += 1
                continue
            if not plain:
                out.append(esc(ch))
            else:
                out.append(ch)
            i += 1
        if emph and not plain:
            out.append("}")
        if tt and not plain:
            out.append("}")
        result = re.sub(r"\s+", " ", "".join(out)).strip()
        if not plain:
            result = re.sub(r"\\texttt\{\s+([^}]+?)\s+\}", r"\\texttt{\1}", result)
            result = re.sub(r"\\emph\{\s+([^}]+?)\s+\}", r"\\emph{\1}", result)
        return result

    def _is_setup_layout(self, content: str) -> bool:
        if "include=FALSE" in content and "opts_chunk" in content:
            return True
        if "\\backslash" in content and "global" in content and "def" in content:
            return True
        if re.search(r"\\backslash\s*CRSPTM", content) and "textsuperscript" in content:
            return True
        return False

    def _parse_inset(self, text: str, start: int) -> tuple[str | None, int]:
        m = re.match(r"\\begin_inset\s+([^\n]+)", text[start:])
        if not m:
            return None, start + 1
        inset_type = m.group(1).strip()
        i = start + m.end()
        depth = 1
        j = i
        while j < len(text) and depth:
            if text.startswith("\\begin_inset", j):
                depth += 1
                j = text.find("\n", j) + 1
                continue
            if text.startswith("\\end_inset", j):
                depth -= 1
                if depth == 0:
                    break
                j += len("\\end_inset")
                continue
            j += 1
        inner = text[i:j]
        end = j + len("\\end_inset")
        return self._render_inset(inset_type, inner), end

    def _render_inset(self, inset_type: str, inner: str) -> str:
        if inset_type.startswith("Flex URL"):
            url = self._plain_layout_text(inner).strip()
            return f"\\url{{{url}}}"

        if inset_type.startswith("Quotes eld"):
            return "``"
        if inset_type.startswith("Quotes erd"):
            return "''"

        if inset_type.startswith("VSpace") or inset_type.startswith("Newpage"):
            return ""

        if inset_type.startswith("space"):
            return " "

        if inset_type.startswith("Note"):
            return ""

        if inset_type.startswith("Formula"):
            formula = self._plain_layout_text(inner).strip()
            if not formula or re.fullmatch(r"\$\s*\\;\s*\$", formula) or formula == "$":
                return ""
            if not formula.startswith("$"):
                formula = f"${formula}$"
            return formula

        if inset_type.startswith("ERT"):
            return self._render_ert(inner)

        if inset_type.startswith("Float"):
            return self._render_float(inner)

        if inset_type.startswith("Graphics"):
            m = re.search(r"filename\s+(\S+)", inner)
            if not m:
                return ""
            fname = Path(m.group(1)).name
            return (
                "\\begin{figure}[htbp]\n\\centering\n"
                f"\\includegraphics[width=0.85\\linewidth]{{vignette-assets/{fname}}}\n"
                "\\end{figure}\n"
            )

        if inset_type.startswith("Caption"):
            cap = self._plain_layout_text(inner)
            return f"\\caption{{{esc(cap.strip())}}}"

        if inset_type.startswith("CommandInset"):
            return ""

        return ""

    def _plain_layout_text(self, inner: str) -> str:
        layouts = self._parse_layouts(inner)
        return " ".join(layout["content"] for layout in layouts if layout["type"] in {"Plain Layout", "Standard"})

    def _render_ert(self, inner: str) -> str:
        layouts = self._parse_layouts(inner)
        lines = []
        for layout in layouts:
            if layout["type"] == "Plain Layout":
                lines.extend(layout["content"].splitlines())
        text = "\n".join(line.rstrip() for line in lines).strip()
        if not text:
            return ""

        first = lines[0].strip() if lines else ""
        if first.startswith("<<") and "=" in first:
            return self._render_knitr_chunk(inner)
        if re.fullmatch(r"\\backslash\s*CRSPTM", text.strip()):
            return "CRSP\\textregistered{}"
        if text.startswith("\\backslash"):
            cmd = text.replace("\\backslash", "", 1).strip()
            mapping = {
                "textregistered": "\\textregistered{}",
                "CRSPTM": "CRSP\\textregistered{}",
                "TM": "\\texttrademark{}",
            }
            return mapping.get(cmd, esc(cmd))
        return f"\\texttt{{{esc(text)}}}"

    def _render_knitr_chunk(self, inner: str) -> str:
        header = None
        body: list[str] = []
        for layout in self._parse_layouts(inner):
            if layout["type"] != "Plain Layout":
                continue
            content = layout["content"].strip()
            if not content:
                continue
            if content.startswith("<<") and "=" in content:
                header = content
                continue
            if content == "@":
                break
            body.append(self._code_line(content))
        if header and "include=FALSE" in header:
            return ""
        body_text = "\n".join(body).strip()
        opts = ""
        if header:
            raw = header.strip()
            if raw.startswith("<<"):
                raw = raw[2:]
            if raw.endswith(">>="):
                raw = raw[:-3]
            elif raw.endswith(">>"):
                raw = raw[:-2]
            opts = self._code_line(raw)
        return (
            "\\begin{verbatim}\n"
            f"{body_text}\n"
            "\\end{verbatim}\n"
            f"% knitr-options: {opts}\n"
        )

    def _code_line(self, text: str) -> str:
        out: list[str] = []
        i = 0
        while i < len(text):
            if text.startswith("\\begin_inset", i):
                inset, i = self._parse_inset(text, i)
                if inset in {"``", "''"}:
                    out.append('"')
                elif inset:
                    out.append(inset)
                continue
            ch = text[i]
            if ch == "\n":
                i += 1
                continue
            out.append(ch)
            i += 1
        return "".join(out)

    def _render_float(self, inner: str) -> str:
        fig = ""
        caption = ""
        for inset_type, chunk in self._iter_insets(inner):
            if inset_type.startswith("Graphics"):
                m = re.search(r"filename\s+(\S+)", chunk)
                if m:
                    fname = Path(m.group(1)).name
                    fig = f"\\includegraphics[width=0.85\\linewidth]{{vignette-assets/{fname}}}"
            if inset_type.startswith("Caption"):
                cap = self._plain_layout_text(chunk)
                cap = re.sub(r"\\family typewriter\s*", "", cap)
                cap = re.sub(r"\\family default\s*", "", cap)
                caption = esc(cap.strip())
        if not fig:
            return ""
        cap = f"\\caption{{{caption}}}" if caption else ""
        return f"\\begin{{figure}}[htbp]\n\\centering\n{fig}\n{cap}\n\\end{{figure}}\n"

    def _iter_insets(self, text: str):
        i = 0
        while i < len(text):
            m = re.search(r"\\begin_inset\s+([^\n]+)", text[i:])
            if not m:
                break
            inset_type = m.group(1).strip()
            content_start = i + m.end()
            depth = 1
            j = content_start
            while j < len(text) and depth:
                if text.startswith("\\begin_inset", j):
                    depth += 1
                    j = text.find("\n", j) + 1
                    continue
                if text.startswith("\\end_inset", j):
                    depth -= 1
                    if depth == 0:
                        break
                    j += len("\\end_inset")
                    continue
                j += 1
            yield inset_type, text[content_start:j]
            i = j + len("\\end_inset")


def lyx_to_latex(lyx_path: Path) -> str:
    text = lyx_path.read_text(encoding="utf-8", errors="replace")
    parser = LyXParser(text, lyx_path.parent)
    body = parser.parse()
    return (
        "\\documentclass{article}\n"
        "\\usepackage{graphicx}\n"
        "\\usepackage{hyperref}\n"
        "\\usepackage{fancyvrb}\n"
        "\\usepackage{framed}\n"
        "\\provideenvironment{Shaded}{}{}\n"
        "\\begin{document}\n"
        f"{body}\n"
        "\\end{document}\n"
    )


def main() -> int:
    if len(sys.argv) != 3:
        print("Usage: lyx2tex.py input.lyx output.tex", file=sys.stderr)
        return 1
    src = Path(sys.argv[1])
    dst = Path(sys.argv[2])
    dst.write_text(lyx_to_latex(src), encoding="utf-8")
    print(f"Wrote {dst}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
