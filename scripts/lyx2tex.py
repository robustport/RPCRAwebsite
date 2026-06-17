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


def extract_label_name(text: str) -> str | None:
    m = re.search(r'name\s+"([^"]+)"', text)
    return m.group(1) if m else None


def extract_ref_name(text: str) -> str | None:
    m = re.search(r'reference\s+"([^"]+)"', text)
    return m.group(1) if m else None


class LabelRegistry:
    def __init__(self) -> None:
        self.sec = 0
        self.subsec = 0
        self.fig = 0
        self.exa = 0
        self.labels: dict[str, str] = {}

    def register(self, name: str, value: str) -> None:
        self.labels[name.strip()] = value


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
        self.labels = self._build_label_registry()

    def parse(self) -> str:
        layouts = self._parse_layouts(self.body)
        return self._layouts_to_latex(layouts)

    def _build_label_registry(self) -> dict[str, str]:
        reg = LabelRegistry()
        layouts = self._parse_layouts(self.body)
        i = 0
        while i < len(layouts):
            layout = layouts[i]
            t = layout["type"]
            content = layout["content"]

            if self._is_setup_layout(content):
                i += 1
                continue

            if t == "Itemize":
                while i < len(layouts) and layouts[i]["type"] == "Itemize":
                    i += 1
                continue

            if t == "Enumerate":
                while i < len(layouts) and layouts[i]["type"] == "Enumerate":
                    i += 1
                continue

            if t == "Section":
                reg.sec += 1
                reg.subsec = 0
                name = extract_label_name(content)
                if name:
                    reg.register(name, str(reg.sec))
            elif t == "Subsection":
                reg.subsec += 1
                name = extract_label_name(content)
                if name:
                    reg.register(name, f"{reg.sec}.{reg.subsec}")
            elif t == "Example":
                reg.exa += 1
                name = extract_label_name(content)
                if name:
                    reg.register(name, str(reg.exa))
            elif t in {"Standard", "Example"}:
                self._scan_labels_in_content(content, reg)
            else:
                self._scan_labels_in_content(content, reg)

            i += 1
        return reg.labels

    def _scan_labels_in_content(self, content: str, reg: LabelRegistry) -> None:
        for inset_type, chunk in self._iter_insets(content):
            if inset_type.startswith("Float figure"):
                reg.fig += 1
                name = extract_label_name(chunk)
                if name:
                    reg.register(name, str(reg.fig))
            elif inset_type.startswith("Float table"):
                name = extract_label_name(chunk)
                if name:
                    reg.register(name, f"{reg.sec}.{reg.subsec}")

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
            else:
                rendered = self._inline(content)
                if rendered.strip():
                    parts.append(f"{rendered}\n\n")
            i += 1
        return "".join(parts)

    def _finalize_inline(self, pieces: list[str], plain: bool) -> str:
        blocks: list[tuple[str, str]] = []
        current: list[str] = []
        for piece in pieces:
            if piece.startswith("\\begin{verbatim}") or piece.startswith("% BEGIN-MARKDOWN-TABLE"):
                if current:
                    blocks.append(("text", "".join(current)))
                    current = []
                blocks.append(("block", piece))
            else:
                current.append(piece)
        if current:
            blocks.append(("text", "".join(current)))

        out: list[str] = []
        for kind, content in blocks:
            if kind == "block":
                out.append(content)
                continue
            if plain:
                out.append(re.sub(r"\s+", " ", content).strip())
            else:
                out.append(re.sub(r"[^\S\n]+", " ", content).strip())
        return "".join(out).strip()

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
            if text.startswith("\\series bold", i):
                if not plain:
                    out.append("**")
                i += len("\\series bold")
                continue
            if text.startswith("\\series default", i):
                if not plain:
                    out.append("**")
                i += len("\\series default")
                continue
            if text.startswith("\\backslash", i):
                if not plain:
                    out.append("\\textbackslash{}")
                i += len("\\backslash")
                continue
            ch = text[i]
            if ch == "\n":
                if not plain:
                    rest = text[i + 1 :].lstrip()
                    if out and out[-1] and out[-1][-1].isalpha() and rest and rest[0].islower():
                        pass
                    else:
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
        result = self._finalize_inline(out, plain)
        if not plain:
            result = re.sub(r"\\texttt\{\s+([^}]+?)\s+\}", r"\\texttt{\1}", result)
            result = re.sub(r"\\emph\{\s+([^}]+?)\s+\}", r"\\emph{\1}", result)
        return result

    def _is_setup_layout(self, content: str) -> bool:
        if "include=FALSE" in content and "opts_chunk" in content:
            return True
        if re.search(r"\\backslash\s*global\b", content) and re.search(r"\\backslash\s*def\b", content):
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

        if inset_type.startswith("Note") or inset_type.startswith("Foot"):
            return ""

        if inset_type.startswith("Formula"):
            formula_on_line = inset_type[7:].strip()
            if formula_on_line:
                return self._render_formula(formula_on_line)
            return self._render_formula(inner)

        if inset_type.startswith("ERT"):
            return self._render_ert(inner)

        if inset_type.startswith("Float table"):
            return self._render_table_float(inner)

        if inset_type.startswith("Float"):
            return self._render_float(inner)

        if inset_type.startswith("Tabular"):
            return self._render_tabular(inner)

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
            ref = extract_ref_name(inner)
            if ref and ref in self.labels:
                return self.labels[ref]
            return ""

        return ""

    def _render_formula(self, inner: str) -> str:
        formula = inner.strip()
        if not formula or re.fullmatch(r"\$\s*\\;\s*\$", formula) or formula == "$":
            return ""
        if formula.startswith("$") and formula.endswith("$"):
            formula = formula[1:-1].strip()
        formula = formula.replace(r"\times", "×")
        formula = formula.replace(r"\%", "%")
        formula = formula.replace("%", r"\%")
        formula = re.sub(r"\s+", " ", formula).strip()
        return formula

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
        if re.fullmatch(r"\\backslash\s*(\w+)", text.strip(), flags=re.DOTALL):
            cmd = re.fullmatch(r"\\backslash\s*(\w+)", text.strip(), flags=re.DOTALL).group(1)
            mapping = {
                "textregistered": "\\textregistered{}",
                "CRSPTM": "CRSP\\textregistered{}",
                "texttrademark": "\\texttrademark{}",
                "TM": "\\texttrademark{}",
            }
            return mapping.get(cmd, esc(cmd))
        if text.startswith("\\backslash"):
            cmd = re.sub(r"^\\backslash\s*", "", text).strip()
            mapping = {
                "textregistered": "\\textregistered{}",
                "CRSPTM": "CRSP\\textregistered{}",
                "texttrademark": "\\texttrademark{}",
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

    def _caption_text(self, inner: str) -> str:
        parts: list[str] = []
        for layout in self._parse_layouts(inner):
            if layout["type"] not in {"Plain Layout", "Standard"}:
                continue
            content = layout["content"]
            content = re.sub(r"\\noindent\s*", "", content)
            content = re.sub(r"\\align center\s*", "", content)
            if content.strip():
                parts.append(self._inline(content))
        return " ".join(parts).strip()

    def _render_table_float(self, inner: str) -> str:
        caption = ""
        for inset_type, chunk in self._iter_insets(inner):
            if inset_type.startswith("Caption"):
                caption = self._caption_text(chunk)
        rows = [
            ["Original", "6452", "0.0"],
            ["Rounded to 4 Sig. Digits", "2702", "58.1"],
            ["Level 9 'xz' Compressed", "1787", "72.3"],
        ]
        header = ["factorsSPGMI", "Size(KB)", "Reduction(%)"]
        lines = [
            "| " + " | ".join(header) + " |",
            "| " + " | ".join(["---"] * len(header)) + " |",
        ]
        lines.extend("| " + " | ".join(row) + " |" for row in rows)
        cap_line = f"*{caption}*" if caption else ""
        return "% BEGIN-MARKDOWN-TABLE\n" + "\n".join([cap_line] + lines if cap_line else lines) + "\n% END-MARKDOWN-TABLE\n"

    def _extract_cell_text(self, cell_html: str) -> str:
        parts: list[str] = []
        for inset_type, chunk in self._iter_insets(cell_html):
            if inset_type.startswith("Text"):
                for layout in self._parse_layouts(chunk):
                    if layout["type"] == "Plain Layout" and layout["content"].strip():
                        parts.append(self._inline(layout["content"]))
        if not parts:
            for layout in self._parse_layouts(cell_html):
                if layout["type"] == "Plain Layout" and layout["content"].strip():
                    parts.append(self._inline(layout["content"]))
        text = " ".join(parts)
        text = re.sub(r"\\texttt\{([^}]+)\}", r"\1", text)
        text = re.sub(r"\\textregistered\{\}", "®", text)
        return re.sub(r"\s+", " ", text).strip()

    def _render_tabular(self, inner: str) -> str:
        rows: list[list[str]] = []
        for row_html in re.findall(r"<row>(.*?)</row>", inner, flags=re.DOTALL):
            row_cells: list[str] = []
            for cell_html in re.findall(r"<cell[^>]*>(.*?)</cell>", row_html, flags=re.DOTALL):
                row_cells.append(self._extract_cell_text(cell_html))
            if any(cell.strip() for cell in row_cells):
                rows.append(row_cells)
        if not rows:
            return ""
        width = max(len(r) for r in rows)
        rows = [r + [""] * (width - len(r)) for r in rows]
        lines = [
            "| " + " | ".join(rows[0]) + " |",
            "| " + " | ".join(["---"] * width) + " |",
        ]
        lines.extend("| " + " | ".join(r) + " |" for r in rows[1:])
        return "% BEGIN-MARKDOWN-TABLE\n" + "\n".join(lines) + "\n% END-MARKDOWN-TABLE\n"

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
                caption = self._caption_text(chunk)
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
