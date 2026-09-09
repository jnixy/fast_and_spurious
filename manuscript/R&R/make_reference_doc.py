"""
Build a Quarto/pandoc reference-doc for revised-manuscript.qmd's Word output.

Purpose:
    Quarto's `docx` format ignores LaTeX-style font/size YAML fields; Word
    styling (fonts, heading colour, line spacing, margins, page numbers) is
    controlled entirely by a reference .docx whose built-in styles get copied
    onto the rendered manuscript. This script generates that reference doc from
    pandoc's own default template and overrides the styles below to a
    Journal of Quantitative Criminology (Springer) submission look:
    Times New Roman 12pt body, single line spacing, plain bold black
    unnumbered headings, 1" margins, centred page numbers in the footer.

    Line numbering is intentionally NOT added (per author preference; the
    Editorial Manager submission system can add it on the reviewer PDF).
    The author block is left as rendered — blinding is handled separately.

Usage:
    python make_reference_doc.py

Output:
    reference-doc.docx (this directory) -- referenced from
    revised-manuscript.qmd's YAML as `format: docx: reference-doc:`.

Re-run this script and re-render whenever the formatting choices below change;
nothing here reads or writes manuscript content, only style definitions.
"""

import subprocess
from pathlib import Path

from docx import Document
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.oxml import OxmlElement
from docx.oxml.ns import qn
from docx.shared import Inches, Pt, RGBColor

QUARTO_EXE = r"C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe"
HERE = Path(__file__).resolve().parent
OUTPUT = HERE / "reference-doc.docx"

BODY_FONT = "Times New Roman"
BODY_SIZE = Pt(12)
BODY_LINE_SPACING = 1.0  # single-spaced (author preference for JQC)

BODY_STYLES = [
    "Normal",
    "Body Text",
    "Compact",
    "First Paragraph",
    "Image Caption",
    "Table Caption",
    "Bibliography",
    "Footnote Text",
]
HEADING_STYLES = ["Heading 1", "Heading 2", "Heading 3"]
TITLE_BLOCK_STYLES = ["Title", "Author", "Date", "Abstract", "Abstract Title"]


def set_style_font(style, font_name, size=None, bold=None, color=None):
    """Set a style's font name (all script ranges), size, bold, and colour."""
    font = style.font
    font.name = font_name
    if size is not None:
        font.size = size
    if bold is not None:
        font.bold = bold
    if color is not None:
        font.color.rgb = color

    # python-docx's font.name only sets the Latin (w:ascii/w:hAnsi) range; set
    # east-Asian and complex-script ranges too so Word does not silently fall
    # back to a theme font for those runs.
    rpr = style.element.get_or_add_rPr()
    rfonts = rpr.find(qn("w:rFonts"))
    if rfonts is None:
        rfonts = rpr.makeelement(qn("w:rFonts"), {})
        rpr.append(rfonts)
    for attr in ("w:eastAsia", "w:cs"):
        rfonts.set(qn(attr), font_name)

    # Pandoc's default reference doc points Title/Heading styles at theme fonts
    # (w:asciiTheme="majorHAnsi", etc.) alongside a literal font name. When both
    # are present Word resolves the theme reference first, so the literal name is
    # silently ignored. Strip the theme references so only the explicit name
    # applies.
    for theme_attr in ("w:asciiTheme", "w:hAnsiTheme", "w:eastAsiaTheme", "w:cstheme"):
        if rfonts.get(qn(theme_attr)) is not None:
            del rfonts.attrib[qn(theme_attr)]


def add_page_number_footer(doc):
    """Put a centred `PAGE` field in the primary footer of section 1."""
    footer = doc.sections[0].footer
    footer.is_linked_to_previous = False
    para = footer.paragraphs[0] if footer.paragraphs else footer.add_paragraph()
    para.text = ""
    para.alignment = WD_ALIGN_PARAGRAPH.CENTER

    run = para.add_run()
    fld_begin = OxmlElement("w:fldChar")
    fld_begin.set(qn("w:fldCharType"), "begin")
    instr = OxmlElement("w:instrText")
    instr.set(qn("xml:space"), "preserve")
    instr.text = "PAGE"
    fld_sep = OxmlElement("w:fldChar")
    fld_sep.set(qn("w:fldCharType"), "separate")
    fld_end = OxmlElement("w:fldChar")
    fld_end.set(qn("w:fldCharType"), "end")
    for node in (fld_begin, instr, fld_sep, fld_end):
        run._r.append(node)

    # Match the body font on the footer run directly (the "Footer" paragraph
    # style may not exist in pandoc's default reference doc).
    run.font.name = BODY_FONT
    run.font.size = BODY_SIZE


def main():
    # 1. Start from pandoc's own default reference doc (the bundled pandoc that
    #    Quarto already uses for rendering).
    result = subprocess.run(
        [QUARTO_EXE, "pandoc", "--print-default-data-file", "reference.docx"],
        capture_output=True,
        check=True,
    )
    OUTPUT.write_bytes(result.stdout)

    doc = Document(str(OUTPUT))

    def style_if_present(name, **kw):
        try:
            style = doc.styles[name]
        except KeyError:
            return
        set_style_font(style, BODY_FONT, size=kw.get("size", BODY_SIZE),
                       bold=kw.get("bold"), color=kw.get("color"))
        if kw.get("single_space"):
            style.paragraph_format.line_spacing = BODY_LINE_SPACING

    for name in BODY_STYLES:
        style_if_present(name, single_space=True)

    for name in HEADING_STYLES:
        style_if_present(name, bold=True, color=RGBColor(0, 0, 0), single_space=True)

    for name in TITLE_BLOCK_STYLES:
        style_if_present(name, bold=(name == "Title"), single_space=True)

    # 2. One-inch margins on all sides.
    section = doc.sections[0]
    section.left_margin = Inches(1)
    section.right_margin = Inches(1)
    section.top_margin = Inches(1)
    section.bottom_margin = Inches(1)

    # 3. Centred page numbers in the footer.
    add_page_number_footer(doc)

    doc.save(str(OUTPUT))
    print(f"Wrote {OUTPUT}")


if __name__ == "__main__":
    main()
