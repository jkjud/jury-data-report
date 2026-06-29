// typst-template.typ — imported by Quarto before typst-show.typ
// Contains the jcc-report function definition.

// ============================================================
// Judicial Council of California — Report Template
// Based on Style and Correspondence Guide (Rev. Jan 2026)
// ============================================================

#let jcc-report(
  title: "Report Title",
  subtitle: none,
  author: none,
  date: none,
  draft: false,
  complex: false,
  body,
) = {

  // ── Page setup ──
  set page(
    paper: "us-letter",
    margin: (top: 1in, bottom: 1in, left: 1in, right: 1in),
    footer: context {
      let pg = counter(page).get().first()
      if pg > 1 {
        align(center, text(font: "Times New Roman", size: 12pt)[#pg])
      }
    },
  )

  // ── Body text: 12pt TNR, ~15pt leading, left-aligned, no indent ──
  set text(
    font: "Times New Roman",
    size: 12pt,
    lang: "en",
    region: "us",
  )
  set par(
    leading: 0.65em,
    first-line-indent: 0pt,
    spacing: 0.65em,
    justify: false,
  )

  // ── Headings ──
  show heading: it => {
    let lvl = it.level
    if complex {
      if lvl == 1 {
        v(24pt)
        align(center, text(font: "Arial", weight: 900, size: 14pt)[#it.body])
        v(14pt)
      } else if lvl == 2 {
        v(18pt)
        text(font: "Arial", weight: "bold", size: 11pt)[#it.body]
        v(6pt)
      } else if lvl == 3 {
        v(12pt)
        text(font: "Times New Roman", weight: "bold", size: 12pt)[#it.body]
        v(4pt)
      } else if lvl == 4 {
        v(12pt)
        text(font: "Times New Roman", weight: "bold", style: "italic", size: 12pt)[#it.body]
        v(4pt)
      } else {
        v(8pt)
        text(font: "Times New Roman", style: "italic", size: 12pt)[#it.body.]
        h(6pt)
      }
    } else {
      if lvl == 1 {
        v(24pt)
        text(font: "Arial", weight: 900, size: 11pt)[#it.body]
        v(6pt)
      } else if lvl == 2 {
        v(18pt)
        text(font: "Arial", weight: "bold", size: 11pt)[#it.body]
        v(4pt)
      } else {
        v(12pt)
        text(font: "Arial", weight: "bold", style: "italic", size: 11pt)[#it.body]
        v(4pt)
      }
    }
  }

  // ── Footnotes: 10pt TNR, ~11pt spacing ──
  set footnote.entry(separator: line(length: 30%, stroke: 0.5pt))
  show footnote.entry: it => {
    set text(font: "Times New Roman", size: 10pt)
    set par(leading: 0.45em)
    it
  }

  // ── Links: underlined per guide (underline = hyperlink) ──
  show link: it => underline(it)

  // ── Tables ──
  set table(
    stroke: (x, y) => {
      if y == 0 { (bottom: 0.8pt, top: 0.8pt) }
      else { (bottom: 0.5pt + luma(200)) }
    },
    inset: 6pt,
  )

  // ── Lists ──
  set list(marker: ([•], [–], [‣]))
  set enum(numbering: "1.a.i.")

  // ── Title block ──
  if complex {
    align(center)[
      #text(font: "Arial", weight: "bold", size: 18pt)[#title]
    ]
    v(6pt)
    if subtitle != none {
      align(center)[#text(font: "Arial", size: 14pt)[#subtitle]]
    }
    v(18pt)
  } else {
    text(font: "Arial", weight: 900, size: 11pt)[#title]
    v(6pt)
    if subtitle != none {
      text(font: "Arial", weight: "bold", size: 11pt)[#subtitle]
    }
    v(12pt)
  }

  if author != none or date != none {
    if author != none { text(size: 11pt)[#author] }
    if author != none and date != none { h(1em) }
    if date != none { text(size: 11pt)[#date] }
    v(12pt)
  }

  if draft {
    place(
      center + horizon,
      rotate(45deg,
        text(font: "Arial", size: 72pt, fill: luma(230), weight: "bold")[DRAFT]
      ),
    )
  }

  body
}