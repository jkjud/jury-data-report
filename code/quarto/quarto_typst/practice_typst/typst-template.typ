
#let introline() = {
  pad(top: 1.75em, align(center, line(length: 60%, stroke: 0.4pt + luma(180))))
}

// Document template for JCC Jury Data Report
#let jcc-report(
  title: none,
  subtitle: none,
  author: none,
  date: none,
  toc: false,
  cols: 1,
  margin: (top: 0.5in, bottom: 1in, x: 0.75in),
  paper: "us-letter",
  font: "Linux Libertine",
  fontsize: 11pt,
  body,
) = {

  set page(
    paper: paper,
    margin: margin,
    header: context {
      if counter(page).get().first() > 1 [
        #set text(size: 8pt, fill: luma(130))
        #grid(
          columns: (1fr, 1fr),
          align(left)[Judicial Council of California],
          align(right)[Jury Data Report],
        )
        #line(length: 100%, stroke: 0.4pt + luma(180))
      ]
    },
    footer: context {
      set text(size: 8pt, fill: luma(130))
      line(length: 100%, stroke: 0.4pt + luma(180))
      align(center)[#counter(page).display("1")]
    },
  )

  set text(
    font: font,
    size: fontsize,
    lang: "en",
  )

  set par(
    justify: true,
    leading: 0.65em,
    spacing: 1em
  )

  // Figure & caption spacing
  show figure: set block(above: 2em, below: 2em)
  show figure.caption: set text(size: 12pt, fill: luma(80))
  show figure.where(kind: table): set block(above: 1.2em, below: 1em)

  // Table cell padding
  set table(
    inset: 6pt,
    stroke: 0.4pt + luma(200),
  )

  // Heading styles
  show heading.where(level: 1): it => {
    v(2em, weak: true)
    set text(size: 18pt, weight: "bold")
    it
    v(1em, weak: true)
  }

  show heading.where(level: 2): it => {
    v(2em, weak: true)
    set text(size: 14pt, weight: "bold")
    it
    v(2em, weak: true)
  }

  show heading.where(level: 3): it => {
    v(0.7em, weak: true)
    set text(size: 14pt, weight: "semibold")
    it
    v(1em, weak: true)
  }

  show heading.where(level: 4): it => {
    v(0.5em, weak: true)
    set text(size: 10pt, weight: "regular", fill: luma(80))
    it
    v(0.1em, weak: true)
  }

  // Title block
  set page(
  numbering: none,
  footer: none  // Hide footer on title page
)
  align(center + horizon)[
    #image("100_yr_logo.png", width: 1.5in)
    #v(1em)
    #text(24pt, weight: "bold")[#title]
    #v(1em)
    #if subtitle != none [
      #text(16pt, fill: luma(60), style: "italic")[#subtitle]
      #v(1em)
    ]
    #text(12pt)[#author]
    #v(0.5em)
    #text(12pt, fill: luma(80))[#date]
  ]
  pagebreak()

  set page(
    numbering: "1",
    footer: context {
      set text(size: 8pt, fill: luma(130))
      line(length: 100%, stroke: 0.4pt + luma(180))
      align(center)[#counter(page).display("1")]
    }
  )
  counter(page).update(1)

  if toc {
    outline(
      title: [Contents],
      indent: 1.5em,
      depth: 2,
    )
    pagebreak()
  }

  body
}