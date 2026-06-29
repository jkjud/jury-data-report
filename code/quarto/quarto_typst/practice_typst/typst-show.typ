// typst-show.typ is a Pandoc template processed by Quarto before Typst compilation.
#show: jcc-report.with(
$if(title)$
  title: [$title$],
$endif$
$if(subtitle)$
  subtitle: [$subtitle$],
$endif$
$if(author)$
  author: [$author$],
$endif$
$if(date)$
  date: [$date$],
$endif$
  toc: $toc$,
$if(margin)$
  margin: ($for(margin/pairs)$$it.key$: $it.value$,$endfor$),
$endif$
)