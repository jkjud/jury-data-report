// typst-show.typ — Quarto calls this to wrap the document body
// in the jcc-report template function.

#show: doc => jcc-report(
  title: "$title$",
  $if(subtitle)$subtitle: "$subtitle$",$endif$
  $if(author)$author: "$author$",$endif$
  $if(date)$date: "$date$",$endif$
  $if(draft)$draft: $draft$,$endif$
  $if(complex)$complex: $complex$,$endif$
  doc,
)