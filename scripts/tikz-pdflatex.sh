#!/usr/bin/env bash
# pdflatex for the pandoc-ext/diagram TikZ engine (execpath in _quarto.yml).
# After compiling, Ghostscript turns every glyph into an outline, so the SVG
# that Inkscape makes from the PDF does not depend on TeX fonts being
# installed in the viewer's browser (quarto-dev/quarto-cli#13113).
set -euo pipefail

pdflatex "$@"

outdir="."
tex=""
while [ $# -gt 0 ]; do
  case "$1" in
    -output-directory) outdir="$2"; shift 2 ;;
    -*) shift ;;
    *) tex="$1"; shift ;;
  esac
done
pdf="$outdir/$(basename "${tex%.tex}").pdf"
gs -q -dNOPAUSE -dBATCH -dNoOutputFonts -sDEVICE=pdfwrite \
  -o "$pdf.outlined" "$pdf"
mv "$pdf.outlined" "$pdf"
