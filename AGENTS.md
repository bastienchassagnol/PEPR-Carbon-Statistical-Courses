# Workshop slides

This repository is the reveal.js deck for the PEPR Carbon statistical workshop. The deck lives at the repository root (`index.qmd`, `styles.scss`, `sections/`). Do not move it into a `slides/` subdirectory. Shared options (format, filters, callouts, bibliography, resources, website search, `llms-txt`) live in `_quarto.yml`; `index.qmd` keeps only the title block and the includes. The website has a second page, `logistic-regression-is-regression.qmd`, an HTML reading guide listed under `project: render` and in the navbar; render the whole site with `quarto render`.

## Language and notation

- British English in slide text, speaker notes, captions, and this file.
- One claim per slide. At most two figures. If a slide needs a scrollbar, split it.
- When a slide states two or more parallel notions, use a bullet list. Mark a term the room must keep with `[term]{.mark}` ([span syntax](https://quarto.org/docs/authoring/markdown-basics.html#other-spans)). Do not mark a whole sentence.
- Keep one notation throughout:
  - \(Y\): response
  - \(x\) or \(\mathbf{x}\): covariate or covariate row
  - \(\mu = \mathbb{E}(Y \mid \mathbf{x})\): conditional mean
  - \(g(\mu)\): link
  - \(i\): observational index
  - \(j\): cluster or experimental unit when the treatment sits on the cluster
- When \(Y\) is binary, \(\mu = \pi\). Do not introduce a second letter for the response.
- Write mathematics with dollar syntax ([equations](https://quarto.org/docs/authoring/markdown-basics.html#equations)): `$...$` inline and `$$...$$` for display. Do not use `\(...\)` or `\[...\]`.
- A display equation that a later slide names gets an id, `$$ ... $$ {#eq-...}`, and is cited as `@eq-...` ([equation cross-references](https://quarto.org/docs/authoring/cross-references.html#equations)).
- Name a source once. Do not add a second sentence about which URL, PDF, or spelling is the same text.
- When a source is named in prose, prefix it once: 📚 book, 📄 article, 🔗 web page, 📊 dataset, 📝 report.

## Model statements

A slide that states what a model assumes, or what it generates, uses the `custom-callout` extension ([Reveal.js support](https://quarto.thecoatlessprofessor.com/custom-callout/qcustom-callout-revealjs.html)). The three types are defined once, under `custom-callout` in `_quarto.yml`:

- `assumption`: the conditions the fit requires
- `model`: the generative equation, in the deck's notation
- `theorem`: a named result from the source, stated as that source states it

A formal statement with conditions, such as one of the central limit theorems, can instead use a Quarto theorem div, `::: {#thm-… name="…"}` ([theorems and proofs](https://quarto.org/docs/authoring/cross-references.html#theorems-and-proofs)), followed by a `.callout-tip` titled "When to use it".

Reveal.js does not collapse callouts. Keep each one to a few lines. The callout is the statement; the sentence around it says what the statement is for. Do not invent a new colour or icon on a single slide. This applies to every model in the deck, not only to smoothers and generalised additive models.

## Diagrams and motion

- Draw every DAG and every generative model in TikZ, rendered by the `diagram` filter ([pandoc-ext/diagram](https://github.com/pandoc-ext/diagram)): a ```` ```{.tikz} ```` block with `%%| filename: dag-…` and `%%| alt: …`. The deck's CSS overrides `%%| width:`; to shrink a tall diagram, add `.fit-fig` to the slide. The node styles and the `\plate{name}{(fit)(list)}{label}` macro are defined once, under `diagram` in `_quarto.yml`. Symbols follow the [RevBayes graphical-model convention](https://revbayes.github.io/tutorials/intro/getting_started#probabilistic-graphical-models):
  - `const`: a square, for a fixed quantity (a covariate, a design value, a parameter treated as fixed)
  - `stoch`: a blue circle, for a latent random variable
  - `obs`: a grey circle, for an observed (clamped) random variable
  - `det`: a dotted circle, for a quantity computed from its parents ($\eta$, $\mu$)
  - `plate`: a dashed rounded rectangle labelled with its index; a nested plate repeats inside the outer one. The outer plate is the independent unit, the inner plate its correlated replicates.
  - `var`: a grey ellipse, for a measured variable in a causal graph
  - `note`: an equation or a word beside a node; include it in the plate's fit when it sits inside the plate
- The causal-role slides in `05-bayesian-causal.qmd` add one palette, also defined under `diagram` in `_quarto.yml`: grey `var` for $X$ and $Y$, orange `role` and `roleedge` for the third variable $Z$, blue `effectedge` for the effect of $X$ on $Y$, and the `\rolelegend` macro. Give such a slide the `.dag-role` class, which fixes the diagram height.
- Equations go in `note` nodes, not inside the node. One diagram per slide. Mermaid stays for workflows that are not models (a pipeline, a decision path). Keep `securityLevel: loose` in the reveal.js `mermaid` options.
- Compiling TikZ needs TeX, Ghostscript and Inkscape. Put `TIKZ_BIN=<absolute path>/scripts/tikz-pdflatex.sh` in `_environment.local` (git-ignored): the wrapper outlines the glyphs, so the SVG does not need TeX fonts in the browser ([quarto-cli#13113](https://github.com/quarto-dev/quarto-cli/issues/13113)). Commit `_diagram-cache/`: CI has no TeX and reuses those SVGs. After editing the styles in `_quarto.yml`, delete `_diagram-cache/` and render again, because the cache key is the diagram source only.
- A list that should appear one item at a time uses Quarto's [incremental lists](https://quarto.org/docs/presentations/revealjs/#incremental-lists): wrap the bullets in `::: {.incremental}`. Leave a writing prompt or a checklist fully visible.
- Auto-animate only adjacent slides that share a claim. Give the persistent piece an explicit `data-id`. Do not mark the whole deck.
- Vertical stacks are the navigation. Level-1 headings are the sections; level-2 headings are the slides inside them. `navigation-mode` stays `vertical`: Down walks the section, Right moves to the next section.
- Cross-links use a heading id, `{#sec-…}`, and a Markdown link `[label](#sec-…)`. Do not link by slide index (`#/2/1`): the index changes when a slide is inserted. Do not turn on `number-sections`. `preview-links` stays `auto`, so an external URL can open in an overlay and a `#sec-…` link still moves inside the deck.
- Reveal.js extensions, installed under `_extensions/` and committed:
  - `quiz` for the closing multiple-choice questions (check with `x`, reset with `q`)
  - `reveal-header` for the running header and the PEPR mark
  - `tabset` when one slide compares implementations of the same model
  - `custom-callout` for an assumption, a model equation, or a theorem
  - `code-fullscreen` for a fullscreen control on code blocks
  - `codewindow` for an echoed R excerpt: wrap the chunk in `::: {.codewindow .r}` and put the file-tab name on the line before the fence
  - `flashcards` for a definition card (flip with `f`; do not reuse `q`, which resets a quiz item)
  - `fontawesome` for the social icons on the opening and closing slides
  - `embedpdf` for the SAMPL guideline
  - `diagram` (pandoc-ext) for the TikZ graphical models
- Partner marks from `logos.svg` sit in the reveal.js footer via `logo: figures/footer-logos.svg`. Do not put that strip back into the header.
- Lightbox is on for every figure (`lightbox.match: auto`). A single image can still set `width` and the `lightbox` class, for example `{width="80%" .lightbox}`. Add `.nolightbox` only when a click must not enlarge the image.

## Figures

- Prefer an existing figure with a known source, or a figure generated by `Rscript scripts/generate-slide-figures.R` (simulations) or `Rscript scripts/generate-data-figures.R` (CA-SYS and SPRUCE).
- Hand-drawn illustrations on text slides are drawn by `Rscript scripts/generate-sketches.R`: primitives in `scripts/sketch-toolkit.R`, one file per section in `scripts/sketches/`, output `figures/generated/sketch-*.png`. Insert them with `{.sketch}` (or `{.sketch-sm}`), an empty caption, and `fig-alt`. A sketch is a teaching drawing, never data. Do not let more than two text-only slides run in a row.
- The two worked datasets are CA-SYS (16S rRNA amplicon counts, `data/16s_RNA_sequencing/`) and SPRUCE (qPCR gene copies, `data/SPRUCE/`). Raw files there are read-only; `scripts/prepare-teaching-data.R` writes the teaching tables to `data/derived/`.
- Simulated figures are illustrative. Say so on the slide, keep seed `20261005`, and do not describe them as CA-SYS or SPRUCE results.
- Regenerated numbers that appear in the text must match `figures/generated/estimates.json` (simulations) or `figures/generated/data-estimates.json` (real data).
- Slides show a short excerpt of `scripts/generate-slide-figures.R` (`echo: true`, `eval: false`), then the saved figure on the next slide. Do not refit the models inside the deck. `output-location: slide` comments unevaluated lines and leaves the figure slide empty, so the figure is a following slide.
- Store borrowed images under `figures/sources/` and generated images under `figures/generated/`. Use `snake_case` file names.
- Do not upscale a source that is too small to read. Redraw it, and point to the original in `figures/sources/` if you still need it.
- Alt text on every image. Credit a figure when the source image carries a credit.
- `Statistical classes.bib` stays at the repository root. It is the deck bibliography.
- Any other figure, exported deck, or bibliography left at the root is intake. Once that content is on a slide, or has been copied under `figures/`, delete the root copy. Do not leave it beside `index.qmd`.

## Tables

- A plain two-column slide stays Markdown.
- A real dataset with more than five rows is a [`reactable`](https://glin.github.io/reactable/) table: pagination, and a cell colour when a column encodes a role. A grouped or merged table stays `gt`.
- A table with merged rows, merged columns, or row and column groups is a [`gt`](https://gt.rstudio.com/) table. `gt` 1.3.0 is the version to use. Group columns with `tab_spanner()`, group rows with `tab_row_group()`, and merge cells with `cols_merge()` or `cols_merge_range()`. Do not draw that table as an image, and do not fake a span with empty Markdown cells.
- Install it when it is missing: `install.packages("gt")`. The publish workflow installs `gt` with knitr, because the linear-algebra slide renders a grouped table.

## Citations

- Literature citations come from `Statistical classes.bib`. Do not invent BibTeX entries, citation keys, or DOIs. Do not point the bibliography at `temp_rag_pvalue/`.
- For a source that is not in that file, link the original page or DOI at the point of use.
- Software that only draws a figure does not need a bibliography entry. Name the script instead.

## Build

```bash
Rscript scripts/prepare-teaching-data.R
Rscript scripts/generate-data-figures.R
Rscript scripts/generate-slide-figures.R
Rscript scripts/generate-sketches.R
quarto render
Rscript scripts/check-slide-links.R
```

`check-slide-links.R` reports `#sec-…` links with no matching heading id, and duplicated ids.

The project uses `execute: freeze: true` in `_quarto.yml`. R chunks run on your machine; their output lives in `_freeze/`, which you commit. GitHub Actions installs Quarto only and reuses the freeze ([Quarto freeze](https://quarto.org/docs/projects/code-execution.html#freeze), [GitHub Pages](https://quarto.org/docs/publishing/github-pages.html#freezing-computations)). After you change an evaluated chunk, render that file once (`quarto render index.qmd` does not re-execute under freeze) or delete `_freeze/` and render again. You need `gt` (and knitr) locally for the table chunks.

Rendered HTML goes to `docs/` and is published to the `gh-pages` branch by `.github/workflows/publish.yml`. Do not commit `docs/`.

Speaker view is `S`. The timing targets are on the section slides: 30, 50, 30, 40, 20, 20, and 30 minutes.
