# PEPR Carbon statistical workshop

Reveal.js slides for a four-hour workshop on statistical modelling of carbon data, with the emphasis on questions, estimands, and responses that are not Gaussian.

The deck is `index.qmd` at the root of this repository. Section files live in `sections/`.

## Render locally

```bash
Rscript scripts/generate-slide-figures.R
quarto render index.qmd
```

Open `docs/index.html`. Speaker view: press `S`.

## GitHub Pages

The workflow `.github/workflows/quarto-pages.yml` renders the deck and deploys it. In the repository settings, set Pages to **GitHub Actions** as the source. The rendered site is not committed.

## Sources

Treatment contrasts for SPRUCE follow the [experimental design page](https://mnspruce.ornl.gov/content/experimental-design). The soil-microbe example is NEON product [DP1.10081.001](https://data.neonscience.org/data-products/DP1.10081.001), superseded in the 2025 release by [DP1.10081.002](https://data.neonscience.org/data-products/DP1.10081.002). Simulated plots use seed `20261005` and are not fits to either dataset.
