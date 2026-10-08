# Draw the hand-sketched slide illustrations (figures/generated/sketch-*.png).
# Run from the repository root: Rscript scripts/generate-sketches.R
# Each file in scripts/sketches/ draws the sketches of one deck section.
# The drawings are teaching illustrations, not data.

source("scripts/sketch-toolkit.R")

files <- sort(list.files("scripts/sketches", pattern = "\\.R$",
                         full.names = TRUE))
for (f in files) {
  message("Sketching ", basename(f))
  source(f, local = new.env())
}
