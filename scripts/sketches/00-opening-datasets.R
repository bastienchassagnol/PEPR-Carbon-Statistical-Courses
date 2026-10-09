# Sketches for sections/00-opening.qmd and sections/01-datasets.qmd.
# Sourced by scripts/generate-sketches.R with the toolkit loaded.
# Every drawing is a schematic, not data.

# Slide: How the four hours are split (blocks drawn in proportion to minutes)
sk_open("opening-agenda", w = 12, h = 4)
mins <- c(30, 50, 70, 20, 20, 50)
cols <- c(SK$blue_l, SK$orange_l, SK$red_l, SK$green_l, SK$violet_l, SK$grey_l)
x <- 0.4 + c(0, cumsum(mins)) / sum(mins) * 11.2
for (k in seq_along(mins)) {
  sk_rect(x[k], 1.4, x[k + 1], 2.9, solid = cols[k], lwd = 3)
  sk_text((x[k] + x[k + 1]) / 2, 2.15, mins[k], cex = 2.8)
}
sk_arrow(0.4, 0.75, 11.6, 0.75, both = TRUE, lwd = 3)
sk_text(6, 3.45, "4 hours, six blocks", cex = 2.8)
sk_save()

# Slide: FairCarboN, and why the carbon budget needs it
sk_open("opening-carbon-budget")
# source: a chimney
sk_rect(1.2, 1.2, 3.6, 3.0, solid = SK$grey_l)
sk_rect(2.8, 3.0, 3.3, 4.2, solid = SK$grey_l)
sk_arrow(3.05, 4.5, 4.6, 6.6, col = SK$red, lwd = 6, bend = -0.15)
sk_text(2.4, 6.9, "emit", cex = 3.2, col = SK$red)
# sink: a tree on soil
sk_rect(6.8, 1.2, 11.2, 2.0, fill = SK$orange, col = SK$ink)
sk_line(9, 2.0, 9, 3.6, lwd = 7)
sk_circle(9, 4.4, 1.1, fill = SK$green, col = SK$green)
sk_arrow(7.0, 6.6, 8.3, 5.2, col = SK$green, lwd = 6, bend = 0.15)
sk_text(9.6, 6.9, "absorb", cex = 3.2, col = SK$green)
# net balance
sk_text(6, 7.5, "net ≈ 0", cex = 3.4, col = SK$blue, font = 2)
sk_line(0.5, 1.2, 11.5, 1.2, lwd = 3)
sk_save()

# Slide: What Y is in that table (three supports for one table)
sk_open("datasets-supports")
rows <- c(6.6, 4.1, 1.6)
labs <- c("reads", "richness", "presence")
for (k in 1:3) {
  sk_text(0.3, rows[k] + 0.75, labs[k], cex = 2.8, adj = c(0, 0.5),
          col = SK$blue)
}
# reads: 0, 1, 2, ... open to the right
sk_arrow(0.5, rows[1], 11.6, rows[1], lwd = 3)
sk_dots(seq(0.8, 10.4, by = 0.8), rep(rows[1], 13), col = SK$green)
sk_text(11.2, rows[1] - 0.6, "∞", cex = 3)
# richness: 0 ... bounded
sk_line(0.5, rows[2], 10.4, rows[2], lwd = 3)
sk_line(10.4, rows[2] - 0.4, 10.4, rows[2] + 0.4, col = SK$red, lwd = 5)
sk_dots(seq(0.8, 10, by = 0.8), rep(rows[2], 12), col = SK$green)
sk_text(10.4, rows[2] - 0.75, "max", cex = 2.6, col = SK$red)
# presence: 0 or 1
sk_line(0.5, rows[3], 6, rows[3], lwd = 3)
sk_dots(c(1, 5), rep(rows[3], 2), col = SK$green, r = 0.18)
sk_text(1, rows[3] - 0.65, "0", cex = 3)
sk_text(5, rows[3] - 0.65, "1", cex = 3)
sk_save()

# Slide: Sampling effort is a covariate (a deeper library sees more taxa)
sk_open("datasets-offset")
set.seed(20261005)
taxa_col <- c(SK$red, SK$blue, SK$green, SK$orange, SK$violet)
# tube A, twice the depth of tube B
sk_rect(1.6, 0.8, 4.2, 6.8, lwd = 4)
sk_rect(7.8, 0.8, 10.4, 3.8, lwd = 4)
for (k in 1:22) sk_dots(runif(1, 1.9, 3.9), runif(1, 1.1, 6.5),
                       col = sample(taxa_col, 1), r = 0.14)
for (k in 1:11) sk_dots(runif(1, 8.1, 10.1), runif(1, 1.1, 3.5),
                       col = sample(taxa_col, 1), r = 0.14)
sk_text(2.9, 7.4, "A: 2× reads", cex = 2.8)
sk_text(9.1, 4.4, "B", cex = 2.8)
sk_arrow(4.6, 4.4, 7.4, 4.4, col = SK$grey, lwd = 3, both = TRUE)
sk_text(6, 5.1, "same soil", cex = 2.6, col = SK$grey)
sk_save()
