# Hand-drawn illustrations for sections/02-pitfalls.qmd.
# Sourced by scripts/generate-sketches.R with scripts/sketch-toolkit.R loaded.
# Teaching drawings, not data.

# "Six purposes, six different mu": one significant coefficient, three readings
sk_open("pitfalls-purposes")
sk_note(6, 6.6, "β, p < 0.05", w = 4.6, h = 1.3, fill = SK$grey_l, cex = 3.2)
sk_arrow(4.6, 5.8, 2.2, 3.4, col = SK$blue, bend = 0.08)
sk_arrow(6, 5.8, 6, 3.4, col = SK$blue)
sk_arrow(7.4, 5.8, 9.8, 3.4, col = SK$blue, bend = -0.08)
sk_note(2, 2.4, "explain?", w = 3.2, h = 1.2, fill = SK$blue_l, cex = 2.9)
sk_note(6, 2.4, "predict?", w = 3.2, h = 1.2, fill = SK$green_l, cex = 2.9)
sk_note(10, 2.4, "confirm?", w = 3.2, h = 1.2, fill = SK$orange_l, cex = 2.9)
sk_text(6, 0.8, "?", cex = 5, col = SK$red, font = 2)
sk_save()

# "The estimand is the target, not the software": aim at the target
sk_open("pitfalls-estimand")
for (r in c(2.6, 1.8, 1.0)) sk_circle(8.3, 4.4, r, col = SK$red, lwd = 4)
sk_circle(8.3, 4.4, 0.3, col = SK$red, solid = SK$red)
sk_text(8.3, 0.9, "estimand", cex = 3.2, col = SK$red)
sk_rect(0.8, 3.2, 3.6, 5.4, fill = SK$grey_l)
sk_rect(0.4, 2.8, 4.0, 3.2, solid = SK$grey_l)
sk_text(2.2, 4.3, "lm()", cex = 3.2)
sk_text(2.2, 1.9, "software", cex = 2.8, col = SK$grey)
sk_arrow(4.3, 4.4, 7.9, 4.4, col = SK$blue, lwd = 5)
sk_text(2.2, 6.9, "aim first", cex = 3, col = SK$blue)
sk_save()

# "A hold-out score does not certify the coefficient"
sk_open("pitfalls-holdout")
sk_rect(0.6, 4.6, 7.4, 6.4, fill = SK$blue_l)
sk_rect(7.4, 4.6, 11.4, 6.4, fill = SK$green_l)
sk_text(4.0, 7.2, "train", cex = 3)
sk_text(9.4, 7.2, "hold-out", cex = 3)
sk_arrow(9.4, 4.4, 9.4, 3.2, col = SK$green)
sk_text(9.4, 2.4, "RMSE ✓", cex = 3.2, col = SK$green)
sk_arrow(4.0, 4.4, 4.0, 3.2, col = SK$red)
sk_text(4.0, 2.4, "β causal ?", cex = 3.2, col = SK$red)
sk_text(4.0, 1.0, "not tested", cex = 2.6, col = SK$red)
sk_save()

# "A p-value is about the data": the tail area under the null
sk_open("pitfalls-pvalue-tail")
f0 <- function(x) 1.2 + 5 * exp(-0.5 * ((x - 5) / 1.5)^2)
xs <- seq(7.4, 10.5, length.out = 40)
polygon(c(xs, rev(xs)), c(f0(xs), rep(1.2, 40)), col = SK$red_l, border = NA)
sk_hachure(c(xs, rev(xs)), c(f0(xs), rep(1.2, 40)), col = SK$red, gap = 0.14)
sk_line(0.6, 1.2, 11.4, 1.2)
sk_curve(f0, 0.6, 11.4, col = SK$blue, lwd = 5)
sk_line(7.4, 0.8, 7.4, 4.4, col = SK$ink, lwd = 5)
sk_text(7.4, 0.45, "observed", cex = 2.6)
sk_text(5, 6.9, "if no effect", cex = 3, col = SK$blue)
sk_arrow(10.6, 4.2, 8.4, 1.9, col = SK$red, bend = 0.2)
sk_text(10.6, 4.7, "p", cex = 4, col = SK$red, font = 2)
sk_save()

# "The tail belongs to a model": two null models, two tails
sk_open("pitfalls-model-tail")
fa <- function(x) 1.2 + 5 * exp(-0.5 * ((x - 5) / 1.4)^2)
fb <- function(x) 1.2 + 4.2 * exp(-0.5 * ((x - 4.7) / 1.0)^2) +
  1.0 * exp(-0.5 * ((x - 7.6) / 1.4)^2)
sk_line(0.6, 1.2, 11.4, 1.2)
sk_curve(fa, 0.6, 11.4, col = SK$blue, lwd = 5)
sk_curve(fb, 0.6, 11.4, col = SK$orange, lwd = 5)
sk_line(8.0, 0.8, 8.0, 4.4, lwd = 5)
sk_text(3.0, 6.9, "model A: p", cex = 3, col = SK$blue)
sk_text(9.4, 6.9, "model B: p'", cex = 3, col = SK$orange)
sk_text(8.0, 0.45, "same data", cex = 2.6)
sk_save()

# "Surprise, on a bit scale": p = 0.05 is about four heads in a row
sk_open("pitfalls-coins", w = 12, h = 6)
for (k in 1:4) {
  x <- 1.6 + (k - 1) * 2.9
  sk_circle(x, 3.6, 1.1, col = SK$orange, fill = SK$orange_l)
  sk_text(x, 3.6, "H", cex = 4, font = 2)
}
sk_text(6, 1.2, "p = 0.05  ≈  4 bits", cex = 3.4, col = SK$blue)
sk_save()

# "A planned question is not a screen": one P against a histogram of many
sk_open("pitfalls-screen")
sk_person(2.2, 2.4, scale = 1.3)
sk_note(2.2, 6.2, "one P", w = 2.8, h = 1.2, fill = SK$green_l, cex = 3)
sk_arrow(2.2, 5.5, 2.2, 4.6, col = SK$green)
sk_text(2.2, 1.0, "planned", cex = 2.8)
sk_line(5.2, 3.0, 5.2, 7.4, col = SK$grey, lwd = 2, lty = 2)
heights <- c(3.6, 2.0, 1.4, 1.2, 1.1, 1.0, 1.0, 0.9)
for (k in seq_along(heights)) {
  x0 <- 6.0 + (k - 1) * 0.7
  sk_rect(x0, 2.4, x0 + 0.6, 2.4 + heights[k], fill = SK$orange, lwd = 3)
}
sk_line(5.8, 2.4, 11.8, 2.4)
sk_text(8.8, 6.9, "10 000 P", cex = 3, col = SK$orange)
sk_text(6.0, 1.7, "0", cex = 2.6)
sk_text(11.6, 1.7, "1", cex = 2.6)
sk_text(8.8, 1.0, "screen", cex = 2.8)
sk_save()

# "The same mistake in a carbon table": a cut at 10 degrees
sk_open("pitfalls-cut")
sk_line(0.8, 4, 11.2, 4, lwd = 5)
for (x in seq(1, 11, by = 1)) sk_line(x, 3.8, x, 4.2, lwd = 2)
sk_line(5.5, 2.0, 5.5, 6.8, col = SK$red, lwd = 6)
sk_text(5.5, 7.3, "cut 10 °C", cex = 2.8, col = SK$red)
sk_dots(c(5.2, 5.8, 10.4), c(4, 4, 4), col = SK$blue, r = 0.22)
sk_text(4.9, 2.9, "9.9", cex = 2.8, adj = 1)
sk_text(6.1, 2.9, "10.1", cex = 2.8, adj = 0)
sk_text(10.4, 2.9, "25", cex = 2.8)
sk_arrow(5.2, 4.6, 5.8, 4.6, col = SK$red, both = TRUE, head = 0.18)
sk_text(4.4, 5.4, "≠", cex = 5, col = SK$red, font = 2)
sk_arrow(6.0, 5.2, 10.3, 5.2, col = SK$red, bend = -0.18, both = TRUE)
sk_text(8.2, 6.5, "=", cex = 5, col = SK$red, font = 2)
sk_save()

# "tau^2 is how far the true effects spread"
sk_open("pitfalls-tau-spread")
sk_line(0.6, 1.6, 11.4, 1.6)
sk_bump(6, 1.6, 4.4, 1.6, 0.8, 11.2, col = SK$violet, lwd = 5)
sk_line(6, 1.6, 6, 6.2, col = SK$ink, lwd = 3, lty = 2)
sk_text(6, 6.8, "μ", cex = 3.6)
sk_arrow(2.9, 2.6, 9.1, 2.6, col = SK$blue, both = TRUE)
sk_text(9.6, 3.3, "± 1.96 τ", cex = 3, col = SK$blue, adj = 0)
sk_dots(c(3.6, 4.7, 5.5, 6.4, 7.0, 8.2), rep(1.6, 6), col = SK$red, r = 0.2)
sk_text(1.4, 0.7, "true site effects θ", cex = 2.8, col = SK$red, adj = 0)
sk_save()

# "Count the unit before you count the rows": 4 sites, 720 rows
sk_open("pitfalls-units")
sx <- c(1.8, 4.6, 7.4, 10.2)
for (s in sx) {
  sk_rect(s - 0.9, 5.4, s + 0.9, 6.8, solid = SK$green_l, lwd = 3)
  for (dx in c(-0.8, 0, 0.8)) {
    sk_line(s, 5.4, s + dx, 3.9, lwd = 2, col = SK$grey)
    for (ddx in c(-0.2, 0, 0.2)) {
      sk_line(s + dx, 3.7, s + dx + ddx, 2.8, lwd = 1.2, col = SK$grey)
    }
    sk_circle(s + dx, 3.7, 0.16, col = SK$grey, lwd = 2)
  }
}
sk_text(6, 7.5, "4 sites", cex = 3.2, col = SK$green)
sk_text(6, 1.8, "720 rows", cex = 3.2, col = SK$grey)
sk_text(6, 0.7, "n = 4, not 720", cex = 3.2, col = SK$red)
sk_save()
