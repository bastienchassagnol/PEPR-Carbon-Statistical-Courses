# Hand-drawn illustrations for sections/03-non-gaussian.qmd, part A
# (from the start of the section up to "One skeleton for all three").
# Schematic only: no curve or point here is CA-SYS or SPRUCE data.

# Slide: The ends are allowed to wander
sk_open("nongauss-a-qq-wander", w = 12, h = 6.5)
tq <- qnorm(ppoints(11))
# left: points on the line, looser at the ends
sk_line(0.8, 1.2, 5.2, 5.6, col = SK$grey, lwd = 3)
xs <- 3 + tq * 1.1
ys <- 3.4 + tq * 1.1 + c(-0.5, 0.25, 0.05, -0.08, 0.06, 0, -0.05, 0.08,
                         -0.06, -0.3, 0.55)
sk_dots(xs, ys, col = SK$green, r = 0.14)
sk_text(3, 0.45, "ends wander ✓", cex = 2.6, col = SK$green)
# right: a systematic bend
sk_line(6.8, 1.2, 11.2, 5.6, col = SK$grey, lwd = 3)
xr <- 9 + tq * 1.1
yr <- 3.1 + tq * 0.85 + 0.38 * tq^2
sk_dots(xr, yr, col = SK$red, r = 0.14)
sk_text(9, 0.45, "bend ✗", cex = 2.6, col = SK$red)
sk_save()

# Slide: A product is not a sum
sk_open("nongauss-a-sum-product", w = 12, h = 7)
sk_text(3, 6.4, "a + b + c + ...", cex = 3.2, col = SK$blue)
sk_text(9, 6.4, "a × b × c × ...", cex = 3.2, col = SK$red)
sk_line(6, 0.6, 6, 6.8, col = SK$grey, lwd = 2.5)
sk_line(0.6, 1.2, 5.4, 1.2, lwd = 3)
sk_bump(3, 0.8, 3.6, 1.2, 0.8, 5.2, col = SK$blue, lwd = 5)
sk_line(6.6, 1.2, 11.6, 1.2, lwd = 3)
sk_curve(function(x) {
  z <- pmax(x - 6.7, 1e-3)
  1.2 + 7.8 * dlnorm(z, meanlog = 0.4, sdlog = 0.7)
}, 6.75, 11.5, n = 120, col = SK$red, lwd = 5)
sk_text(3, 0.5, "symmetric", cex = 2.8, col = SK$blue)
sk_text(9, 0.5, "skewed", cex = 2.8, col = SK$red)
sk_save()

# Slide: The assumption is conditional
sk_open("nongauss-a-conditional", w = 12, h = 7)
sk_axes(1, 1, 11.5, 6.6, xlab = "x", ylab = "Y")
sk_line(1.6, 1.8, 11, 5.6, col = SK$blue, lwd = 5)
for (x0 in c(3, 6, 9)) {
  m <- 1.8 + (x0 - 1.6) * (3.8 / 9.4)
  sk_line(x0, m - 1.5, x0, m + 1.5, col = SK$grey, lwd = 2)
  yy <- seq(m - 1.5, m + 1.5, length.out = 60)
  xx <- x0 + 1.1 * exp(-0.5 * ((yy - m) / 0.55)^2)
  sk_path(xx, yy, col = SK$green, lwd = 4, rough = 0.6)
}
sk_text(8.6, 1.7, "Y | x", cex = 3.2, col = SK$green)
sk_save()

# Slide: A transformation can be the right scale
sk_open("nongauss-a-log-ruler", w = 12, h = 5)
sk_line(0.8, 2.4, 11.2, 2.4, lwd = 5)
labs <- c("1", "2", "4", "8", "16")
xs <- seq(1.2, 10.8, length.out = 5)
for (k in seq_along(xs)) {
  sk_line(xs[k], 2.0, xs[k], 2.8, lwd = 4)
  sk_text(xs[k], 1.3, labs[k], cex = 3)
}
for (k in 1:4) {
  sk_arrow(xs[k] + 0.2, 3.1, xs[k + 1] - 0.2, 3.1, col = SK$green,
           bend = 0.25)
  sk_text((xs[k] + xs[k + 1]) / 2, 4.3, "×2", cex = 3, col = SK$green)
}
sk_save()

# Slide: A transform rewrites the question
sk_open("nongauss-a-exp-coef", w = 12, h = 7)
sk_axes(1, 1, 11.4, 6.8, xlab = "β", ylab = "")
# x in units: beta from 0 to 0.6 maps to 1..11; y: 1..1.9 maps to 1..6.6
bx <- function(b) 1 + b / 0.6 * 10
by <- function(v) 1 + (v - 1) / 0.9 * 5.6
sk_curve(function(x) by(1 + (x - 1) / 10 * 0.6), 1, 10.6, col = SK$blue,
         lwd = 5)
sk_curve(function(x) by(exp((x - 1) / 10 * 0.6)), 1, 10.6, col = SK$red,
         lwd = 5)
sk_text(10.0, by(1.6) - 0.9, "1 + β", cex = 2.8, col = SK$blue,
        adj = c(0, 0.5))
sk_text(9.6, by(1.85), "exp(β)", cex = 3, col = SK$red, adj = c(1, 0.5))
sk_line(bx(0.5), 1, bx(0.5), by(exp(0.5)), col = SK$grey, lwd = 2.5)
sk_arrow(bx(0.5) + 0.35, by(1.5), bx(0.5) + 0.35, by(exp(0.5)), col = SK$red,
         lwd = 3, head = 0.2, both = TRUE)
sk_text(bx(0.5), 0.4, "0.5", cex = 2.8)
sk_text(bx(0.05), 0.4, "0.05", cex = 2.8)
sk_save()

# Slide: A logarithm needs a positive response
sk_open("nongauss-a-log-domain", w = 12, h = 7)
sk_rect(0.6, 0.6, 4.4, 6.6, col = SK$red, lwd = 2, fill = SK$red_l)
sk_text(2.5, 6.0, "Y ≤ 0", cex = 3, col = SK$red)
sk_text(2.5, 3.3, "log ?", cex = 3.4, col = SK$red)
sk_axes(4.4, 3.6, 11.6, 6.8, xlab = "", ylab = "")
sk_line(4.4, 3.6, 4.4, 0.6, lwd = 4)
sk_curve(function(x) 3.6 + 1.6 * log((x - 4.4) / 1.2), 4.55, 11.2,
         col = SK$blue, lwd = 5)
sk_text(10.2, 6.6, "log Y", cex = 3, col = SK$blue)
sk_save()

# Slide: The conditionals were already fine
sk_open("nongauss-a-pooled-bells", w = 12, h = 6)
sk_line(0.4, 1, 5.4, 1, lwd = 3)
sk_bump(2, 0.45, 3.2, 1, 0.5, 3.7, col = SK$green, lwd = 5)
sk_bump(3.6, 0.7, 1.6, 1, 1.6, 5.3, col = SK$green, lwd = 5)
sk_text(2.9, 0.4, "each group", cex = 2.6, col = SK$green)
sk_arrow(5.7, 3, 6.7, 3, lwd = 4)
sk_line(7, 1, 11.8, 1, lwd = 3)
hx <- seq(7.1, 11.5, length.out = 12)
hh <- c(0.7, 2.2, 3.4, 3.1, 2.3, 1.7, 1.3, 1.0, 0.7, 0.45, 0.3, 0.2)
for (k in seq_len(11)) {
  sk_rect(hx[k], 1, hx[k + 1], 1 + hh[k], col = SK$red, lwd = 2.2)
}
sk_text(9.4, 0.4, "pooled: skewed", cex = 2.6, col = SK$red)
sk_save()

# Slide: Back-transformation misses the mean
sk_open("nongauss-a-not-commute", w = 12, h = 7)
sk_note(2, 6, "Y", w = 1.6, h = 1.1, fill = SK$green_l, cex = 3.2)
sk_note(10, 6, "log Y", w = 2.6, h = 1.1, fill = SK$grey_l, cex = 3)
sk_note(10, 1.4, "mean", w = 2.6, h = 1.1, fill = SK$grey_l, cex = 3)
sk_note(2, 1.4, "E(Y)", w = 2.2, h = 1.1, fill = SK$green_l, cex = 3)
sk_arrow(2.9, 6, 8.6, 6, lwd = 4)
sk_text(5.8, 6.6, "log", cex = 2.6)
sk_arrow(10, 5.3, 10, 2.1, lwd = 4)
sk_text(10.3, 3.7, "average", cex = 2.4, adj = c(0, 0.5))
sk_arrow(8.6, 1.4, 4.9, 1.4, lwd = 4, col = SK$red)
sk_text(6.8, 2.0, "exp", cex = 2.6, col = SK$red)
sk_text(4.0, 1.4, "≠", cex = 4, col = SK$red)
sk_arrow(2, 5.3, 2, 2.1, lwd = 4, col = SK$green)
sk_text(1.7, 3.7, "average", cex = 2.4, adj = c(1, 0.5), col = SK$green)
sk_save()

# Slide: A constant sum is a simplex
sk_open("nongauss-a-simplex", w = 8, h = 8)
sk_rect(1.5, 1.5, 6.5, 6.5, col = SK$grey, lwd = 2.5)
sk_line(1.5, 6.5, 6.5, 1.5, col = SK$red, lwd = 7)
sk_text(4, 0.7, "x", cex = 3)
sk_text(0.7, 4, "y", cex = 3)
sk_text(5.2, 5.4, "x + y = 1", cex = 3, col = SK$red)
sk_dots(c(2.5, 4.2, 5.6), c(5.5, 3.8, 2.4), col = SK$red, r = 0.16)
sk_save()

# Slide: A logit link stays inside the bounds
sk_open("nongauss-a-logit-bounds", w = 12, h = 7)
sk_line(0.8, 1.5, 11.4, 1.5, col = SK$grey, lwd = 2.5, lty = 2)
sk_line(0.8, 5.5, 11.4, 5.5, col = SK$grey, lwd = 2.5, lty = 2)
sk_text(0.5, 1.5, "0", cex = 3, adj = c(1, 0.5))
sk_text(0.5, 5.5, "1", cex = 3, adj = c(1, 0.5))
sk_curve(function(x) -0.3 + 0.75 * x, 1, 9, col = SK$red, lwd = 4)
sk_curve(function(x) 1.5 + 4 / (1 + exp(-(x - 6) * 1.1)), 1, 11.2,
         col = SK$green, lwd = 6)
sk_text(9.4, 6.4, "line ✗", cex = 2.8, col = SK$red, adj = c(0, 0.5))
sk_text(11.2, 4.7, "π ✓", cex = 2.8, col = SK$green, adj = c(1, 0.5))
sk_save()

# Slide: log Y changes the estimand
sk_open("nongauss-a-gm-am", w = 12, h = 7)
sk_line(0.6, 1.2, 11.6, 1.2, lwd = 3)
f_ln <- function(x) 1.2 + 18 * dlnorm(pmax(x - 0.7, 1e-3), 0.9, 0.9)
sk_curve(f_ln, 0.75, 11.4, n = 150, col = SK$ink, lwd = 5)
gm <- 0.7 + exp(0.9)
am <- 0.7 + exp(0.9 + 0.9^2 / 2)
sk_line(gm, 1.2, gm, f_ln(gm) + 0.2, col = SK$red, lwd = 5)
sk_line(am, 1.2, am, f_ln(am) + 0.2, col = SK$green, lwd = 5)
sk_text(gm + 0.1, 0.55, "exp E(log Y)", cex = 2.6, col = SK$red,
        adj = c(1, 0.5))
sk_text(am + 0.1, 0.55, "E(Y)", cex = 3, col = SK$green, adj = c(0, 0.5))
sk_save()

# Slide: A test is a model with one factor
sk_open("nongauss-a-test-as-line", w = 12, h = 7)
sk_line(1, 1, 11, 1, lwd = 3)
set.seed(4)
ya <- 2.2 + rnorm(7, 0, 0.45)
yb <- 4.8 + rnorm(7, 0, 0.45)
sk_dots(3 + runif(7, -0.4, 0.4), ya, col = SK$blue, r = 0.14)
sk_dots(9 + runif(7, -0.4, 0.4), yb, col = SK$blue, r = 0.14)
sk_line(3, 2.2, 9, 4.8, col = SK$red, lwd = 6)
sk_text(3, 0.4, "group 0", cex = 2.8)
sk_text(9, 0.4, "group 1", cex = 2.8)
sk_text(6, 5.6, "slope = difference", cex = 2.8, col = SK$red)
sk_save()

# Slide: The pair is a random intercept
sk_open("nongauss-a-paired-lines", w = 12, h = 7)
sk_line(1, 1, 11, 1, lwd = 3)
base <- c(2.0, 2.9, 3.6, 4.4, 5.3)
for (k in seq_along(base)) {
  sk_line(3, base[k], 9, base[k] - 0.6, col = SK$blue, lwd = 4)
  sk_dots(c(3, 9), c(base[k], base[k] - 0.6), col = SK$blue, r = 0.13)
}
sk_text(3, 0.4, "before", cex = 2.8)
sk_text(9, 0.4, "after", cex = 2.8)
sk_arrow(1.6, 1.7, 1.6, 5.6, col = SK$red, lwd = 4, both = TRUE)
sk_text(1.2, 6.3, "between people", cex = 2.4, col = SK$red,
        adj = c(0, 0.5))
sk_text(10.6, 3.2, "same\ndrop", cex = 2.6, col = SK$blue)
sk_save()

# Slide: One row, then every row
sk_open("nongauss-a-matrix", w = 12, h = 7)
sk_rect(0.8, 0.8, 1.8, 6.2, col = SK$green, lwd = 4, fill = SK$green_l)
sk_text(1.3, 6.7, "Y", cex = 3.2, col = SK$green)
sk_text(2.5, 3.5, "=", cex = 4)
sk_rect(3.2, 0.8, 7.2, 6.2, col = SK$ink, lwd = 4, fill = SK$grey_l)
sk_rect(3.2, 0.8, 4.0, 6.2, col = SK$ink, lwd = 3, solid = SK$orange_l)
sk_text(3.6, 3.5, "1", cex = 3)
sk_text(5.2, 6.7, "X", cex = 3.2)
sk_rect(7.6, 2.0, 8.4, 5.0, col = SK$red, lwd = 4, fill = SK$red_l)
sk_text(8.0, 5.5, "β", cex = 3.2, col = SK$red)
sk_text(9.2, 3.5, "+", cex = 4)
sk_rect(9.9, 0.8, 10.9, 6.2, col = SK$grey, lwd = 4)
sk_text(10.4, 6.7, "ε", cex = 3.2, col = SK$grey)
sk_line(0.8, 4.6, 7.2, 4.6, col = SK$blue, lwd = 3)
sk_text(0.4, 4.6, "i", cex = 2.8, col = SK$blue)
sk_save()

# Slide: The family matches the type of Y
sk_open("nongauss-a-support", w = 12, h = 7)
# binary
sk_dots(c(3, 5), c(5.9, 5.9), col = SK$blue, r = 0.22)
sk_text(3, 5.1, "0", cex = 2.6)
sk_text(5, 5.1, "1", cex = 2.6)
sk_text(8.6, 5.9, "Bernoulli", cex = 2.8, col = SK$blue, adj = c(0, 0.5))
# counts
sk_arrow(1.6, 3.6, 8, 3.6, lwd = 3, head = 0.22)
sk_dots(seq(2, 7, by = 1), rep(3.6, 6), col = SK$green, r = 0.18)
sk_text(2, 2.9, "0", cex = 2.4)
sk_text(3, 2.9, "1", cex = 2.4)
sk_text(4, 2.9, "2", cex = 2.4)
sk_text(8.6, 3.6, "Poisson", cex = 2.8, col = SK$green, adj = c(0, 0.5))
# real line
sk_arrow(1.6, 1.4, 8, 1.4, lwd = 5, head = 0.25, both = TRUE,
         col = SK$violet)
sk_text(8.6, 1.4, "Gaussian", cex = 2.8, col = SK$violet, adj = c(0, 0.5))
sk_save()

# Slide: The link keeps mu in range
sk_open("nongauss-a-link-map", w = 12, h = 7)
sk_arrow(0.8, 1.4, 11.2, 1.4, lwd = 5, head = 0.25, both = TRUE)
sk_text(6, 0.5, "xᵀβ : any real number", cex = 2.6)
sk_line(4, 5.6, 8, 5.6, col = SK$green, lwd = 7)
sk_line(4, 5.2, 4, 6.0, col = SK$green, lwd = 4)
sk_line(8, 5.2, 8, 6.0, col = SK$green, lwd = 4)
sk_text(4, 6.5, "0", cex = 2.8, col = SK$green)
sk_text(8, 6.5, "1", cex = 2.8, col = SK$green)
for (x0 in c(1.4, 3.8, 6, 8.2, 10.6)) {
  sk_arrow(x0, 1.8, 4 + (x0 - 1) / 10 * 4, 5.1, lwd = 3, col = SK$blue,
           head = 0.2)
}
sk_text(10.6, 4.3, "g⁻¹", cex = 3.2, col = SK$blue)
sk_save()
