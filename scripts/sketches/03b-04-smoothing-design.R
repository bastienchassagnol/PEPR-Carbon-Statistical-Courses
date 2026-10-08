# Sketches for sections/03b-smoothing.qmd and sections/04-design.qmd.
# Sourced by scripts/generate-sketches.R with the toolkit loaded.
# Schematic only: no curve here is a CA-SYS or SPRUCE result.

# ---------------------------------------------------------------- smoothing

# Slide: Flexibility spends variance {#sec-bias-variance}
sk_open("smoothing-bias-variance")
sk_axes(1, 1, 11.4, 7.4, xlab = "flexibility", cex = 2.6)
sk_curve(function(x) 1.3 + 5.2 * exp(-(x - 1) / 2.2), 1.3, 10.8,
         col = SK$blue, lwd = 5)
sk_curve(function(x) 1.3 + 0.045 * (x - 1)^2, 1.3, 10.8,
         col = SK$red, lwd = 5)
tot <- function(x) 1.6 + 5.2 * exp(-(x - 1) / 2.2) + 0.045 * (x - 1)^2
sk_curve(tot, 1.5, 10.6, col = SK$ink, lwd = 6)
xs <- seq(1.5, 10.6, by = 0.01); xm <- xs[which.min(tot(xs))]
sk_line(xm, 1, xm, tot(xm), col = SK$green, lwd = 3, lty = 2)
sk_dots(xm, tot(xm), col = SK$green, r = 0.16)
sk_text(2.7, 6.9, "bias²", col = SK$blue, cex = 3)
sk_text(10.2, 6.6, "variance", col = SK$red, cex = 3)
sk_text(xm, 0.45, "best", col = SK$green, cex = 2.8)
sk_save()

# Slide: Fit a line in the window {#sec-local-linear}
sk_open("smoothing-local-linear")
sk_axes(0.8, 1, 11.4, 7.6, xlab = "x", cex = 2.6)
f <- function(x) 1.4 + 0.028 * x^2.2
sk_rect(8, 1.1, 11, 7.2, col = SK$grey, lwd = 2, fill = SK$grey_l)
set.seed(7)
xd <- seq(1.2, 10.8, length.out = 22)
sk_dots(xd, f(xd) + rnorm(22, 0, 0.25), col = SK$grey, solid = SK$grey)
sk_curve(f, 1, 11, col = SK$ink, lwd = 3)
# local constant: the weighted mean, pulled down at the edge
sk_line(9, 5.4, 11, 5.4, col = SK$red, lwd = 6)
# local linear: follows the slope to the edge
sk_line(9, f(9), 11, f(11), col = SK$green, lwd = 6)
sk_text(7.7, 6.6, "line ✓", col = SK$green, cex = 3, adj = c(1, 0.5))
sk_text(7.7, 4.9, "constant ✗", col = SK$red, cex = 3, adj = c(1, 0.5))
sk_text(9.5, 7.55, "edge", col = SK$grey, cex = 2.4)
sk_save()

# Slide: GCV chooses the bandwidth {#sec-gcv}
sk_open("smoothing-gcv")
sk_axes(1, 1, 11.4, 5.6, xlab = "h", cex = 3)
sk_text(1.1, 6.0, "GCV", cex = 3, adj = c(0, 0.5))
g <- function(x) 1.6 + 0.12 * (x - 5.6)^2 + 1.6 / x
sk_curve(g, 1.3, 11, col = SK$blue, lwd = 6)
xs <- seq(1.3, 11, by = 0.01); xm <- xs[which.min(g(xs))]
sk_dots(xm, g(xm), col = SK$green, r = 0.17)
sk_text(xm, 0.45, "ĥ", col = SK$green, cex = 3.2)
sk_line(xm, 1, xm, g(xm), col = SK$green, lwd = 3, lty = 2)
# insets: noisy fit on the left, flat fit on the right
sk_path(seq(1.6, 4.0, length.out = 9),
        7.0 + c(0, .5, -.4, .6, -.5, .4, -.3, .5, 0), col = SK$red, lwd = 4)
sk_text(2.8, 6.0, "noisy", col = SK$red, cex = 2.6)
sk_line(8.4, 7.1, 10.8, 7.1, col = SK$orange, lwd = 4)
sk_text(9.6, 6.3, "flat", col = SK$orange, cex = 2.6)
sk_save()

# Slide: B-splines span the same curves {#sec-b-spline}
sk_open("smoothing-bspline", w = 12, h = 6)
sk_line(0.6, 1, 11.4, 1, lwd = 3)
xx <- seq(0, 1, length.out = 200)
B <- splines::bs(xx, df = 8, degree = 3, intercept = TRUE)
for (k in seq_len(ncol(B))) {
  xk <- 0.8 + 10.4 * xx
  yk <- 1 + 4.2 * B[, k]
  keep <- yk > 1.01
  col <- if (k == 5) SK$blue else SK$grey
  if (k == 5) sk_hachure(c(xk[keep], rev(xk[keep])),
                         c(yk[keep], rep(1, sum(keep))), col = SK$blue_l)
  lines(xk[keep], yk[keep], col = col, lwd = if (k == 5) 6 else 3.5)
}
sk_arrow(8.6, 5.3, 6.9, 4.4, col = SK$blue, bend = 0.15)
sk_text(9.6, 5.5, "local", col = SK$blue, cex = 3)
sk_save()

# Slide: A penalty replaces the knot count {#sec-smooth-spline}
sk_open("smoothing-penalty", w = 12, h = 5)
set.seed(5)
for (side in 0:1) {
  x0 <- 0.4 + side * 6
  xs <- seq(x0 + 0.3, x0 + 5, length.out = 9)
  ys <- 2.4 + 0.18 * (xs - x0) + rnorm(9, 0, 0.55)
  sk_dots(xs, ys, col = SK$grey, solid = SK$grey, r = 0.11)
  if (side == 0) {
    sk_path(xs, ys, col = SK$red, lwd = 4)
    sk_text(x0 + 2.65, 0.5, "small λ", col = SK$red, cex = 3)
  } else {
    sk_line(xs[1], 2.4 + 0.18 * 0.3, xs[9], 2.4 + 0.18 * 5,
            col = SK$green, lwd = 5)
    sk_text(x0 + 2.65, 0.5, "large λ", col = SK$green, cex = 3)
  }
}
sk_save()

# Slide: The curve can rise twice {#sec-rcs-glucose}
sk_open("smoothing-two-rises", w = 12, h = 6)
sk_axes(0.8, 1, 11.4, 5.6, xlab = "x", cex = 2.6)
sk_curve(function(x) 1.6 + 3.2 * exp(-(x - 1) / 1.4) + 1.4 / (1 + exp(-(x - 9) * 1.6)),
         1.1, 11, col = SK$green, lwd = 6)
sk_line(1.1, 3.3, 5.5, 3.3, col = SK$red, lwd = 4)
sk_line(5.5, 1.8, 11, 1.8, col = SK$red, lwd = 4)
sk_line(5.5, 1.1, 5.5, 5.3, col = SK$grey, lwd = 2.5, lty = 2)
sk_text(5.5, 0.4, "cut", col = SK$red, cex = 2.6)
sk_arrow(9.4, 4.9, 10.1, 3.4, col = SK$green, bend = -0.2)
sk_text(8.9, 5.3, "2nd rise", col = SK$green, cex = 2.6)
sk_save()

# Slide: A neighbourhood empties as dimension grows {#sec-curse}
sk_open("smoothing-curse", w = 12, h = 6)
set.seed(11)
# p = 1: a short window holds several points
sk_line(0.6, 3, 4.6, 3, lwd = 3)
x1 <- runif(14, 0.7, 4.5)
sk_dots(x1, rep(3, 14), col = SK$ink, r = 0.1)
sk_rect(2.1, 2.6, 3.1, 3.4, col = SK$green, lwd = 3.5)
sk_text(2.6, 1.6, "p = 1", cex = 3)
# p = 2: the same share of the range holds almost nothing
sk_rect(6.4, 1, 11.4, 5.4, lwd = 3)
x2 <- runif(14, 6.6, 11.2); y2 <- runif(14, 1.2, 5.2)
out <- !(x2 > 8.2 & x2 < 9.6 & y2 > 2.5 & y2 < 3.9)
sk_dots(x2[out], y2[out], col = SK$ink, r = 0.1)
sk_rect(8.4, 2.7, 9.4, 3.7, col = SK$red, lwd = 3.5)
sk_text(8.9, 0.45, "p = 2", cex = 3)
sk_text(8.9, 5.75, "empty", cex = 2.8, col = SK$red)
sk_save()

# Slide: Four controls on the flexibility {#sec-smooth-takeaways}
sk_open("smoothing-knobs", w = 12, h = 5)
dial <- function(x, lab, col) {
  sk_circle(x, 2.7, 1.25, col = col, lwd = 5, fill = NA)
  ang <- pi * 0.8
  sk_line(x, 2.7, x + 0.95 * cos(ang), 2.7 + 0.95 * sin(ang), col = col,
          lwd = 6)
  sk_dots(x, 2.7, col = col, r = 0.12)
  sk_text(x, 0.6, lab, col = col, cex = 3.2)
}
dial(2, "h", SK$blue)
dial(6, "λ", SK$orange)
dial(10, "K", SK$violet)
sk_save()

# ------------------------------------------------------------------- design

# Slide: A trial randomises the assignment {#sec-rct}
sk_open("design-rct")
sk_circle(6, 6.3, 0.9, col = SK$orange, lwd = 5, fill = SK$orange_l)
sk_text(6, 6.3, "?", cex = 3.5)
sk_arrow(5.1, 5.8, 3, 4, col = SK$ink, bend = 0.1)
sk_arrow(6.9, 5.8, 9, 4, col = SK$ink, bend = -0.1)
for (x in c(1.6, 2.8, 4)) sk_person(x, 1.2, col = SK$blue, scale = 1.2)
for (x in c(8, 9.2, 10.4)) sk_person(x, 1.2, col = SK$grey, scale = 1.2)
sk_text(2.8, 0.5, "treated", col = SK$blue, cex = 2.8)
sk_text(9.2, 0.5, "control", col = SK$grey, cex = 2.8)
sk_save()

# Slide: Twenty weighings are not ten people {#sec-tech-bio}
sk_open("design-tech-bio", w = 12, h = 6)
sk_person(2.2, 1.6, col = SK$red, scale = 1.5)
sk_rect(1.3, 1.1, 3.1, 1.6, col = SK$ink, lwd = 3, solid = SK$grey_l)
sk_text(4.1, 3.6, "×20", col = SK$red, cex = 3.4)
sk_text(2.6, 0.5, "n = 1", col = SK$red, cex = 3)
sk_line(5.4, 0.6, 5.4, 5.4, col = SK$grey, lwd = 2.5, lty = 2)
for (k in 0:9) {
  x <- 6.4 + (k %% 5) * 1.15
  y <- if (k < 5) 3.3 else 1.1
  sk_person(x, y, col = SK$green, scale = 0.85)
}
sk_text(8.7, 0.5, "n = 10", col = SK$green, cex = 3)
sk_save()

# Slide: Balance makes each effect identifiable {#sec-balanced}
sk_open("design-balanced", w = 12, h = 6)
set.seed(3)
grid_cells <- function(x0, counts, col) {
  for (r in 0:1) for (c in 0:1) {
    xa <- x0 + c * 2.2; ya <- 1.2 + r * 2.0
    sk_rect(xa, ya, xa + 2.2, ya + 2.0, lwd = 3)
    n <- counts[r * 2 + c + 1]
    if (n > 0) sk_dots(xa + runif(n, 0.4, 1.8), ya + runif(n, 0.4, 1.6),
                       col = col, r = 0.14)
  }
}
grid_cells(0.8, c(3, 3, 3, 3), SK$green)
grid_cells(7, c(6, 1, 0, 4), SK$red)
sk_text(3, 0.5, "balanced ✓", col = SK$green, cex = 2.8)
sk_text(9.2, 0.5, "unbalanced", col = SK$red, cex = 2.8)
sk_save()

# Slide: Treatment contrasts use one reference {#sec-contr-treatment}
sk_open("design-contr-treatment", w = 12, h = 6)
m <- c(3.6, 2.2, 4.6, 2.9)
xs <- c(2, 4.6, 7.2, 9.8)
sk_line(1, m[1], 11, m[1], col = SK$grey, lwd = 3, lty = 2)
sk_text(11.3, m[1], "ref", col = SK$grey, cex = 2.6, adj = c(0, 0.5))
sk_dots(xs[1], m[1], col = SK$ink, r = 0.2)
for (k in 2:4) {
  sk_arrow(xs[k], m[1], xs[k], m[k] - sign(m[k] - m[1]) * 0.3,
           col = SK$blue, lwd = 5, head = 0.25)
  sk_dots(xs[k], m[k], col = SK$ink, r = 0.2)
  sk_text(xs[k] + 0.55, (m[1] + m[k]) / 2, paste0("β", k - 1),
          col = SK$blue, cex = 2.6, adj = c(0, 0.5))
}
for (k in 1:4) sk_text(xs[k], 0.7, c("I", "II", "III", "IV")[k], cex = 3)
sk_save()

# Slide: Eight minutes: which contrast? {#sec-design-contr-write}
sk_open("design-contr-choice", w = 12, h = 5)
panel_means <- function(x0, m, ref, col, lab, trend = FALSE) {
  xs <- x0 + c(0.4, 1.2, 2.0, 2.8)
  if (trend) {
    sk_line(xs[1], m[1], xs[4], m[4], col = col, lwd = 5)
  } else {
    sk_line(x0, ref, x0 + 3.2, ref, col = SK$grey, lwd = 2.5, lty = 2)
    for (k in seq_along(xs)) {
      if (abs(m[k] - ref) > 0.35) {
        sk_arrow(xs[k], ref, xs[k], m[k] - sign(m[k] - ref) * 0.25,
                 col = col, lwd = 4, head = 0.2)
      }
    }
  }
  sk_dots(xs, m, col = SK$ink, r = 0.15)
  sk_text(x0 + 1.6, 0.55, lab, col = col, cex = 2.8)
}
panel_means(0.4, c(2.6, 1.6, 3.8, 3.0), 2.6, SK$blue, "baseline")
panel_means(4.4, c(2.6, 1.6, 3.8, 3.0), 2.75, SK$orange, "mean")
panel_means(8.4, c(1.6, 2.3, 3.1, 3.9), NA, SK$violet, "order",
            trend = TRUE)
sk_save()

# Slide: Do not bin the gradient you built {#sec-spruce-bins}
sk_open("design-spruce-bins", w = 12, h = 6)
sk_axes(0.8, 1, 5.6, 5.4)
xs <- seq(1.3, 5.1, length.out = 5)
sk_line(1.1, 1.6, 5.3, 4.6, col = SK$green, lwd = 5)
sk_dots(xs, 1.6 + (xs - 1.1) * 3 / 4.2 + c(.15, -.2, .2, -.1, .1),
        col = SK$ink, r = 0.17)
sk_text(3.2, 0.4, "5 levels ✓", col = SK$green, cex = 2.8)
sk_rect(7.4, 1, 8.9, 2.6, col = SK$red, lwd = 4, fill = SK$red_l)
sk_rect(9.6, 1, 11.1, 4.2, col = SK$red, lwd = 4, fill = SK$red_l)
sk_text(8.15, 3.1, "low", cex = 2.6)
sk_text(10.35, 4.7, "high", cex = 2.6)
sk_text(9.25, 0.4, "2 bins ✗", col = SK$red, cex = 2.8)
sk_save()

# Slide: A fraction spends the interactions {#sec-doe-fraction}
sk_open("design-fraction", w = 10, h = 8)
P <- rbind(c(2, 1.4), c(6.2, 1.4), c(6.2, 5.6), c(2, 5.6))
off <- c(1.9, 1.3)
Q <- sweep(P, 2, off, "+")
for (k in 1:4) {
  k2 <- k %% 4 + 1
  sk_line(P[k, 1], P[k, 2], P[k2, 1], P[k2, 2], lwd = 3)
  sk_line(Q[k, 1], Q[k, 2], Q[k2, 1], Q[k2, 2], lwd = 3)
  sk_line(P[k, 1], P[k, 2], Q[k, 1], Q[k, 2], lwd = 3)
}
# half fraction: alternate corners
on_p <- c(1, 3); on_q <- c(2, 4)
for (k in 1:4) {
  sk_circle(P[k, 1], P[k, 2], 0.32, lwd = 3,
            solid = if (k %in% on_p) SK$blue else SK$paper,
            col = if (k %in% on_p) SK$blue else SK$grey)
  sk_circle(Q[k, 1], Q[k, 2], 0.32, lwd = 3,
            solid = if (k %in% on_q) SK$blue else SK$paper,
            col = if (k %in% on_q) SK$blue else SK$grey)
}
sk_text(5, 0.45, "4 of 8 runs", col = SK$blue, cex = 3)
sk_save()

# Slide: An optimal design is computed for the constraint {#sec-doe-optimal}
sk_open("design-optimal")
th <- seq(0, 2 * pi, length.out = 120)
rx <- 6 + (3.6 + 0.9 * sin(2 * th)) * cos(th)
ry <- 4.1 + (2.6 + 0.5 * cos(3 * th)) * sin(th)
polygon(rx, ry, col = SK$green_l, border = NA)
sk_path(rx, ry, col = SK$green, lwd = 4)
# textbook square corners, two fall outside the region
sq <- rbind(c(2.4, 1.2), c(9.6, 1.2), c(9.6, 7), c(2.4, 7))
sk_path(c(sq[, 1], sq[1, 1]), c(sq[, 2], sq[1, 2]), col = SK$grey, lwd = 2.5,
        lty = 2)
for (k in 1:4) sk_text(sq[k, 1], sq[k, 2], "✗", col = SK$red, cex = 3.4)
# computed points on the edge of the region
idx <- c(5, 28, 45, 62, 80, 100)
sk_dots(rx[idx] * 0.93 + 6 * 0.07, ry[idx] * 0.93 + 4.1 * 0.07,
        col = SK$ink, r = 0.2)
sk_dots(6, 4.1, col = SK$ink, r = 0.2)
sk_text(6, 0.4, "points fit the region", col = SK$green, cex = 2.6)
sk_save()

# Slide: One reading: the SPRUCE grid {#sec-design-doe-answer}
sk_open("design-spruce-grid", w = 12, h = 6)
lv <- c("+0", "+2.25", "+4.5", "+6.75", "+9")
for (c in 1:5) for (r in 1:2) {
  x0 <- 2.4 + (c - 1) * 1.95; y0 <- 1 + (r - 1) * 1.9
  sk_rect(x0, y0, x0 + 1.6, y0 + 1.5, col = SK$ink, lwd = 3,
          fill = if (r == 2) SK$blue_l else SK$orange_l)
}
for (c in 1:5) sk_text(3.2 + (c - 1) * 1.95, 5.1, lv[c], cex = 2.4)
sk_text(2.2, 1.75, "aCO₂", cex = 2.4, adj = c(1, 0.5), col = SK$orange)
sk_text(2.2, 3.65, "eCO₂", cex = 2.4, adj = c(1, 0.5), col = SK$blue)
sk_text(7, 0.4, "n = 10 enclosures", cex = 2.8, col = SK$green)
sk_text(7, 5.65, "°C", cex = 2.4)
sk_save()
