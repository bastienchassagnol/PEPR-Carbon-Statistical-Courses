# Sketches for sections 05 (causal), 07 (reproducibility) and 08 (close).
# Sourced by scripts/generate-sketches.R with sketch-toolkit.R loaded.
# Teaching illustrations, not data.

# Small helper: a labelled node (circle) for graph sketches.
.node <- function(x, y, label, col = SK$ink, r = 0.55, fill = NA,
                  cex = 2.8) {
  sk_circle(x, y, r, col = col, solid = fill)
  sk_text(x, y, label, cex = cex, col = col)
}

# Arrow between two node centres, shortened so it stops at the rims.
.edge <- function(x0, y0, x1, y1, r = 0.6, col = SK$ink, lwd = 4,
                  bend = 0) {
  d <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  ux <- (x1 - x0) / d; uy <- (y1 - y0) / d
  sk_arrow(x0 + ux * r, y0 + uy * r, x1 - ux * r, y1 - uy * r, col = col,
           lwd = lwd, bend = bend)
}

# ---------------------------------------------------------------------------
# 05 · "A coefficient is not an intervention"
sk_open("causal-assigned", w = 12, h = 6)
# left: assignment
sk_rect(0.8, 2.6, 2.2, 4.0, col = SK$ink, solid = "white")
sk_circle(1.2, 3.6, 0.1, solid = SK$ink, lwd = 1)
sk_circle(1.8, 3.0, 0.1, solid = SK$ink, lwd = 1)
sk_circle(1.5, 3.3, 0.1, solid = SK$ink, lwd = 1)
.node(3.6, 3.3, "x", col = SK$green)
.node(5.2, 3.3, "Y", col = SK$green)
sk_arrow(2.4, 3.3, 2.95, 3.3, col = SK$green)
.edge(3.6, 3.3, 5.2, 3.3, col = SK$green)
sk_text(3.0, 1.4, "assigned ✓", cex = 3, col = SK$green)
sk_line(6.1, 0.6, 6.1, 5.4, col = SK$grey, lwd = 2)
# right: observed, with a hidden common cause
.node(9.0, 4.6, "?", col = SK$red, fill = SK$red_l)
.node(7.8, 2.6, "x", col = SK$ink)
.node(10.4, 2.6, "Y", col = SK$ink)
.edge(9.0, 4.6, 7.8, 2.6, col = SK$red)
.edge(9.0, 4.6, 10.4, 2.6, col = SK$red)
.edge(7.8, 2.6, 10.4, 2.6, col = SK$ink)
sk_text(9.1, 1.0, "observed ✗", cex = 3, col = SK$red)
sk_save()

# 05 · "Two hospitals, one survival rate"
sk_open("causal-hospitals", w = 12, h = 6)
.hosp <- function(x0, label, rate, col) {
  sk_rect(x0, 1.0, x0 + 3.2, 4.2, col = col, fill = paste0(col, "55"))
  sk_rect(x0 + 1.1, 1.0, x0 + 2.1, 2.2, col = col, solid = SK$paper)
  sk_line(x0 + 1.6, 4.6, x0 + 1.6, 5.6, col = SK$red, lwd = 6)
  sk_line(x0 + 1.1, 5.1, x0 + 2.1, 5.1, col = SK$red, lwd = 6)
  sk_text(x0 + 1.6, 3.2, rate, cex = 3.4, font = 2)
  sk_text(x0 + 1.6, 0.4, label, cex = 3, font = 2)
}
.hosp(0.8, "A", "86%", SK$blue)
.hosp(8.0, "B", "79%", SK$grey)
sk_person(6.0, 1.2, scale = 1.2)
sk_arrow(6.6, 3.6, 4.2, 3.6, col = SK$blue, bend = -0.2)
sk_text(6.0, 4.8, "?", cex = 3.5, col = SK$blue, font = 2)
sk_save()

# 05 · "The explanatory target is do(x)"
sk_open("causal-see-do", w = 12, h = 6)
# see: an eye over a scatter
sk_curve(function(x) 4.9 + 0.35 * sin((x - 1.3) * pi / 1.6), 1.3, 2.9,
         lwd = 3)
sk_curve(function(x) 4.9 - 0.35 * sin((x - 1.3) * pi / 1.6), 1.3, 2.9,
         lwd = 3)
sk_circle(2.1, 4.9, 0.18, solid = SK$ink, lwd = 1)
sk_axes(0.8, 1.2, 4.6, 4.0)
sk_dots(c(1.4, 1.9, 2.3, 2.7, 3.1, 3.6, 4.0),
        c(1.7, 2.0, 2.4, 2.3, 2.9, 3.1, 3.5), col = SK$blue)
sk_text(2.7, 0.45, "see", cex = 3, col = SK$blue)
sk_line(6.0, 0.6, 6.0, 5.4, col = SK$grey, lwd = 2)
# do: a hand-set dial
sk_rect(7.2, 2.0, 10.8, 2.6, col = SK$ink, solid = "white")
sk_rect(8.9, 1.7, 9.5, 2.9, col = SK$orange, solid = SK$orange_l)
sk_arrow(8.0, 3.5, 9.1, 3.5, col = SK$orange)
sk_text(9.0, 4.6, "set x", cex = 3, col = SK$orange)
sk_text(9.0, 0.45, "do", cex = 3, col = SK$orange)
sk_save()

# 05 · "A moderator changes the path"
sk_open("causal-moderator", w = 10, h = 7)
sk_axes(1.0, 1.0, 9.4, 6.6, xlab = "x", ylab = "Y")
sk_line(1.4, 1.8, 8.4, 6.0, col = SK$orange, lwd = 5)
sk_line(1.4, 2.4, 8.4, 3.2, col = SK$blue, lwd = 5)
sk_text(8.6, 6.2, "Z = 1", cex = 2.8, col = SK$orange, adj = c(0, 0.5))
sk_text(8.6, 3.2, "Z = 0", cex = 2.8, col = SK$blue, adj = c(0, 0.5))
sk_save()

# 05 · "The simulated association matches only if the arrow is real"
sk_open("causal-sem-iff", w = 12, h = 7)
.mini <- function(x0, y0, slope, col) {
  sk_rect(x0, y0, x0 + 1.8, y0 + 1.6, col = SK$grey, lwd = 2)
  sk_line(x0 + 0.2, y0 + 0.8 - slope * 0.6, x0 + 1.6, y0 + 0.8 + slope * 0.6,
          col = col, lwd = 4)
}
# row 1: arrow real
.node(1.0, 5.2, "M", col = SK$green)
.node(3.6, 5.2, "m", col = SK$green)
.edge(1.0, 5.2, 3.6, 5.2, col = SK$green)
.mini(5.0, 4.4, 1, SK$ink)
sk_text(7.4, 5.2, "≈", cex = 3.5)
.mini(8.0, 4.4, 1, SK$ink)
sk_text(10.9, 5.2, "✓", cex = 3.5, col = SK$green)
# row 2: arrow absent
.node(1.0, 1.9, "M", col = SK$red)
.node(3.6, 1.9, "m", col = SK$red)
sk_line(1.7, 1.9, 2.9, 1.9, col = SK$red, lty = 2, lwd = 3)
sk_line(2.0, 1.5, 2.6, 2.3, col = SK$red, lwd = 4)
sk_line(2.0, 2.3, 2.6, 1.5, col = SK$red, lwd = 4)
.mini(5.0, 1.1, 1, SK$ink)
sk_text(7.4, 1.9, "≠", cex = 3.5)
.mini(8.0, 1.1, 0, SK$ink)
sk_text(10.9, 1.9, "✗", cex = 3.5, col = SK$red)
sk_text(5.9, 6.6, "silico", cex = 2.6, col = SK$blue)
sk_text(8.9, 6.6, "vivo", cex = 2.6, col = SK$blue)
sk_save()

# 05 · "Three graphs, one distribution"
sk_open("causal-cpdag", w = 12, h = 8)
.chain <- function(y, dir, col = SK$ink) {
  xs <- c(1.2, 3.4, 5.6)
  for (k in 1:3) .node(xs[k], y, c("X", "Z", "Y")[k], col = col, r = 0.45,
                       cex = 2.6)
  # dir: two signs, +1 = left to right
  if (dir[1] > 0) .edge(xs[1], y, xs[2], y, r = 0.5, col = col)
  else .edge(xs[2], y, xs[1], y, r = 0.5, col = col)
  if (dir[2] > 0) .edge(xs[2], y, xs[3], y, r = 0.5, col = col)
  else .edge(xs[3], y, xs[2], y, r = 0.5, col = col)
}
sk_rect(0.4, 1.5, 6.4, 7.6, col = SK$blue, lwd = 3)
.chain(6.6, c(1, 1))
.chain(4.6, c(-1, -1))
.chain(2.6, c(-1, 1))
sk_text(3.4, 0.8, "one class", cex = 2.8, col = SK$blue)
# the collider stands alone
xs <- c(7.4, 9.4, 11.4)
.node(7.4, 5.2, "X", col = SK$orange, r = 0.45, cex = 2.6)
.node(9.4, 3.4, "Z", col = SK$orange, r = 0.45, cex = 2.6)
.node(11.4, 5.2, "Y", col = SK$orange, r = 0.45, cex = 2.6)
.edge(7.4, 5.2, 9.4, 3.4, r = 0.5, col = SK$orange)
.edge(11.4, 5.2, 9.4, 3.4, r = 0.5, col = SK$orange)
sk_text(9.4, 1.6, "collider", cex = 2.8, col = SK$orange)
sk_save()

# 05 · "Time supplies the direction"
sk_open("causal-dbn", w = 10, h = 7)
labs <- c("X₁", "X₂", "Y")
ys <- c(5.6, 3.9, 2.2)
for (k in 1:3) {
  .node(2.6, ys[k], labs[k], r = 0.6)
  .node(7.4, ys[k], labs[k], r = 0.6, col = if (k == 3) SK$green else SK$ink)
}
.edge(2.6, ys[1], 7.4, ys[3], r = 0.68, col = SK$blue)
.edge(2.6, ys[2], 7.4, ys[3], r = 0.68, col = SK$blue)
sk_arrow(1.4, 0.9, 8.8, 0.9, col = SK$grey, lwd = 3)
sk_text(2.6, 0.35, "t", cex = 2.8)
sk_text(7.4, 0.35, "t + 1", cex = 2.8)
sk_save()

# 05 · "A searched graph is still a DAG"
sk_open("causal-no-cycle", w = 12, h = 6)
.node(1.6, 2.0, "X", col = SK$red)
.node(4.6, 2.0, "Y", col = SK$red)
.node(3.1, 4.5, "Z", col = SK$red)
.edge(1.6, 2.0, 4.6, 2.0, col = SK$red)
.edge(4.6, 2.0, 3.1, 4.5, col = SK$red)
.edge(3.1, 4.5, 1.6, 2.0, col = SK$red)
sk_text(3.1, 0.6, "loop ✗", cex = 3, col = SK$red)
sk_line(6.0, 0.6, 6.0, 5.4, col = SK$grey, lwd = 2)
.node(7.4, 2.0, "X", col = SK$green)
.node(10.4, 2.0, "Y", col = SK$green)
.node(8.9, 4.5, "Z", col = SK$green)
.edge(7.4, 2.0, 10.4, 2.0, col = SK$green)
.edge(8.9, 4.5, 10.4, 2.0, col = SK$green)
.edge(8.9, 4.5, 7.4, 2.0, col = SK$green)
sk_text(8.9, 0.6, "DAG ✓", cex = 3, col = SK$green)
sk_save()

# ---------------------------------------------------------------------------
# A source file with a folded corner, for the reproducibility sketches.
.file <- function(x0, y0, w, h, label, col = SK$ink, fill = SK$blue_l,
                  cex = 2.8) {
  f <- 0.35 * min(w, h)
  polygon(c(x0, x0 + w - f, x0 + w, x0 + w, x0),
          c(y0 + h, y0 + h, y0 + h - f, y0, y0), col = fill, border = NA)
  sk_path(c(x0, x0 + w - f, x0 + w, x0 + w, x0, x0),
          c(y0 + h, y0 + h, y0 + h - f, y0, y0, y0 + h), col = col, lwd = 3)
  sk_path(c(x0 + w - f, x0 + w - f, x0 + w),
          c(y0 + h, y0 + h - f, y0 + h - f), col = col, lwd = 2.5)
  sk_text(x0 + w / 2, y0 + h / 2 - f / 4, label, cex = cex, col = col)
}

# 07 · "One source, several outputs"
sk_open("repro-outputs", w = 12, h = 7)
.file(0.6, 2.4, 2.6, 2.4, ".qmd", fill = SK$blue_l, cex = 3.2)
outs <- c("HTML", "PDF", "Word", "slides")
ys <- c(6.0, 4.3, 2.6, 0.9)
for (k in 1:4) {
  sk_arrow(3.4, 3.6, 7.6, ys[k] , col = SK$blue, bend = 0)
  sk_note(9.7, ys[k], outs[k], w = 3.4, h = 1.2, fill = SK$green_l, cex = 2.8)
}
sk_save()

# 07 · "A parameter is an input of the document"
sk_open("repro-params", w = 12, h = 7)
.file(0.6, 2.2, 3.0, 2.8, ".qmd", fill = SK$blue_l, cex = 3.2)
sk_note(2.1, 6.0, "params", w = 2.8, h = 1.1, fill = SK$orange_l)
sk_arrow(2.1, 5.4, 2.1, 5.05, col = SK$orange, head = 0.2)
sites <- c("SD1", "SD2", "SD3")
ys <- c(5.6, 3.6, 1.6)
for (k in 1:3) {
  sk_arrow(3.8, 3.6, 7.4, ys[k], col = SK$blue)
  .file(7.8, ys[k] - 0.8, 2.6, 1.6, sites[k], fill = SK$green_l, cex = 2.8)
}
sk_save()

# 07 · "Say which layer you are shipping"
sk_open("repro-layers", w = 12, h = 7)
labs <- c("data", "processing", "presentation")
fills <- c(SK$green_l, SK$orange_l, SK$blue_l)
for (k in 1:3) {
  y0 <- 0.5 + (k - 1) * 1.9
  sk_rect(1.0, y0, 9.0, y0 + 1.5, col = SK$ink, solid = fills[k], lwd = 3)
  sk_text(5.0, y0 + 0.75, labs[k], cex = 3)
}
# a cursor clicking the top layer
sk_path(c(10.0, 10.0, 10.35, 10.6, 10.85, 10.6, 11.0, 10.0),
        c(6.6, 5.0, 5.35, 4.8, 4.9, 5.45, 5.45, 6.6), col = SK$ink, lwd = 3)
sk_arrow(10.3, 4.3, 10.3, 1.4, col = SK$grey, lwd = 3)
sk_save()

# ---------------------------------------------------------------------------
# 08 · "What to open next · the response"
sk_open("close-next", w = 10, h = 7)
sk_line(5.0, 0.4, 5.0, 6.4, lwd = 6)
.sign <- function(y, label, dir, col) {
  if (dir > 0) {
    sk_path(c(5.2, 8.6, 9.3, 8.6, 5.2, 5.2),
            c(y + 0.5, y + 0.5, y, y - 0.5, y - 0.5, y + 0.5), col = col,
            lwd = 3)
    sk_text(6.9, y, label, cex = 2.6, col = col)
  } else {
    sk_path(c(4.8, 1.4, 0.7, 1.4, 4.8, 4.8),
            c(y + 0.5, y + 0.5, y, y - 0.5, y - 0.5, y + 0.5), col = col,
            lwd = 3)
    sk_text(3.1, y, label, cex = 2.6, col = col)
  }
}
.sign(5.7, "zeros", 1, SK$blue)
.sign(4.4, "log-ratios", -1, SK$green)
.sign(3.1, "GAMs", 1, SK$orange)
.sign(1.8, "residuals", -1, SK$violet)
sk_save()

# 08 · "The mean can stay; the probability statement changes"
sk_open("close-bayes-update", w = 12, h = 7)
sk_axes(0.6, 1.0, 11.6, 6.6, xlab = "β")
sk_bump(4.0, 2.2, 1.6, 1.0, 0.8, 11.2, col = SK$grey, lwd = 4)
sk_bump(7.6, 0.8, 3.4, 1.0, 4.6, 10.8, col = SK$blue, lwd = 4)
sk_bump(6.9, 0.7, 4.6, 1.0, 4.4, 9.6, col = SK$green, lwd = 5)
sk_text(1.6, 3.2, "prior", cex = 2.8, col = SK$grey, adj = c(0, 0.5))
sk_text(9.0, 4.6, "likelihood", cex = 2.8, col = SK$blue, adj = c(0, 0.5))
sk_text(6.4, 6.3, "posterior", cex = 2.8, col = SK$green, adj = c(1, 0.5))
sk_save()

# 08 · "What you are allowed to say"
sk_open("close-wald-vs-posterior", w = 12, h = 7)
# left: repeated experiments, one interval each
sk_line(3.0, 0.8, 3.0, 6.2, col = SK$ink, lwd = 2, lty = 2)
cent <- c(2.9, 3.4, 2.6, 3.2, 4.4, 2.8, 3.1)
for (k in seq_along(cent)) {
  y <- 0.9 + (k - 1) * 0.75
  col <- if (abs(cent[k] - 3) > 1.1) SK$red else SK$blue
  sk_line(cent[k] - 1.0, y, cent[k] + 1.0, y, col = col, lwd = 4)
  sk_dots(cent[k], y, col = col, r = 0.1)
}
sk_text(3.0, 6.6, "repeat", cex = 2.8, col = SK$blue)
sk_line(6.0, 0.4, 6.0, 6.6, col = SK$grey, lwd = 2)
# right: one posterior for this sample
xs <- seq(7.6, 10.4, length.out = 40)
polygon(c(xs, rev(xs)),
        c(1.0 + 4 * exp(-0.5 * ((xs - 9) / 0.9)^2), rep(1.0, 40)),
        col = SK$green_l, border = NA)
sk_bump(9.0, 0.9, 4.0, 1.0, 6.6, 11.4, col = SK$green, lwd = 5)
sk_line(6.6, 1.0, 11.4, 1.0, lwd = 3)
sk_text(9.0, 6.6, "this sample", cex = 2.8, col = SK$green)
sk_save()
