# Hand-drawn (Excalidraw-like) primitives for the slide illustrations.
# Sourced by generate-sketches.R and by every file in scripts/sketches/.
# Base graphics on a fixed 12 x 8 canvas; every stroke is drawn twice with
# a small random wobble, as rough.js does. Seeded per figure.

library(ragg)

SK <- list(
  ink    = "#1e1e1e",
  red    = "#e03131",
  blue   = "#1971c2",
  green  = "#2f9e44",
  orange = "#f08c00",
  violet = "#6741d9",
  grey   = "#868e96",
  # light fills for hachure or solid backgrounds
  red_l    = "#ffc9c9",
  blue_l   = "#a5d8ff",
  green_l  = "#b2f2bb",
  orange_l = "#ffec99",
  violet_l = "#d0bfff",
  grey_l   = "#e9ecef",
  paper    = "#fbf8f2",
  font     = "Comic Neue"
)

# Open a canvas. Coordinates run 0..12 horizontally and 0..8 vertically
# (or 0..w, 0..h if given). Call sk_save() when done.
sk_open <- function(name, w = 12, h = 8, seed = 20261005,
                    dir = "figures/generated") {
  set.seed(seed + sum(utf8ToInt(name)))
  path <- file.path(dir, paste0("sketch-", name, ".png"))
  ragg::agg_png(path, width = w * 100, height = h * 100, res = 100,
                background = SK$paper)
  par(mar = c(0, 0, 0, 0), family = SK$font, lend = "round",
      ljoin = "round")
  plot.new()
  plot.window(xlim = c(0, w), ylim = c(0, h), xaxs = "i", yaxs = "i",
              asp = 1)
  invisible(path)
}

sk_save <- function() invisible(dev.off())

# Wobbly polyline through points (x, y). Each segment bows slightly.
.wobble <- function(x, y, rough = 1) {
  out_x <- numeric(0)
  out_y <- numeric(0)
  for (k in seq_len(length(x) - 1)) {
    x0 <- x[k]; y0 <- y[k]; x1 <- x[k + 1]; y1 <- y[k + 1]
    len <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
    n <- max(4, ceiling(len * 6))
    t <- seq(0, 1, length.out = n)
    # perpendicular unit vector
    px <- -(y1 - y0) / max(len, 1e-9)
    py <- (x1 - x0) / max(len, 1e-9)
    bow <- rnorm(1, 0, 0.012 * rough * min(len, 6))
    off <- bow * sin(pi * t) + rnorm(n, 0, 0.004 * rough)
    # small overshoot at the ends, as a pen does
    ts <- t * (1 + rnorm(1, 0, 0.01 * rough)) - abs(rnorm(1, 0, 0.006 * rough))
    out_x <- c(out_x, x0 + (x1 - x0) * ts + px * off)
    out_y <- c(out_y, y0 + (y1 - y0) * ts + py * off)
  }
  list(x = out_x, y = out_y)
}

# Polyline through points, drawn twice for the rough look.
sk_path <- function(x, y, col = SK$ink, lwd = 4, rough = 1, double = TRUE,
                    lty = 1) {
  for (pass in seq_len(if (double) 2 else 1)) {
    w <- .wobble(x, y, rough)
    lines(w$x, w$y, col = col, lwd = if (pass == 1) lwd else lwd * 0.6,
          lty = lty)
  }
}

sk_line <- function(x0, y0, x1, y1, ...) sk_path(c(x0, x1), c(y0, y1), ...)

# Smooth curve y = f(x) on [from, to], hand-drawn.
sk_curve <- function(f, from, to, n = 60, col = SK$ink, lwd = 4, rough = 1,
                     ...) {
  x <- seq(from, to, length.out = n)
  y <- f(x)
  for (pass in 1:2) {
    j <- rnorm(1, 0, 0.02 * rough)
    lines(x, y + j + cumsum(rnorm(n, 0, 0.002 * rough)), col = col,
          lwd = if (pass == 1) lwd else lwd * 0.6, ...)
  }
}

# Arrow from (x0, y0) to (x1, y1), optionally bent by `bend` (fraction of
# the length, positive bends left).
sk_arrow <- function(x0, y0, x1, y1, col = SK$ink, lwd = 4, bend = 0,
                     head = 0.28, both = FALSE) {
  t <- seq(0, 1, length.out = 30)
  len <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  px <- -(y1 - y0) / len
  py <- (x1 - x0) / len
  x <- x0 + (x1 - x0) * t + px * bend * len * sin(pi * t)
  y <- y0 + (y1 - y0) * t + py * bend * len * sin(pi * t)
  for (pass in 1:2) {
    lines(x + rnorm(30, 0, 0.004), y + rnorm(30, 0, 0.004), col = col,
          lwd = if (pass == 1) lwd else lwd * 0.6)
  }
  .head <- function(xa, ya, xb, yb) {
    ang <- atan2(yb - ya, xb - xa)
    for (s in c(-1, 1)) {
      a <- ang + pi + s * 0.45
      sk_line(xb, yb, xb + head * cos(a), yb + head * sin(a), col = col,
              lwd = lwd, rough = 0.5)
    }
  }
  n <- length(x)
  .head(x[n - 3], y[n - 3], x[n], y[n])
  if (both) .head(x[4], y[4], x[1], y[1])
}

# Hachure fill of a polygon: parallel strokes clipped to the shape.
sk_hachure <- function(x, y, col = SK$blue, gap = 0.16, angle = -41,
                       lwd = 2.2) {
  a <- angle * pi / 180
  # rotate so hachure lines are horizontal
  rx <- x * cos(-a) - y * sin(-a)
  ry <- x * sin(-a) + y * cos(-a)
  n <- length(rx)
  for (h in seq(min(ry) + gap / 2, max(ry), by = gap)) {
    xs <- numeric(0)
    for (k in seq_len(n)) {
      k2 <- if (k == n) 1 else k + 1
      y1 <- ry[k]; y2 <- ry[k2]
      if ((y1 <= h && y2 > h) || (y2 <= h && y1 > h)) {
        xs <- c(xs, rx[k] + (h - y1) / (y2 - y1) * (rx[k2] - rx[k]))
      }
    }
    xs <- sort(xs)
    if (length(xs) < 2) next
    for (m in seq(1, length(xs) - 1, by = 2)) {
      ux <- c(xs[m], xs[m + 1]) + rnorm(2, 0, 0.02)
      uy <- c(h, h) + rnorm(2, 0, 0.01)
      lines(ux * cos(a) - uy * sin(a), ux * sin(a) + uy * cos(a),
            col = col, lwd = lwd)
    }
  }
}

# Rectangle; fill = colour for hachure, or solid = colour for a flat fill.
sk_rect <- function(x0, y0, x1, y1, col = SK$ink, lwd = 4, fill = NA,
                    solid = NA, rough = 1) {
  if (!is.na(solid)) {
    rect(x0, y0, x1, y1, col = solid, border = NA)
  }
  if (!is.na(fill)) {
    sk_hachure(c(x0, x1, x1, x0), c(y0, y0, y1, y1), col = fill)
  }
  sk_path(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0), col = col,
          lwd = lwd, rough = rough)
}

# Ellipse (circle when rx == ry). The pen does not quite close, as by hand.
sk_circle <- function(x, y, r, ry = r, col = SK$ink, lwd = 4, fill = NA,
                      solid = NA) {
  th <- seq(0, 2 * pi, length.out = 80)
  if (!is.na(solid)) {
    polygon(x + r * cos(th), y + ry * sin(th), col = solid, border = NA)
  }
  if (!is.na(fill)) {
    sk_hachure(x + r * cos(th), y + ry * sin(th), col = fill)
  }
  for (pass in 1:2) {
    start <- runif(1, 0, 2 * pi)
    tt <- seq(start, start + 2 * pi + runif(1, 0.05, 0.3), length.out = 90)
    jr <- 1 + rnorm(1, 0, 0.025) + 0.02 * sin(tt * 2 + runif(1, 0, 6))
    lines(x + r * jr * cos(tt), y + ry * jr * sin(tt), col = col,
          lwd = if (pass == 1) lwd else lwd * 0.6)
  }
}

# Hand lettering. `cex` 2 is a good body size on this canvas.
sk_text <- function(x, y, label, cex = 3, col = SK$ink, adj = 0.5,
                    font = 1, srt = 0) {
  text(x, y, label, cex = cex, col = col, adj = adj, font = font,
       family = SK$font, srt = srt)
}

# A pair of hand-drawn axes with optional labels.
sk_axes <- function(x0, y0, x1, y1, xlab = NULL, ylab = NULL, cex = 2.6,
                    col = SK$ink) {
  sk_arrow(x0, y0, x1, y0, col = col, head = 0.22)
  sk_arrow(x0, y0, x0, y1, col = col, head = 0.22)
  if (!is.null(xlab)) sk_text(x1, y0 - 0.4, xlab, cex = cex, adj = c(1, 0.5))
  if (!is.null(ylab)) sk_text(x0 - 0.2, y1, ylab, cex = cex, adj = c(1, 0.5))
}

# Scatter of small hand-drawn dots.
sk_dots <- function(x, y, col = SK$ink, r = 0.09, solid = col) {
  for (k in seq_along(x)) {
    sk_circle(x[k], y[k], r, col = col, lwd = 1.5, solid = solid)
  }
}

# Stick figure: head at (x, y + 1.2), height about 1.8 units.
sk_person <- function(x, y, col = SK$ink, scale = 1, arms_up = FALSE) {
  s <- scale
  sk_circle(x, y + 1.45 * s, 0.28 * s, col = col)
  sk_line(x, y + 1.17 * s, x, y + 0.5 * s, col = col)
  sk_line(x, y + 0.5 * s, x - 0.3 * s, y, col = col)
  sk_line(x, y + 0.5 * s, x + 0.3 * s, y, col = col)
  if (arms_up) {
    sk_line(x, y + 0.95 * s, x - 0.4 * s, y + 1.4 * s, col = col)
    sk_line(x, y + 0.95 * s, x + 0.4 * s, y + 1.4 * s, col = col)
  } else {
    sk_line(x - 0.4 * s, y + 0.8 * s, x + 0.4 * s, y + 0.8 * s, col = col)
  }
}

# Sticky-note style box with centred text.
sk_note <- function(x, y, label, w = 3, h = 1.2, fill = SK$orange_l,
                    cex = 2.6, col = SK$ink) {
  sk_rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2, solid = fill,
          col = col, lwd = 2.5)
  sk_text(x, y, label, cex = cex, col = col)
}

# Gaussian-like bump, handy for densities.
sk_bump <- function(mu, sd, height, base, from, to, ...) {
  sk_curve(function(x) base + height * exp(-0.5 * ((x - mu) / sd)^2),
           from, to, ...)
}
