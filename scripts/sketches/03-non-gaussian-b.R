# Sketches for sections/03-non-gaussian.qmd, part B (from the GLM skeleton
# to the end of the section). Teaching illustrations, not data.

logistic <- function(z) 1 / (1 + exp(-z))

# Slide: Hat values and Cook's distance, again
sk_open("nongauss-b-leverage")
sk_axes(1, 1, 11.4, 7.4, xlab = "x", ylab = "")
xs <- c(1.8, 2.3, 2.7, 3.2, 3.6, 4.1, 4.5, 5.0)
ys <- 1.9 + 0.35 * (xs - 1.8) + c(0.3, -0.2, 0.25, -0.3, 0.1, 0.3, -0.25, 0.05)
sk_dots(xs, ys, r = 0.13)
sk_line(1.4, 1.75, 11, 1.75 + 0.35 * 9.2, col = SK$green, lwd = 4, lty = 2)
sk_line(1.4, 1.45, 11, 1.45 + 0.52 * 9.6, col = SK$red, lwd = 4)
sk_circle(10.2, 6.4, 0.28, col = SK$red, solid = SK$red_l, lwd = 4)
sk_text(10.0, 7.35, "high h", cex = 3, col = SK$red)
sk_text(7.6, 2.55, "without it", cex = 2.6, col = SK$green)
sk_arrow(8.6, 5.2, 9.7, 6.0, col = SK$red, lwd = 3, bend = 0.15)
sk_text(7.3, 5.15, "pulls", cex = 2.8, col = SK$red)
sk_save()

# Slide: Deviance is the residual sum of squares
sk_open("nongauss-b-deviance")
sk_arrow(0.8, 3.6, 11.4, 3.6, head = 0.25)
sk_text(11.4, 6.3, "log-likelihood →", cex = 2.4, adj = c(1, 0.5), col = SK$grey)
px <- c(null = 1.8, fit = 6.6, sat = 10.2)
cols <- c(SK$grey, SK$blue, SK$green)
lab <- c("null", "fitted", "saturated")
for (k in 1:3) {
  sk_circle(px[k], 3.6, 0.22, col = cols[k], solid = cols[k])
  sk_text(px[k], 2.4, lab[k], cex = 2.8, col = cols[k])
}
sk_arrow(px[1], 5.0, px[3], 5.0, col = SK$ink, both = TRUE, head = 0.25)
sk_text(mean(px[c(1, 3)]), 5.6, "D₀", cex = 3.4)
sk_arrow(px[2], 4.3, px[3], 4.3, col = SK$blue, both = TRUE, head = 0.22)
sk_text(mean(px[2:3]), 4.75, "D", cex = 3.2, col = SK$blue)
sk_text(6, 7.1, "D = 2 (ℓsat − ℓ)", cex = 3.2)
sk_text(6, 1.1, "small D = close to the data", cex = 2.6, col = SK$grey)
sk_save()

# Slide: Logit · odds move by a factor, probabilities do not
sk_open("nongauss-b-odds-steps")
fx <- function(x) 1 + 5.8 * logistic(1.3 * (x - 6.5))
sk_axes(1, 1, 11.5, 7.5, xlab = "x", ylab = "π")
sk_curve(fx, 1.2, 11.3, col = SK$ink, lwd = 5)
# step in the middle
for (st in list(c(6.0, 7.0, SK$blue), c(9.4, 10.4, SK$orange))) {
  a <- as.numeric(st[1]); b <- as.numeric(st[2]); cl <- st[3]
  sk_line(a, fx(a), b, fx(a), col = cl, lwd = 4)
  sk_line(b, fx(a), b, fx(b), col = cl, lwd = 7)
}
sk_text(4.6, 5.6, "Δπ big", cex = 3, col = SK$blue)
sk_arrow(5.3, 5.0, 6.8, 4.5, col = SK$blue, lwd = 3, bend = 0.2)
sk_text(9.0, 3.6, "Δπ small", cex = 3, col = SK$orange)
sk_arrow(9.6, 4.2, 10.3, 6.4, col = SK$orange, lwd = 3, bend = -0.2)
sk_text(3.4, 7.3, "same × odds", cex = 3)
sk_save()

# Slide: The fitting criterion is cross-entropy
sk_open("nongauss-b-logloss")
X <- function(p) 1.2 + 9.6 * p
Yc <- function(l) 1.2 + 1.9 * l
sk_axes(1.2, 1.2, 11.4, 7.6, xlab = "π̂", ylab = "")
sk_text(1.0, 7.3, "cost", cex = 2.6, adj = c(1, 0.5))
sk_curve(function(x) Yc(-log((x - 1.2) / 9.6)), X(0.04), X(1), col = SK$ink,
         lwd = 5)
sk_line(X(0.19), 1.2, X(0.19), Yc(-log(0.19)), col = SK$red, lwd = 3, lty = 2)
sk_line(X(0.79), 1.2, X(0.79), Yc(-log(0.79)), col = SK$green, lwd = 3,
        lty = 2)
sk_circle(X(0.19), Yc(-log(0.19)), 0.2, col = SK$red, solid = SK$red)
sk_circle(X(0.79), Yc(-log(0.79)), 0.2, col = SK$green, solid = SK$green)
sk_text(X(0.19) + 0.5, Yc(-log(0.19)) + 0.35, "1.66", cex = 3, col = SK$red,
        adj = c(0, 0.5))
sk_text(X(0.79), Yc(-log(0.79)) + 0.9, "0.23", cex = 3, col = SK$green)
sk_text(8.6, 6.6, "y = 1", cex = 3.2, col = SK$blue)
sk_save()

# Slide: One-hot coding is a contrast with a reference
sk_open("nongauss-b-onehot")
L <- function(v) 1.0 + (v + 3.6) * 2.5
sk_arrow(0.6, 3.6, 11.5, 3.6, head = 0.25)
sk_text(11.4, 7.2, "log-odds \u2192", cex = 2.4, adj = c(1, 0.5), col = SK$grey)
sk_line(L(0), 3.3, L(0), 3.9, lwd = 4)
sk_text(L(0), 2.7, "0", cex = 2.8, col = SK$grey)
sk_circle(L(-3.22), 3.6, 0.26, col = SK$blue, solid = SK$blue)
sk_text(L(-3.22), 2.7, "reference", cex = 2.8, col = SK$blue)
sk_circle(L(-0.22), 3.6, 0.26, col = SK$orange, solid = SK$orange)
sk_text(L(-0.22) - 0.3, 4.4, "high", cex = 2.8, col = SK$orange)
sk_arrow(L(-3.22), 5.3, L(-0.22), 5.3, both = FALSE, head = 0.28,
         col = SK$orange)
sk_text(mean(L(c(-3.22, -0.22))), 6.1, "\u03b2\u2081 = contrast", cex = 3.2,
        col = SK$orange)
sk_arrow(L(0), 1.6, L(-3.22), 1.6, col = SK$blue, head = 0.25)
sk_text(mean(L(c(0, -3.22))), 0.85, "\u03b2\u2080 = reference level", cex = 2.8,
        col = SK$blue)
sk_save()

# Slide: A Wald interval is a large-sample interval for pi
sk_open("nongauss-b-wald-asym")
ex <- function(e) 2.0 + (e + 1.5) * 1.6      # logit -> canvas x
py <- function(p) 1.2 + 6.0 * p              # pi -> canvas y
sk_axes(2.0, 1.2, 11.5, 7.6, xlab = "logit", ylab = "π")
sk_curve(function(x) py(logistic((x - 2.0) / 1.6 - 1.5)), 2.1, 11.3,
         col = SK$ink, lwd = 5)
e0 <- 1.5; lo <- 0.3; hi <- 2.7
# symmetric bracket on the logit axis
sk_line(ex(lo), 0.75, ex(hi), 0.75, col = SK$blue, lwd = 6)
for (e in c(lo, e0, hi)) sk_line(ex(e), 0.55, ex(e), 0.95, col = SK$blue, lwd = 4)
for (e in c(lo, hi)) {
  sk_line(ex(e), 1.2, ex(e), py(logistic(e)), col = SK$grey, lwd = 2, lty = 2)
  sk_line(2.0, py(logistic(e)), ex(e), py(logistic(e)), col = SK$grey, lwd = 2,
          lty = 2)
}
# asymmetric bracket on the pi axis
sk_line(1.5, py(logistic(lo)), 1.5, py(logistic(hi)), col = SK$orange, lwd = 6)
for (e in c(lo, e0, hi)) {
  sk_line(1.3, py(logistic(e)), 1.7, py(logistic(e)), col = SK$orange, lwd = 4)
}
sk_text(9.4, 2.4, "symmetric", cex = 2.8, col = SK$blue)
sk_text(5.2, 7.35, "not symmetric", cex = 2.8, col = SK$orange)
sk_arrow(3.1, 7.35, 1.9, 6.3, col = SK$orange, lwd = 3, bend = 0.2)
sk_save()

# Slide: Hold the other covariates still
sk_open("nongauss-b-hold-still")
# slider x1: moving
sk_text(1.0, 5.6, "x₁", cex = 3.4, col = SK$blue)
sk_line(2.0, 5.6, 10.6, 5.6, lwd = 5, col = SK$grey)
sk_rect(4.0, 5.1, 4.6, 6.1, solid = SK$blue_l, col = SK$blue)
sk_arrow(4.9, 6.6, 7.4, 6.6, col = SK$blue, head = 0.25)
sk_rect(7.5, 5.1, 8.1, 6.1, col = SK$blue, lwd = 3)
sk_text(6.2, 7.35, "+1", cex = 3, col = SK$blue)
# slider x2: locked
sk_text(1.0, 2.8, "x₂", cex = 3.4, col = SK$ink)
sk_line(2.0, 2.8, 10.6, 2.8, lwd = 5, col = SK$grey)
sk_rect(6.0, 2.3, 6.6, 3.3, solid = SK$grey_l, col = SK$ink)
# padlock
sk_rect(6.8, 3.4, 7.8, 4.2, solid = SK$orange_l, col = SK$ink, lwd = 3)
th <- seq(0, pi, length.out = 30)
sk_path(7.3 + 0.32 * cos(th), 4.2 + 0.4 * sin(th), lwd = 3)
sk_text(6.3, 1.6, "held still", cex = 3, col = SK$ink)
sk_save()

# Slide: A row of coefficients is a family
sk_open("nongauss-b-family")
sk_line(5.5, 0.6, 5.5, 7.6, col = SK$grey, lwd = 3, lty = 2)
ctr <- c(5.2, 5.9, 4.8, 6.1, 5.6, 5.1, 7.6, 5.7)
for (k in seq_along(ctr)) {
  yk <- 7.2 - (k - 1) * 0.85
  if (k == 7) {
    sk_line(ctr[k] - 2.6, yk - 0.25, ctr[k] + 2.6, yk - 0.25, col = SK$green, lwd = 4,
            lty = 2)
    sk_line(ctr[k] - 1.7, yk, ctr[k] + 1.7, yk, col = SK$red, lwd = 6)
    sk_circle(ctr[k], yk, 0.16, col = SK$red, solid = SK$red)
  } else {
    sk_line(ctr[k] - 1.7, yk, ctr[k] + 1.7, yk, col = SK$ink, lwd = 4)
    sk_circle(ctr[k], yk, 0.14, col = SK$ink, solid = SK$ink)
  }
}
sk_text(5.5, 0.35, "0", cex = 2.6, col = SK$grey)
sk_text(9.9, 2.65, "pointwise", cex = 2.6, col = SK$red, adj = c(0, 0.5))
sk_text(9.3, 1.25, "simultaneous", cex = 2.6, col = SK$green, adj = c(0, 0.5))
sk_text(1.6, 4.2, "8 ×\n95%", cex = 3, col = SK$ink)
sk_save()

# Slide: The follow-up matches the family
sk_open("nongauss-b-tukey-dunnett")
tk <- list(x = c(1.6, 4.6, 4.6, 1.6), y = c(6.0, 6.0, 3.0, 3.0))
for (a in 1:3) for (b in (a + 1):4) {
  sk_line(tk$x[a], tk$y[a], tk$x[b], tk$y[b], col = SK$blue, lwd = 3)
}
for (k in 1:4) sk_circle(tk$x[k], tk$y[k], 0.45, solid = SK$blue_l)
sk_text(3.1, 1.3, "Tukey", cex = 3.2, col = SK$blue)
sk_line(6.1, 0.8, 6.1, 7.4, col = SK$grey, lwd = 2, lty = 2)
cx <- 9.3; cy <- 4.5
ang <- c(90, 210, 330) * pi / 180
for (a in ang) {
  sk_line(cx, cy, cx + 2.0 * cos(a), cy + 2.0 * sin(a), col = SK$orange,
          lwd = 3)
}
for (a in ang) sk_circle(cx + 2.0 * cos(a), cy + 2.0 * sin(a), 0.45,
                         solid = SK$orange_l)
sk_circle(cx, cy, 0.6, solid = SK$grey_l, lwd = 5)
sk_text(cx, cy, "C", cex = 3, font = 2)
sk_text(cx, 1.3, "Dunnett", cex = 3.2, col = SK$orange)
sk_save()

# Slide: One false claim, or some false signal
sk_open("nongauss-b-fwer-fdr")
sk_text(3.0, 7.2, "FWER", cex = 3.4, col = SK$blue)
sk_text(9.0, 7.2, "FDR", cex = 3.4, col = SK$orange)
sk_line(6.0, 0.6, 6.0, 7.6, col = SK$grey, lwd = 2, lty = 2)
# FWER: few claims, all right
gx <- c(1.8, 3.0, 4.2)
for (x in gx) sk_circle(x, 4.6, 0.38, col = SK$green, solid = SK$green_l)
sk_text(3.0, 2.8, "no red", cex = 3, col = SK$green)
sk_text(3.0, 1.6, "at all", cex = 3, col = SK$green)
# FDR: many claims, a few wrong
set.seed(7)
grid <- expand.grid(x = seq(7.2, 10.8, by = 0.9), y = seq(3.0, 6.0, by = 0.75))
red <- c(4, 13, 22)
for (k in seq_len(nrow(grid))) {
  cl <- if (k %in% red) SK$red else SK$green
  fl <- if (k %in% red) SK$red_l else SK$green_l
  sk_circle(grid$x[k], grid$y[k], 0.26, col = cl, solid = fl, lwd = 3)
}
sk_text(9.0, 1.6, "a few red", cex = 3, col = SK$red)
sk_save()

# Slide: A zero is an observation
sk_open("nongauss-b-zeros")
for (ox in c(0, 6.2)) {
  sk_line(ox + 0.5, 4.6, ox + 5.5, 4.6, col = SK$grey, lwd = 4)
  sk_line(ox + 0.5, 3.4, ox + 5.5, 3.4, col = SK$grey, lwd = 4)
  sk_line(ox + 2.4, 1.6, ox + 2.4, 3.4, col = SK$grey, lwd = 4)
  sk_line(ox + 3.6, 1.6, ox + 3.6, 3.4, col = SK$grey, lwd = 4)
  sk_line(ox + 2.4, 4.6, ox + 2.4, 6.4, col = SK$grey, lwd = 4)
  sk_line(ox + 3.6, 4.6, ox + 3.6, 6.4, col = SK$grey, lwd = 4)
}
sk_line(6.1, 0.6, 6.1, 7.6, col = SK$grey, lwd = 2, lty = 2)
sk_person(1.1, 4.9, col = SK$green, scale = 0.9)
sk_note(4.6, 6.9, "0", w = 1.2, h = 0.9, fill = SK$green_l, cex = 3.2)
sk_text(3.0, 0.9, "zero", cex = 3.2, col = SK$green)
# empty stool
sk_line(6.9, 5.7, 7.9, 5.7, col = SK$red, lwd = 4)
sk_line(7.0, 5.7, 6.9, 4.9, col = SK$red, lwd = 4)
sk_line(7.8, 5.7, 7.9, 4.9, col = SK$red, lwd = 4)
sk_note(10.8, 6.9, "?", w = 1.2, h = 0.9, fill = SK$red_l, cex = 3.2)
sk_text(9.2, 0.9, "missing", cex = 3.2, col = SK$red)
sk_save()

# Slide: A gamma latent rate is a negative binomial
sk_open("nongauss-b-gamma-poisson")
gfun <- function(x) 5.4 + 2.0 * dgamma(x - 0.8, shape = 3, rate = 0.8) / 0.27
sk_curve(gfun, 0.9, 8.6, col = SK$violet, lwd = 5)
sk_line(0.8, 5.4, 8.8, 5.4, col = SK$ink, lwd = 3)
lam <- c(2.4, 4.3, 7.2)
lc <- c(SK$blue, SK$orange, SK$red)
for (k in 1:3) {
  sk_circle(lam[k], 5.4, 0.17, col = lc[k], solid = lc[k])
  sk_arrow(lam[k], 5.1, lam[k], 4.25, col = lc[k], lwd = 3, head = 0.18)
  sk_bump(lam[k], 0.42, 1.0, 3.0, lam[k] - 1.2, lam[k] + 1.2, col = lc[k],
          lwd = 4)
}
sk_line(0.8, 3.0, 8.8, 3.0, col = SK$ink, lwd = 3)
sk_bump(4.4, 1.9, 1.5, 0.9, 0.9, 8.7, col = SK$ink, lwd = 5)
sk_line(0.8, 0.9, 8.8, 0.9, col = SK$ink, lwd = 3)
sk_text(9.2, 6.4, "λ ~ Gamma", cex = 2.6, col = SK$violet,
        adj = c(0, 0.5))
sk_text(9.2, 3.6, "Poisson(λ)", cex = 2.6, adj = c(0, 0.5))
sk_text(9.2, 1.6, "= NB", cex = 3.2, adj = c(0, 0.5), font = 2)
sk_save()
