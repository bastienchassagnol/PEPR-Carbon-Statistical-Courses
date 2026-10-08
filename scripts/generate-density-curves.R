# Illustrative densities for the GLM slides. Not CA-SYS or SPRUCE.
# Deterministic density functions, so there is no seed.

dir.create("figures/generated", showWarnings = FALSE, recursive = TRUE)

save_png <- function(filename, draw) {
  png(
    file.path("figures/generated", filename),
    width = 1400, height = 780, res = 140
  )
  on.exit(dev.off(), add = TRUE)
  op <- par(mar = c(4.2, 4.2, 1.2, 1), family = "sans", cex = 1.15)
  on.exit(par(op), add = TRUE)
  draw()
}

save_png("density-gaussian.png", function() {
  curve(dnorm(x, 0, 1), -6, 6, n = 400, lwd = 3, col = "#0e4d3a",
        xlab = "y", ylab = "density", ylim = c(0, 0.45), bty = "l")
  curve(dnorm(x, 0, 2), add = TRUE, n = 400, lwd = 3, col = "#9a3412")
  curve(dnorm(x, 1.5, 1), add = TRUE, n = 400, lwd = 3, col = "#1d4e89")
  legend("topright", bty = "n", lwd = 3,
         col = c("#0e4d3a", "#9a3412", "#1d4e89"),
         legend = c(
           expression(mu == 0 * "," ~ sigma == 1),
           expression(mu == 0 * "," ~ sigma == 2),
           expression(mu == 1.5 * "," ~ sigma == 1)
         ))
})

save_png("density-poisson.png", function() {
  lambdas <- c(2, 8, 20)
  cols <- c("#0e4d3a", "#1d4e89", "#9a3412")
  xs <- 0:40
  plot(NA, xlim = c(0, 40), ylim = c(0, 0.30), xlab = "y", ylab = "probability", bty = "l")
  for (i in seq_along(lambdas)) {
    lines(xs, dpois(xs, lambdas[i]), type = "h", lwd = 3, col = cols[i])
  }
  legend("topright", bty = "n", lwd = 3, col = cols,
         legend = c(
           expression(lambda == 2),
           expression(lambda == 8),
           expression(lambda == 20)
         ))
})

save_png("density-negbin.png", function() {
  mu <- 8
  thetas <- c(1, 5, 50)
  cols <- c("#9a3412", "#1d4e89", "#0e4d3a")
  xs <- 0:40
  plot(NA, xlim = c(0, 40), ylim = c(0, 0.16), xlab = "y", ylab = "probability", bty = "l")
  for (i in seq_along(thetas)) {
    lines(xs, dnbinom(xs, size = thetas[i], mu = mu), type = "h", lwd = 3, col = cols[i])
  }
  lines(xs, dpois(xs, mu), type = "b", pch = 16, cex = 0.6, lwd = 2, col = "grey40")
  legend("topright", bty = "n", lwd = 3,
         col = c(cols, "grey40"),
         legend = c(
           expression(theta == 1),
           expression(theta == 5),
           expression(theta == 50),
           "Poisson, same mean"
         ))
})

save_png("density-logit.png", function() {
  curve(plogis(0 + 0.6 * x), -4, 4, n = 400, lwd = 3, col = "#0e4d3a",
        xlab = "x", ylab = expression(pi), ylim = c(0, 1), bty = "l")
  curve(plogis(0 + 1.5 * x), add = TRUE, n = 400, lwd = 3, col = "#1d4e89")
  curve(plogis(-1 + 1.5 * x), add = TRUE, n = 400, lwd = 3, col = "#9a3412")
  legend("bottomright", bty = "n", lwd = 3,
         col = c("#0e4d3a", "#1d4e89", "#9a3412"),
         legend = c(
           expression(beta[0] == 0 * "," ~ beta[1] == 0.6),
           expression(beta[0] == 0 * "," ~ beta[1] == 1.5),
           expression(beta[0] == -1 * "," ~ beta[1] == 1.5)
         ))
})

save_png("jensen-convex-concave.png", function() {
  par(mfrow = c(1, 2), mar = c(4, 4, 2.2, 1))
  # Convex: f(x) = x^2 on [1, 3]. E[X] = 2, f(E) = 4, E[f] = 5.
  curve(x^2, 0.6, 3.4, n = 400, lwd = 3, col = "#1c2b24",
        xlab = "y", ylab = "f(y)", ylim = c(0, 11))
  title(main = "Convex", col.main = "#1c2b24", cex.main = 1.2)
  segments(1, 1, 3, 9, lwd = 2, col = "#9a3412")
  points(c(2, 2), c(4, 5), pch = 16, cex = 1.4, col = c("#0e4d3a", "#9a3412"))
  arrows(2.15, 4.15, 2.15, 4.85, length = 0.12, lwd = 3, col = "#9a3412")
  text(2.35, 4.5, "gap", col = "#9a3412", adj = 0)
  # Concave: f(x) = log(x) on [1, e^2]. Use 1 and 4. E = 2.5
  # f(1)=0, f(4)=log(4), chord, E[X]=2.5, f(E)=log(2.5), E[f]=log(4)/2
  curve(log(x), 0.7, 4.4, n = 400, lwd = 3, col = "#1c2b24",
        xlab = "y", ylab = "f(y)")
  title(main = "Concave", col.main = "#1c2b24", cex.main = 1.2)
  segments(1, log(1), 4, log(4), lwd = 2, col = "#1d4e89")
  ex <- 2.5
  f_ex <- log(ex)
  e_f <- log(4) / 2
  points(c(ex, ex), c(f_ex, e_f), pch = 16, cex = 1.4, col = c("#0e4d3a", "#1d4e89"))
  arrows(ex + 0.15, f_ex - 0.05, ex + 0.15, e_f + 0.05, length = 0.12, lwd = 3, col = "#1d4e89")
  text(ex + 0.3, (f_ex + e_f) / 2, "gap", col = "#1d4e89", adj = 0)
})
