# Illustrative figures for the PEPR Carbon workshop slides.
# Every series here is simulated. Nothing is a fit to NEON or SPRUCE data.
# Seed 20261005. Regenerate with: Rscript scripts/generate-slide-figures.R

seed <- 20261005L
out_dir <- "figures/generated"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

library(ggplot2)
library(MASS)
library(dplyr)
library(jsonlite)
library(ragg)

theme_slide <- function() {
  theme_minimal(base_size = 18) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e6e1d8"),
      plot.title = element_text(
        face = "bold", colour = "#0e4d3a", size = 18
      ),
      plot.subtitle = element_text(colour = "#3d4a42", size = 13),
      axis.title = element_text(colour = "#1c2b24"),
      strip.text = element_text(face = "bold", colour = "#0e4d3a"),
      legend.position = "bottom",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

save_fig <- function(plot, filename, width = 9, height = 5.2) {
  ggsave(
    filename = file.path(out_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 200,
    device = ragg::agg_png,
    bg = "white"
  )
}

set.seed(seed)
n <- 120L
moisture <- seq(-0.15, 0.15, length.out = n)
eta <- 2 + 2.5 * moisture
mu <- exp(eta)

y_pois <- rpois(n, lambda = mu)
y_nb <- rnbinom(n, mu = mu, size = 1.5)
y_gauss <- 8 + 12 * moisture + rnorm(n, sd = 1.2)

ph <- seq(-1.5, 1.5, length.out = n)
eta_bin <- -0.2 + 1.4 * ph
y_bin <- rbinom(n, size = 1L, prob = plogis(eta_bin))

fit_pois <- glm(y_pois ~ moisture, family = poisson)
fit_pois_on_nb <- glm(y_nb ~ moisture, family = poisson)
fit_nb <- glm.nb(y_nb ~ moisture)
fit_lm_gauss <- lm(y_gauss ~ moisture)
fit_lm_count <- lm(y_pois ~ moisture)
fit_bin <- glm(y_bin ~ ph, family = binomial)

quantile_resid <- function(y, mu_hat) {
  # Randomised quantile residual for a Poisson mean (Dunn–Smyth).
  lower <- ifelse(y <= 0, 0, ppois(y - 1L, lambda = mu_hat))
  upper <- ppois(y, lambda = mu_hat)
  lower <- pmin(lower, 1 - 1e-8)
  upper <- pmin(pmax(upper, lower + 1e-8), 1 - 1e-10)
  u <- runif(length(y), min = lower, max = upper)
  qnorm(u)
}

qq_gaussian <- bind_rows(
  tibble(
    model = "Gaussian lm, Gaussian Y",
    residual = resid(fit_lm_gauss)
  ),
  tibble(
    model = "Gaussian lm, Poisson Y",
    residual = resid(fit_lm_count)
  )
)

p_qq <- ggplot(qq_gaussian, aes(sample = residual)) +
  stat_qq_line(colour = "#c46b3a", linewidth = 0.7) +
  stat_qq(colour = "#0e4d3a", size = 1.6, alpha = 0.85) +
  facet_wrap(~model) +
  labs(
    title = "A QQ plot asks whether a reference shape is plausible",
    subtitle = "Illustrative residuals, seed 20261005. The line is the Gaussian reference.",
    x = "Gaussian quantiles",
    y = "Residual quantiles"
  ) +
  theme_slide()
save_fig(p_qq, "qq-gaussian-vs-count.png", width = 10, height = 5.2)

set.seed(seed + 1L)
qr <- bind_rows(
  tibble(
    model = "Poisson fit to Poisson counts",
    residual = quantile_resid(y_pois, fitted(fit_pois))
  ),
  tibble(
    model = "Poisson fit to overdispersed counts",
    residual = quantile_resid(y_nb, fitted(fit_pois_on_nb))
  )
)

p_qr <- ggplot(qr, aes(sample = residual)) +
  stat_qq_line(colour = "#c46b3a", linewidth = 0.7) +
  stat_qq(colour = "#0e4d3a", size = 1.6, alpha = 0.85) +
  facet_wrap(~model) +
  labs(
    title = "Quantile residuals check the count model, not a test of Y",
    subtitle = "Randomised Poisson quantile residuals. A tail beyond the line is extra-Poisson variation.",
    x = "Gaussian quantiles",
    y = "Quantile residual"
  ) +
  theme_slide()
save_fig(p_qr, "qq-quantile-residuals.png", width = 10, height = 5.2)

# Mean-variance by moisture bin.
mv <- bind_rows(
  tibble(moisture = moisture, y = y_pois, family = "Poisson draws"),
  tibble(moisture = moisture, y = y_nb, family = "Negative binomial draws")
) |>
  mutate(bin = cut(moisture, breaks = 8, include.lowest = TRUE)) |>
  group_by(family, bin) |>
  summarise(
    mean_y = mean(y),
    var_y = var(y),
    .groups = "drop"
  )

p_mv <- ggplot(mv, aes(mean_y, var_y, colour = family)) +
  geom_abline(slope = 1, intercept = 0, colour = "#1c2b24", linewidth = 0.6) +
  geom_point(size = 3) +
  scale_colour_manual(values = c(
    "Poisson draws" = "#0e4d3a",
    "Negative binomial draws" = "#c46b3a"
  )) +
  labs(
    title = "Equal mean and variance is a Poisson assumption",
    subtitle = "Points are bin-wise moments. The line is variance = mean.",
    x = "Bin mean of Y",
    y = "Bin variance of Y",
    colour = NULL
  ) +
  theme_slide()
save_fig(p_mv, "mean-variance.png")

grid_m <- tibble(moisture = seq(-0.15, 0.15, length.out = 200))
pred_pois <- predict(fit_pois, newdata = grid_m, se.fit = TRUE, type = "link")
grid_m <- grid_m |>
  mutate(
    mu = exp(pred_pois$fit),
    lo = exp(pred_pois$fit - 1.96 * pred_pois$se.fit),
    hi = exp(pred_pois$fit + 1.96 * pred_pois$se.fit)
  )

p_pois <- ggplot() +
  geom_ribbon(
    data = grid_m,
    aes(moisture, ymin = lo, ymax = hi),
    fill = "#9ec9b6",
    alpha = 0.9
  ) +
  geom_line(data = grid_m, aes(moisture, mu), colour = "#0e4d3a", linewidth = 1) +
  geom_point(
    data = tibble(moisture = moisture, y = y_pois),
    aes(moisture, y),
    colour = "#1c2b24",
    alpha = 0.7,
    size = 1.8
  ) +
  labs(
    title = "Poisson log-linear mean for a count",
    subtitle = "Band: Wald interval for the mean on the link scale, mapped back with exp.",
    x = "Centred soil moisture, x (g/g)",
    y = "Colony count, Y"
  ) +
  theme_slide()
save_fig(p_pois, "poisson-fit.png")

pred_nb <- predict(fit_nb, newdata = grid_m[, "moisture", drop = FALSE], se.fit = TRUE, type = "link")
theta_hat <- fit_nb$theta
grid_nb <- tibble(
  moisture = grid_m$moisture,
  mu = exp(pred_nb$fit),
  lo = exp(pred_nb$fit - 1.96 * pred_nb$se.fit),
  hi = exp(pred_nb$fit + 1.96 * pred_nb$se.fit)
) |>
  mutate(
    sd = sqrt(mu + mu^2 / theta_hat),
    pred_lo = pmax(0, mu - 1.96 * sd),
    pred_hi = mu + 1.96 * sd
  )

p_nb <- ggplot() +
  geom_ribbon(
    data = grid_nb,
    aes(moisture, ymin = pred_lo, ymax = pred_hi),
    fill = "#f0d3c2",
    alpha = 0.95
  ) +
  geom_ribbon(
    data = grid_nb,
    aes(moisture, ymin = lo, ymax = hi),
    fill = "#0e4d3a",
    alpha = 0.25
  ) +
  geom_line(data = grid_nb, aes(moisture, mu), colour = "#0e4d3a", linewidth = 1) +
  geom_point(
    data = tibble(moisture = moisture, y = y_nb),
    aes(moisture, y),
    colour = "#1c2b24",
    alpha = 0.65,
    size = 1.8
  ) +
  labs(
    title = "The negative binomial keeps the mean and widens the variance",
    subtitle = "Green band: uncertainty of the mean. Clay band: approximate observation band.",
    x = "Centred soil moisture, x (g/g)",
    y = "Colony count, Y"
  ) +
  theme_slide()
save_fig(p_nb, "negative-binomial-fit.png")

grid_p <- tibble(ph = seq(-1.5, 1.5, length.out = 200))
pred_bin <- predict(fit_bin, newdata = grid_p, se.fit = TRUE, type = "link")
grid_p <- grid_p |>
  mutate(
    pi = plogis(pred_bin$fit),
    lo = plogis(pred_bin$fit - 1.96 * pred_bin$se.fit),
    hi = plogis(pred_bin$fit + 1.96 * pred_bin$se.fit)
  )

p_bin <- ggplot() +
  geom_ribbon(
    data = grid_p,
    aes(ph, ymin = lo, ymax = hi),
    fill = "#9ec9b6",
    alpha = 0.9
  ) +
  geom_line(data = grid_p, aes(ph, pi), colour = "#0e4d3a", linewidth = 1) +
  geom_point(
    data = tibble(ph = ph, y = y_bin),
    aes(ph, y),
    colour = "#1c2b24",
    alpha = 0.45,
    size = 1.8,
    position = position_jitter(height = 0.03, width = 0)
  ) +
  scale_y_continuous(limits = c(-0.05, 1.05), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  labs(
    title = "A logit model maps x onto a probability",
    subtitle = "Points are binary detections, jittered vertically. Band: Wald interval for the probability.",
    x = "Centred soil pH, x",
    y = "Detection probability"
  ) +
  theme_slide()
save_fig(p_bin, "logit-fit.png")

design <- expand.grid(
  warming_c = c(0, 2.25, 4.5, 6.75, 9),
  co2 = c("Ambient CO2", "Elevated CO2")
)
design$enclosure <- seq_len(nrow(design))

p_design <- ggplot(design, aes(warming_c, co2, fill = warming_c)) +
  geom_tile(colour = "white", linewidth = 1.2, width = 1.6, height = 0.8) +
  geom_text(aes(label = sprintf("+%s°C", warming_c)), colour = "#1c2b24", size = 4.5) +
  scale_fill_gradient(low = "#d7ebe3", high = "#c46b3a", guide = "none") +
  scale_x_continuous(breaks = c(0, 2.25, 4.5, 6.75, 9)) +
  labs(
    title = "SPRUCE: ten enclosures, five temperatures, two CO2 levels",
    subtitle = "Schematic of the allocation, not a map. Each tile is one experimental unit.",
    x = "Warming above ambient (°C)",
    y = NULL
  ) +
  theme_slide() +
  theme(panel.grid = element_blank())
save_fig(p_design, "spruce-allocation.png", width = 10, height = 4.6)

set.seed(seed + 2L)
obs <- tibble(temp = seq(0, 9, length.out = 40)) |>
  mutate(
    mu = 4 + 0.35 * temp - 0.08 * (temp - 4.5)^2,
    y = rnorm(n(), mu, 0.35)
  )
fit_line <- lm(y ~ temp, data = obs)
obs <- obs |> mutate(line = predict(fit_line))

p_curve <- ggplot(obs, aes(temp, y)) +
  geom_point(colour = "#1c2b24", size = 2, alpha = 0.8) +
  geom_line(aes(y = line), colour = "#c46b3a", linewidth = 1) +
  geom_line(aes(y = mu), colour = "#0e4d3a", linewidth = 1) +
  labs(
    title = "A straight line can miss a hump that matters",
    subtitle = "Illustrative curve. Clay: fitted straight line. Green: mean used to simulate Y.",
    x = "Warming, x (°C)",
    y = "Response, Y"
  ) +
  theme_slide()
save_fig(p_curve, "linear-vs-curve.png")

wald <- function(fit) {
  est <- coef(fit)
  se <- sqrt(diag(vcov(fit)))
  list(
    estimate = est,
    se = se,
    exp_estimate = exp(est),
    conf_low = est - 1.96 * se,
    conf_high = est + 1.96 * se,
    exp_low = exp(est - 1.96 * se),
    exp_high = exp(est + 1.96 * se)
  )
}

summary_nb <- list(
  theta = unname(fit_nb$theta),
  se_theta = unname(fit_nb$SE.theta)
)

# Tüzen (10 July 2025): one sample, then the sampling distribution of the mean.
# Seed 42 is that note's seed, not the workshop seed used above.
set.seed(42)
sd_sample <- rnorm(50, mean = 100, sd = 15)
sd_means <- replicate(1000, mean(rnorm(50, mean = 100, sd = 15)))
sd_se <- list(
  seed = 42L,
  n = 50L,
  replicates = 1000L,
  population_mean = 100,
  population_sd = 15,
  sample_sd = sd(sd_sample),
  sample_mean = mean(sd_sample),
  se_of_means = sd(sd_means),
  source = "https://www.r-bloggers.com/2025/07/standard-deviation-vs-standard-error-meaning-misuse-and-the-math-behind-the-confusion/"
)
p_sd_se <- patchwork::wrap_plots(
  ggplot(data.frame(y = sd_sample), aes(y)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "#0e4d3a", colour = "white", alpha = 0.85) +
    geom_vline(xintercept = mean(sd_sample), colour = "#9a3412", linewidth = 0.8) +
    labs(title = "One sample: spread of Y", x = "Y", y = "Density"),
  ggplot(data.frame(m = sd_means), aes(m)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "#9a3412", colour = "white", alpha = 0.85) +
    geom_vline(xintercept = mean(sd_means), colour = "#0e4d3a", linewidth = 0.8) +
    labs(title = "1,000 means: spread of the mean", x = "Sample mean", y = "Density"),
  ncol = 2
) +
  patchwork::plot_annotation(
    title = "Standard deviation is not the standard error",
    subtitle = "Illustrative draw from N(100, 15). Seed 42. Not a carbon dataset."
  ) &
  theme_slide()
save_fig(p_sd_se, "sd-versus-se.png", width = 11, height = 5.2)

# Schematic of Thiese, Ronna, and Ott (2016), Figure 1, adapted there from Rothman.
# The heights are not data. They only show which error moves with n.
error_n <- seq(20, 400, length.out = 200)
error_df <- rbind(
  data.frame(n = error_n, error = 6 / sqrt(error_n / 20), kind = "Random error of the estimate"),
  data.frame(n = error_n, error = 2.4, kind = "Systematic error")
)
p_error <- ggplot(error_df, aes(n, error, colour = kind)) +
  geom_line(linewidth = 1.1) +
  scale_colour_manual(values = c(
    "Random error of the estimate" = "#0e4d3a",
    "Systematic error" = "#9a3412"
  )) +
  labs(
    title = "More rows shrink noise, not bias",
    subtitle = "Illustrative curves. The shape follows Thiese, Ronna, and Ott, Figure 1.",
    x = "Sample size",
    y = "Size of the error",
    colour = NULL
  )
save_fig(p_error, "random-versus-systematic-error.png", width = 9, height = 4.6)

payload <- list(
  seed = seed,
  n = n,
  poisson = wald(fit_pois),
  negative_binomial = c(wald(fit_nb), summary_nb),
  logit = wald(fit_bin),
  sd_versus_se = sd_se,
  note = "Illustrative simulations. Coefficients are estimates from one seed, not NEON or SPRUCE results. The SD-versus-SE panel uses seed 42."
)

write_json(payload, file.path(out_dir, "estimates.json"), pretty = TRUE, auto_unbox = TRUE)

# Diagnostic pairs for plot.lm. Own seed so this block does not depend on
# the draw above. Illustrative, not a carbon dataset.
set.seed(seed)
n_diag <- 80L
x_diag <- seq(-1, 1, length.out = n_diag)
y_ok <- 1 + 2 * x_diag + rnorm(n_diag, sd = 0.35)
y_curve <- x_diag^2 + rnorm(n_diag, sd = 0.08)
y_tail <- 1 + 2 * x_diag + rt(n_diag, df = 3) * 0.45
y_het <- 1 + 2 * x_diag + rnorm(n_diag, sd = 0.12 + 1.1 * (x_diag - min(x_diag)))
x_lev <- c(x_diag, 4)
y_lev <- c(y_ok, 1 + 2 * 4 + 8)
fit_ok <- lm(y_ok ~ x_diag)
fit_curve <- lm(y_curve ~ x_diag)
fit_tail <- lm(y_tail ~ x_diag)
fit_het <- lm(y_het ~ x_diag)
fit_lev <- lm(y_lev ~ x_lev)

save_lm_pair <- function(fit_left, fit_right, which, file) {
  ragg::agg_png(
    file.path(out_dir, file),
    width = 1200, height = 560, units = "px", res = 140
  )
  op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.4, 1), oma = c(0, 0, 0, 0))
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)
  plot(fit_left, which = which, caption = "Fits", sub.caption = "", id.n = 0)
  plot(fit_right, which = which, caption = "Does not fit", sub.caption = "", id.n = 2)
}

save_lm_pair(fit_ok, fit_curve, 1, "lm-resid-fitted.png")
save_lm_pair(fit_ok, fit_tail, 2, "lm-qq.png")
save_lm_pair(fit_ok, fit_het, 3, "lm-scale-location.png")
save_lm_pair(fit_ok, fit_lev, 5, "lm-leverage.png")

# Longitudinal illustration. Own seed, so a full rerun does not move earlier draws.
set.seed(seed)
if (requireNamespace("lme4", quietly = TRUE)) {
  n_enc <- 12L
  n_time <- 6L
  enclosure <- factor(rep(seq_len(n_enc), each = n_time))
  time <- rep(seq_len(n_time) - 1, n_enc)
  warming <- rep(rep(c("ambient", "warmed"), each = n_enc / 2), each = n_time)
  u0 <- rnorm(n_enc, sd = 1.1)
  u1 <- rnorm(n_enc, sd = 0.28)
  Y <- 2 + u0[enclosure] +
    (0.25 + u1[enclosure]) * time +
    0.45 * (warming == "warmed") * time +
    rnorm(n_enc * n_time, sd = 0.3)
  longi <- data.frame(enclosure, time, warming, Y)
  fit_longi <- lme4::lmer(
    Y ~ time * warming + (time | enclosure),
    data = longi,
    REML = TRUE
  )
  longi$fitted <- predict(fit_longi)
  p_longi <- ggplot(longi, aes(time, Y, group = enclosure, colour = warming)) +
    geom_point(size = 2.2) +
    geom_line(aes(y = fitted), linewidth = 0.7) +
    scale_colour_manual(values = c(ambient = "#1d4e89", warmed = "#c45c26")) +
    labs(
      title = "One mean line, a slope and an intercept per enclosure",
      subtitle = "Illustrative. Seed 20261005. Lines are the random-intercept, random-slope fit.",
      x = "Time",
      y = "Y",
      colour = NULL
    ) +
    theme_slide()
  save_fig(p_longi, "longitudinal-random-slope.png", width = 9, height = 5)
}

# Adult height by sex. Illustrative Gaussians, not a survey.
# Pooled Y is a two-component mixture. Each component is Gaussian.
set.seed(seed)
n_height <- 2000L
mu_f <- 163
mu_h <- 177
sd_f <- 6.5
sd_h <- 7
height <- data.frame(
  sex = rep(c("F", "H"), each = n_height / 2),
  Y = c(
    rnorm(n_height / 2, mu_f, sd_f),
    rnorm(n_height / 2, mu_h, sd_h)
  )
)
height_grid <- seq(140, 200, length.out = 400)
height_curves <- rbind(
  data.frame(
    sex = "F",
    Y = height_grid,
    density = 0.5 * dnorm(height_grid, mu_f, sd_f)
  ),
  data.frame(
    sex = "H",
    Y = height_grid,
    density = 0.5 * dnorm(height_grid, mu_h, sd_h)
  )
)
p_height <- ggplot(height, aes(Y)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 32,
    fill = "#d9d3c7",
    colour = "white",
    linewidth = 0.2
  ) +
  geom_line(
    data = height_curves,
    aes(y = density, colour = sex),
    linewidth = 1.15
  ) +
  geom_vline(
    xintercept = c(mu_f, mu_h),
    linetype = "dashed",
    linewidth = 0.6,
    colour = "#1c2b24"
  ) +
  annotate(
    "text",
    x = c(mu_f - 1.2, mu_h + 1.2),
    y = 0.034,
    label = c("mu[F]", "mu[H]"),
    parse = TRUE,
    size = 4.5,
    hjust = c(1, 0),
    colour = "#1c2b24"
  ) +
  scale_colour_manual(values = c(F = "#1d4e89", H = "#c45c26")) +
  labs(
    subtitle = "Illustrative. Seed 20261005. Grey bars pool both sexes.",
    x = "Height Y (cm)",
    y = "Density",
    colour = "Sex x"
  ) +
  theme_slide()
save_fig(p_height, "height-by-sex.png", width = 9.2, height = 4.4)

# Four functions of one standard normal. The 0.05 quantile is exact.
x_norm <- seq(-4, 4, length.out = 401)
q05 <- qnorm(0.05)
shade <- data.frame(
  x = c(-4, seq(-4, q05, length.out = 80), q05),
  y = c(0, dnorm(seq(-4, q05, length.out = 80)), 0)
)
panel_theme <- function() {
  theme_slide() +
    theme(
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}
p_dnorm <- ggplot(data.frame(x = x_norm, y = dnorm(x_norm)), aes(x, y)) +
  geom_line(colour = "#1d4e89", linewidth = 1.1) +
  labs(title = "Density, dnorm", x = "z", y = "f(z)") +
  panel_theme()
p_qnorm <- ggplot(data.frame(x = x_norm, y = dnorm(x_norm)), aes(x, y)) +
  geom_polygon(data = shade, fill = "#1d4e89", alpha = 0.28) +
  geom_line(colour = "#1d4e89", linewidth = 1.1) +
  geom_vline(xintercept = q05, linetype = "dashed", colour = "#1c2b24") +
  annotate(
    "text", x = -3.15, y = 0.28,
    label = "area = 0.05", size = 3.6, colour = "#1c2b24"
  ) +
  labs(
    title = "Quantile, qnorm(0.05)",
    subtitle = sprintf("z = %.2f", q05),
    x = "z", y = "f(z)"
  ) +
  panel_theme()
p_pnorm <- ggplot(data.frame(x = x_norm, y = pnorm(x_norm)), aes(x, y)) +
  geom_line(colour = "#1d4e89", linewidth = 1.1) +
  annotate(
    "segment",
    x = q05, xend = q05, y = 0, yend = 0.05,
    linetype = "dashed", colour = "#1c2b24"
  ) +
  annotate(
    "segment",
    x = -4, xend = q05, y = 0.05, yend = 0.05,
    linetype = "dashed", colour = "#1c2b24"
  ) +
  annotate(
    "point", x = q05, y = 0.05, colour = "#c45c26", size = 2.4
  ) +
  labs(title = "Cumulative distribution, pnorm", x = "z", y = "F(z)") +
  panel_theme()
set.seed(seed)
sample_z <- data.frame(z = rnorm(400))
p_rnorm <- ggplot(sample_z, aes(z)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 24,
    fill = "#d9d3c7",
    colour = "white",
    linewidth = 0.2
  ) +
  geom_line(
    data = data.frame(z = x_norm, y = dnorm(x_norm)),
    aes(y = y),
    colour = "#1d4e89",
    linewidth = 1.1
  ) +
  labs(
    title = "Sample, rnorm",
    subtitle = "n = 400. Seed 20261005.",
    x = "z", y = "Density"
  ) +
  panel_theme()
ragg::agg_png(
  file.path(out_dir, "normal-four-functions.png"),
  width = 10.4, height = 7.2, units = "in", res = 180
)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
print(p_dnorm, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_qnorm, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p_pnorm, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_rnorm, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
grDevices::dev.off()


# Opening block: one figure per notion, in the order the slides introduce
# them. All illustrative; seed 20261005 is reset before each draw.
col_a <- "#1d4e89"
col_b <- "#c45c26"
col_c <- "#0e4d3a"
col_ink <- "#1c2b24"

# Mean and variance: the mean moves the curve, sigma widens it.
y_grid <- seq(-6, 9, length.out = 600)
mv <- rbind(
  data.frame(panel = "Same σ = 1, different μ", curve = "μ = 0",
             y = y_grid, d = dnorm(y_grid, 0, 1)),
  data.frame(panel = "Same σ = 1, different μ", curve = "μ = 3",
             y = y_grid, d = dnorm(y_grid, 3, 1)),
  data.frame(panel = "Same μ = 1, different σ", curve = "σ = 1",
             y = y_grid, d = dnorm(y_grid, 1, 1)),
  data.frame(panel = "Same μ = 1, different σ", curve = "σ = 2.5",
             y = y_grid, d = dnorm(y_grid, 1, 2.5))
)
mv$panel <- factor(mv$panel, levels = unique(mv$panel))
mv_labels <- data.frame(
  panel = factor(c("Same σ = 1, different μ", "Same σ = 1, different μ",
                   "Same μ = 1, different σ", "Same μ = 1, different σ"),
                 levels = levels(mv$panel)),
  curve = c("μ = 0", "μ = 3", "σ = 1", "σ = 2.5"),
  x = c(-1.6, 4.6, 2.9, 5.2), y = c(0.33, 0.33, 0.33, 0.12)
)
mv_marks <- data.frame(
  panel = c("Same σ = 1, different μ", "Same σ = 1, different μ",
            "Same μ = 1, different σ"),
  x = c(0, 3, 1)
)
mv_sd <- data.frame(
  panel = "Same μ = 1, different σ",
  x = c(1, 1), xend = c(2, 3.5), y = c(0.2, 0.07),
  curve = c("σ = 1", "σ = 2.5")
)
mv_marks$panel <- factor(mv_marks$panel, levels = levels(mv$panel))
mv_sd$panel <- factor(mv_sd$panel, levels = levels(mv$panel))
p_mean_var <- ggplot(mv, aes(y, d, colour = curve)) +
  geom_line(linewidth = 1.2) +
  geom_vline(data = mv_marks, aes(xintercept = x), linetype = "dashed",
             colour = col_ink, linewidth = 0.5) +
  geom_segment(data = mv_sd, aes(x = x, xend = xend, y = y, yend = y),
               arrow = grid::arrow(length = grid::unit(0.15, "cm")),
               linewidth = 0.9, inherit.aes = FALSE,
               colour = c(col_a, col_b)) +
  geom_text(data = mv_labels, aes(x, y, label = curve), size = 5.5) +
  facet_wrap(~panel) +
  scale_colour_manual(values = c(`μ = 0` = col_a, `μ = 3` = col_b,
                                 `σ = 1` = col_a, `σ = 2.5` = col_b),
                      guide = "none") +
  labs(x = "Y", y = "Density",
       subtitle = "Gaussian densities. Dashed line: μ. Arrow: one σ.") +
  theme_slide()
save_fig(p_mean_var, "opening-mean-variance.png", width = 10, height = 4.2)

# A skewed response: the mean is the balance point, not the middle value.
set.seed(seed)
skew <- data.frame(Y = rgamma(300, shape = 2, rate = 0.5))
skew_grid <- data.frame(Y = seq(0, 20, length.out = 400))
skew_grid$d <- dgamma(skew_grid$Y, shape = 2, rate = 0.5)
skew_lines <- data.frame(
  what = c("Mean E(Y) = 4", sprintf("Median = %.1f", qgamma(0.5, 2, 0.5))),
  x = c(4, qgamma(0.5, 2, 0.5))
)
p_skew <- ggplot(skew, aes(Y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "#d9d3c7", colour = "white", linewidth = 0.2) +
  geom_line(data = skew_grid, aes(Y, d), colour = col_c, linewidth = 1.1) +
  geom_vline(data = skew_lines, aes(xintercept = x, colour = what),
             linewidth = 1, linetype = "dashed") +
  scale_colour_manual(values = c(col_b, col_a)) +
  labs(x = "Y", y = "Density", colour = NULL,
       subtitle = sprintf(
         "Gamma draw, n = 300, seed 20261005. Sample mean %.2f.",
         mean(skew$Y)
       )) +
  theme_slide()
save_fig(p_skew, "opening-skew-mean.png", width = 9, height = 4.2)

# Bias and variance: two ways to estimate the same mean, 2,000 repeats.
set.seed(seed)
reps <- replicate(2000, {
  s <- rnorm(10, mean = 5, sd = 3)
  c(mean(s), 0.6 * mean(s) + 1)
})
est <- data.frame(
  estimator = rep(c("Sample mean: centred, wide",
                    "Shrunk mean: narrow, off target"), each = 2000),
  value = c(reps[1, ], reps[2, ])
)
p_bias_var <- ggplot(est, aes(value, fill = estimator)) +
  geom_density(alpha = 0.45, colour = NA) +
  geom_vline(xintercept = 5, linetype = "dashed", colour = col_ink) +
  annotate("text", x = 5.1, y = 0, label = "true μ = 5", hjust = 0,
           vjust = -0.5, size = 5, colour = col_ink) +
  scale_fill_manual(values = c(col_a, col_b)) +
  labs(x = "Estimate of μ", y = "Density", fill = NULL,
       subtitle = "2,000 samples of n = 10. Seed 20261005.") +
  theme_slide()
save_fig(p_bias_var, "opening-bias-variance.png", width = 9, height = 4.2)

# The conditional mean as a line: a distribution of Y at every x.
set.seed(seed)
cm <- data.frame(x = runif(150, 0, 10))
cm$Y <- 2 + 0.8 * cm$x + rnorm(150, sd = 1.3)
slices <- do.call(rbind, lapply(c(2, 5, 8), function(x0) {
  yy <- seq(-3.5, 3.5, length.out = 100)
  data.frame(x0 = x0, Y = 2 + 0.8 * x0 + yy,
             x = x0 + 1.6 * dnorm(yy, sd = 1.3) / dnorm(0, sd = 1.3))
}))
p_cond_line <- ggplot(cm, aes(x, Y)) +
  geom_point(colour = "#9aa39d", size = 1.6) +
  geom_abline(intercept = 2, slope = 0.8, colour = col_b, linewidth = 1.2) +
  geom_vline(xintercept = c(2, 5, 8), colour = "#e6e1d8") +
  geom_path(data = slices, aes(x, Y, group = x0), colour = col_a,
            linewidth = 1.1) +
  annotate("text", x = 0.2, y = 11,
           label = "mu(x) == beta[0] + beta[1] * x", parse = TRUE,
           colour = col_b, size = 6, hjust = 0) +
  labs(x = "x", y = "Y",
       subtitle = "Simulated, seed 20261005. Blue: the distribution of Y at one x.") +
  theme_slide()
save_fig(p_cond_line, "opening-conditional-line.png", width = 9, height = 4.4)

# The link: a straight line on the scale of eta, a curve that stays in range.
x_link <- seq(-4, 4, length.out = 300)
links <- rbind(
  data.frame(panel = "Count: log link, μ = exp(η)", x = x_link,
             value = exp(0.4 + 0.45 * x_link), what = "μ with the link"),
  data.frame(panel = "Count: log link, μ = exp(η)", x = x_link,
             value = 1.5 + 0.9 * x_link, what = "straight line, no link"),
  data.frame(panel = "Binary: logit link, π = 1/(1 + exp(−η))", x = x_link,
             value = plogis(1.1 * x_link), what = "μ with the link"),
  data.frame(panel = "Binary: logit link, π = 1/(1 + exp(−η))", x = x_link,
             value = 0.5 + 0.17 * x_link, what = "straight line, no link")
)
links$panel <- factor(links$panel, levels = unique(links$panel))
bounds <- data.frame(
  panel = c("Count: log link, μ = exp(η)",
            "Binary: logit link, π = 1/(1 + exp(−η))",
            "Binary: logit link, π = 1/(1 + exp(−η))"),
  y = c(0, 0, 1)
)
p_links <- ggplot(links, aes(x, value, colour = what, linetype = what)) +
  geom_hline(data = bounds, aes(yintercept = y), colour = col_ink,
             linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~panel, scales = "free_y") +
  scale_colour_manual(values = c(`μ with the link` = col_a,
                                 `straight line, no link` = "#9a3412")) +
  scale_linetype_manual(values = c(`μ with the link` = "solid",
                                   `straight line, no link` = "dashed")) +
  labs(x = "x", y = "Mean of Y", colour = NULL, linetype = NULL,
       subtitle = "The dashed line leaves the allowed range. The link keeps μ inside it.") +
  theme_slide()
save_fig(p_links, "opening-link-functions.png", width = 10, height = 4.2)

# Likelihood: the same Poisson mean, two sample sizes.
set.seed(seed)
y10 <- rpois(10, 4)
y100 <- rpois(100, 4)
theta <- seq(2, 7, length.out = 400)
rel_ll <- function(y) {
  ll <- sapply(theta, function(t) sum(dpois(y, t, log = TRUE)))
  ll - max(ll)
}
lik <- rbind(
  data.frame(sample = "n = 10", theta = theta, ll = rel_ll(y10)),
  data.frame(sample = "n = 100", theta = theta, ll = rel_ll(y100))
)
mle <- data.frame(sample = c("n = 10", "n = 100"),
                  theta = c(mean(y10), mean(y100)))
p_lik <- ggplot(lik, aes(theta, ll, colour = sample)) +
  geom_hline(yintercept = -1.92, linetype = "dotted", colour = col_ink) +
  geom_line(linewidth = 1.2) +
  geom_point(data = mle, aes(theta, 0), size = 3) +
  annotate("text", x = 2.05, y = -1.92, label = "95% interval cut",
           vjust = -0.5, hjust = 0, size = 4.5, colour = col_ink) +
  coord_cartesian(ylim = c(-8, 0.3)) +
  scale_colour_manual(values = c(col_b, col_a)) +
  labs(x = "θ, the Poisson mean", y = "ℓ(θ) − ℓ(θ̂)", colour = NULL,
       subtitle = "Poisson draws, true θ = 4, seed 20261005. Dot: the maximum.") +
  theme_slide()
save_fig(p_lik, "opening-likelihood.png", width = 9, height = 4.2)

# Central limit theorem: means of a skewed variable become Gaussian.
set.seed(seed)
clt <- do.call(rbind, lapply(c(1, 5, 30), function(n_i) {
  data.frame(n = sprintf("n = %d", n_i),
             mean = replicate(3000, mean(rexp(n_i, rate = 1))))
}))
clt$n <- factor(clt$n, levels = c("n = 1", "n = 5", "n = 30"))
clt_curve <- do.call(rbind, lapply(c(1, 5, 30), function(n_i) {
  g <- seq(0, 4, length.out = 300)
  data.frame(n = sprintf("n = %d", n_i), mean = g,
             d = dnorm(g, 1, 1 / sqrt(n_i)))
}))
clt_curve$n <- factor(clt_curve$n, levels = levels(clt$n))
p_clt <- ggplot(clt, aes(mean)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "#d9d3c7", colour = "white", linewidth = 0.2) +
  geom_line(data = clt_curve, aes(mean, d), colour = col_a,
            linewidth = 1.1) +
  facet_wrap(~n, scales = "free_y") +
  coord_cartesian(xlim = c(0, 4)) +
  labs(x = expression(bar(Y)), y = "Density",
       subtitle = "3,000 means of exponential draws, seed 20261005. Blue: N(1, 1/n).") +
  theme_slide()
save_fig(p_clt, "opening-clt.png", width = 10, height = 4)

# Pearson correlation is the cosine of the angle between centred vectors.
set.seed(seed)
cors <- do.call(rbind, lapply(c(-0.8, 0, 0.5, 0.9), function(rho) {
  z <- MASS::mvrnorm(60, c(0, 0), matrix(c(1, rho, rho, 1), 2))
  r <- cor(z[, 1], z[, 2])
  data.frame(
    panel = sprintf("r = %.2f, θ = %.0f°", r, acos(r) * 180 / pi),
    rho = rho, x = z[, 1], Y = z[, 2]
  )
}))
cors$panel <- factor(cors$panel, levels = unique(cors$panel))
p_cor <- ggplot(cors, aes(x, Y)) +
  geom_point(colour = col_a, size = 1.6, alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              colour = col_b, linewidth = 0.9) +
  facet_wrap(~panel, nrow = 1) +
  labs(x = "x", y = "Y",
       subtitle = "Simulated pairs, n = 60, seed 20261005.") +
  theme_slide() +
  theme(axis.text = element_blank())
save_fig(p_cor, "opening-correlation.png", width = 11, height = 3.6)


# Four QQ shapes, one panel each, for the fragment slide #sec-qq-shapes.
# Shapes: Gaussian, heavy tails, right skew, light tails. Dark style after figures/sources/qq-four-shapes.png. Samples standardised.
set.seed(seed)
n_qq <- 60
std <- function(v) (v - mean(v)) / sd(v)
# Idealised shapes: each distribution's own quantiles, plus a little noise,
# so the bend is visible at n = 60. The Gaussian panel is a real draw.
p_grid <- (seq_len(n_qq) - 0.5) / n_qq
wobble <- function(q) sort(std(q) + rnorm(n_qq, sd = 0.04))
qq_samples <- list(
  normal = std(rnorm(n_qq)),
  heavy = wobble(qt(p_grid, df = 1.6)),
  skew = wobble(qexp(p_grid)),
  light = wobble(qunif(p_grid))
)
qq_style <- list(
  normal = list(col = "#3ddc97", title = "Panel A"),
  heavy = list(col = "#f06292", title = "Panel B"),
  skew = list(col = "#f5a623", title = "Panel C"),
  light = list(col = "#4fc3f7", title = "Panel D")
)
for (nm in names(qq_samples)) {
  y_s <- sort(qq_samples[[nm]])
  p_i <- (seq_len(n_qq) - 0.5) / n_qq
  d_qq <- data.frame(theo = qnorm(p_i), samp = y_s)
  # pointwise 95% band from the Beta law of uniform order statistics
  d_qq$lo <- qnorm(qbeta(0.025, seq_len(n_qq), n_qq + 1 - seq_len(n_qq)))
  d_qq$hi <- qnorm(qbeta(0.975, seq_len(n_qq), n_qq + 1 - seq_len(n_qq)))
  st <- qq_style[[nm]]
  p_qq <- ggplot(d_qq, aes(theo, samp)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = st$col, alpha = 0.18) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = "#c9ccd6", linewidth = 0.6) +
    geom_point(colour = st$col, size = 2.4) +
    coord_cartesian(xlim = c(-2.7, 2.7), ylim = c(-4.4, 4.4)) +
    labs(title = st$title, x = expression("theoretical " * Phi^-1 * (p)),
         y = expression("sample " * y[(i)])) +
    theme_minimal(base_size = 18, base_family = "mono") +
    theme(
      plot.background = element_rect(fill = "#12141c", colour = NA),
      panel.background = element_rect(fill = "#1a1d27", colour = NA),
      panel.grid = element_blank(),
      plot.title = element_text(colour = st$col, face = "bold",
                                hjust = 0.5),
      axis.title = element_text(colour = "#e6e8ef"),
      axis.text = element_text(colour = "#9aa0b0")
    )
  ggsave(file.path(out_dir, sprintf("qq-shape-%s.png", nm)), p_qq,
         width = 3.6, height = 4.2, dpi = 200, device = ragg::agg_png,
         bg = "#12141c")
}


# The CLT as repeated convolution: the density of a sum of n independent
# exponential(1) draws, computed numerically, standardised, against N(0, 1).
dx <- 0.01
grid_x <- seq(0, 80, by = dx)
f1 <- dexp(grid_x)
conv_density <- function(f, g) {
  out <- stats::convolve(f, rev(g), type = "open") * dx
  out[seq_along(grid_x)]
}
dens <- list(`1` = f1)
f_n <- f1
for (n_conv in 2:16) {
  f_n <- conv_density(f_n, f1)
  if (n_conv %in% c(2, 4, 16)) dens[[as.character(n_conv)]] <- f_n
}
conv_df <- do.call(rbind, lapply(names(dens), function(k) {
  n_k <- as.numeric(k)
  z <- (grid_x - n_k) / sqrt(n_k)
  keep <- z > -4 & z < 5
  data.frame(n = sprintf("n = %s", k), z = z[keep],
             density = dens[[k]][keep] * sqrt(n_k))
}))
conv_df$n <- factor(conv_df$n, levels = sprintf("n = %s", c(1, 2, 4, 16)))
z_grid <- seq(-4, 5, length.out = 400)
p_conv <- ggplot(conv_df, aes(z, density)) +
  geom_line(data = data.frame(z = z_grid, density = dnorm(z_grid)),
            colour = "#9aa39d", linewidth = 1, linetype = "dashed") +
  geom_line(colour = "#1d4e89", linewidth = 1.2) +
  facet_wrap(~n, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.75)) +
  labs(x = "standardised sum", y = "Density",
       subtitle = "Density of a sum of n exponential(1) draws, by repeated numerical convolution. Dashed: N(0, 1).") +
  theme_slide() +
  theme(plot.subtitle = element_text(size = 11))
save_fig(p_conv, "clt-convolution.png", width = 11, height = 3.6)

message("Wrote figures to ", out_dir)
