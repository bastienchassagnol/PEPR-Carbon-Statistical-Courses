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

message("Wrote figures to ", out_dir)
