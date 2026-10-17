# Illustrative figures for the PEPR Carbon workshop slides.
# Every series here is simulated, except the toxoplasmosis table (Efron, 1986)
# used for the analysis-of-deviance slides. Nothing is a fit to NEON or SPRUCE data.
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

# Analysis of deviance on published data, not a simulation: toxoplasmosis
# positives by city against rainfall (Efron, 1986), as tabulated in Brooms,
# "Analysis of deviance" (2005), references/analysis_of_deviance.pdf.
toxo <- data.frame(
  tested = c(
    4, 10, 5, 10, 2, 5, 8, 19, 6, 10, 24, 1, 30, 22, 1, 11, 1,
    54, 9, 18, 12, 1, 11, 77, 51, 16, 82, 13, 43, 75, 13, 10, 6, 37
  ),
  positive = c(
    2, 3, 1, 3, 2, 3, 2, 7, 3, 8, 7, 0, 15, 4, 0, 6, 0,
    33, 4, 5, 2, 0, 8, 41, 24, 7, 46, 9, 23, 53, 8, 3, 1, 23
  ),
  rain = c(
    1735, 1936, 2000, 1973, 1750, 1800, 1750, 2077, 1920, 1800, 2050, 1830,
    1650, 2200, 2000, 1770, 1920, 1770, 2240, 1620, 1756, 1650, 2250, 1796,
    1890, 1871, 2063, 2100, 1918, 1834, 1780, 1900, 1976, 2292
  )
)
toxo$r <- toxo$rain / 1000 - mean(toxo$rain / 1000)
toxo_null <- glm(cbind(positive, tested - positive) ~ 1, family = binomial, data = toxo)
toxo_cubic <- glm(
  cbind(positive, tested - positive) ~ r + I(r^2) + I(r^3),
  family = binomial, data = toxo
)
toxo_lr <- anova(toxo_null, toxo_cubic, test = "Chisq")
toxo_quasi <- anova(
  update(toxo_null, family = quasibinomial),
  update(toxo_cubic, family = quasibinomial),
  test = "F"
)
toxo_est <- list(
  null_deviance = round(deviance(toxo_null), 2),
  residual_deviance = round(deviance(toxo_cubic), 2),
  df_null = df.residual(toxo_null),
  df_residual = df.residual(toxo_cubic),
  deviance_drop = round(toxo_lr$Deviance[2], 2),
  chisq_p = signif(toxo_lr$`Pr(>Chi)`[2], 2),
  phi_deviance = round(deviance(toxo_cubic) / df.residual(toxo_cubic), 2),
  phi_pearson = round(
    sum(residuals(toxo_cubic, type = "pearson")^2) / df.residual(toxo_cubic), 2
  ),
  quasi_f = round(toxo_quasi$F[2], 2),
  quasi_f_p = signif(toxo_quasi$`Pr(>F)`[2], 2)
)
toxo_grid <- data.frame(rain = seq(min(toxo$rain), max(toxo$rain), length.out = 200))
toxo_grid$r <- toxo_grid$rain / 1000 - mean(toxo$rain / 1000)
toxo_grid$cubic <- predict(toxo_cubic, newdata = toxo_grid, type = "response")
toxo_grid$null <- plogis(coef(toxo_null)[[1]])
p_toxo <- ggplot(toxo, aes(rain, positive / tested)) +
  geom_point(aes(size = tested), colour = "#1d4e89", alpha = 0.55) +
  geom_line(data = toxo_grid, aes(rain, null), colour = "#1c2b24", linetype = "dashed", linewidth = 0.9) +
  geom_line(data = toxo_grid, aes(rain, cubic), colour = "#c45c26", linewidth = 1.3) +
  scale_size_area(max_size = 9, name = "People tested") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Annual rainfall (mm)",
    y = "Proportion testing positive"
  ) +
  theme_slide()
save_fig(p_toxo, "toxo-deviance.png", width = 9, height = 4.6)

payload <- list(
  seed = seed,
  n = n,
  poisson = wald(fit_pois),
  negative_binomial = c(wald(fit_nb), summary_nb),
  logit = wald(fit_bin),
  sd_versus_se = sd_se,
  toxoplasmosis = toxo_est,
  note = "Illustrative simulations. Coefficients are estimates from one seed, not NEON or SPRUCE results. The SD-versus-SE panel uses seed 42. The toxoplasmosis entry is a fit to published data (Efron, 1986), not a simulation."
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
save_lm_pair(fit_ok, fit_lev, 4, "lm-cooks-distance.png")
save_lm_pair(fit_ok, fit_lev, 6, "lm-cooks-leverage.png")

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
mu_f <- 158
mu_h <- 182
sd_f <- 6.5
sd_h <- 7
height <- data.frame(
  sex = rep(c("F", "H"), each = n_height / 2),
  Y = c(
    rnorm(n_height / 2, mu_f, sd_f),
    rnorm(n_height / 2, mu_h, sd_h)
  )
)
# Three ridges: each conditional distribution, then the pooled one. The pooled
# ridge reuses every row, so its mean falls between the two modes.
height_ridges <- rbind(
  data.frame(panel = ifelse(height$sex == "F", "F only", "H only"), Y = height$Y),
  data.frame(panel = "Both sexes pooled", Y = height$Y)
)
height_ridges$panel <- factor(
  height_ridges$panel,
  levels = c("Both sexes pooled", "H only", "F only")
)
height_means <- aggregate(Y ~ panel, data = height_ridges, FUN = mean)
p_height <- ggplot(height_ridges, aes(x = Y, y = panel, fill = panel)) +
  ggridges::geom_density_ridges(
    scale = 0.92,
    alpha = 0.9,
    colour = "#1c2b24",
    linewidth = 0.35,
    quantile_lines = TRUE,
    quantile_fun = function(x, ...) mean(x),
    vline_linetype = "dashed",
    vline_colour = "#1c2b24"
  ) +
  geom_text(
    data = height_means,
    aes(
      x = Y + ifelse(panel == "Both sexes pooled", 1.2, 11),
      y = as.numeric(panel) + ifelse(panel == "Both sexes pooled", 0.62, 0.45),
      label = sprintf("mean %.0f cm", Y)
    ),
    hjust = 0,
    size = 4.2,
    colour = "#1c2b24",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "F only" = "#1d4e89",
      "H only" = "#c45c26",
      "Both sexes pooled" = "#d9d3c7"
    )
  ) +
  scale_y_discrete(expand = expansion(add = c(0.15, 0.95))) +
  labs(
    subtitle = "Illustrative. Seed 20261005. Dashed lines: the mean of each ridge.",
    x = "Height Y (cm)",
    y = NULL
  ) +
  theme_slide() +
  theme(legend.position = "none", panel.grid.major.y = element_blank())
save_fig(p_height, "height-by-sex.png", width = 9.2, height = 4.8)
cat("Height ridge means:", sprintf("%s %.1f", height_means$panel, height_means$Y), "\n")

# Four views of one distribution, each as a pair: a fair die on the left
# (discrete), a standard Gaussian on the right (continuous). One figure per slide.
panel_theme <- function() {
  theme_slide() +
    theme(
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
}
save_pair <- function(p_left, p_right, filename, width = 8.6, height = 3.3) {
  ragg::agg_png(
    file.path(out_dir, filename),
    width = width, height = height, units = "in", res = 200,
    background = "white"
  )
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
  print(p_left, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_right, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 2))
  invisible(grDevices::dev.off())
}
die <- data.frame(k = 1:6, p = rep(1 / 6, 6))
z_grid <- seq(-4, 4, length.out = 401)
q05 <- qnorm(0.05)

# 1. Mass and density. Shaded: Pr(2 <= Y <= 4) and Pr(-1 <= Z <= 1).
die$in_band <- die$k >= 2 & die$k <= 4
band <- data.frame(
  z = c(-1, seq(-1, 1, length.out = 80), 1),
  y = c(0, dnorm(seq(-1, 1, length.out = 80)), 0)
)
p_die_d <- ggplot(die, aes(k, p, colour = in_band)) +
  geom_segment(aes(xend = k, yend = 0), linewidth = 1.6) +
  geom_point(size = 3) +
  scale_colour_manual(values = c(`FALSE` = "#1d4e89", `TRUE` = "#c45c26"), guide = "none") +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(limits = c(0, 0.25)) +
  labs(
    title = "Die: a mass at each face",
    subtitle = "Pr(2 ≤ Y ≤ 4) = 3/6",
    x = "Face k", y = "p(k)"
  ) +
  panel_theme()
p_norm_d <- ggplot(data.frame(z = z_grid, y = dnorm(z_grid)), aes(z, y)) +
  geom_polygon(data = band, fill = "#c45c26", alpha = 0.35) +
  geom_line(colour = "#1d4e89", linewidth = 1.1) +
  labs(
    title = "Gaussian: a density, dnorm",
    subtitle = sprintf("Pr(−1 ≤ Z ≤ 1) = %.2f, the shaded area", diff(pnorm(c(-1, 1)))),
    x = "z", y = "f(z)"
  ) +
  panel_theme()
save_pair(p_die_d, p_norm_d, "distribution-density.png")

# 2. Cumulative distribution. Marked: F(4) = 4/6 and F(-1.64) = 0.05.
die_cdf <- data.frame(k = 0:6, F = c(0, cumsum(die$p)))
p_die_p <- ggplot() +
  geom_segment(
    data = data.frame(x = 0:6, xend = c(1:6, 7), F = die_cdf$F),
    aes(x = x, xend = xend, y = F, yend = F),
    colour = "#1d4e89", linewidth = 1.1
  ) +
  geom_point(data = die_cdf[-1, ], aes(k, F), colour = "#1d4e89", size = 2.4) +
  geom_point(
    data = data.frame(k = 1:6, F = die_cdf$F[1:6]), aes(k, F),
    shape = 21, fill = "white", colour = "#1d4e89", size = 2.4
  ) +
  annotate("segment", x = 4, xend = 4, y = 0, yend = 4 / 6, linetype = "dashed", colour = "#1c2b24") +
  annotate("point", x = 4, y = 4 / 6, colour = "#c45c26", size = 3) +
  scale_x_continuous(breaks = 0:7, limits = c(0, 7)) +
  labs(title = "Die: a staircase", subtitle = "F(4) = 4/6", x = "y", y = "F(y)") +
  panel_theme()
p_norm_p <- ggplot(data.frame(z = z_grid, y = pnorm(z_grid)), aes(z, y)) +
  geom_line(colour = "#1d4e89", linewidth = 1.1) +
  annotate("segment", x = q05, xend = q05, y = 0, yend = 0.05, linetype = "dashed", colour = "#1c2b24") +
  annotate("segment", x = -4, xend = q05, y = 0.05, yend = 0.05, linetype = "dashed", colour = "#1c2b24") +
  annotate("point", x = q05, y = 0.05, colour = "#c45c26", size = 3) +
  labs(
    title = "Gaussian: a smooth curve, pnorm",
    subtitle = sprintf("F(%.2f) = 0.05", q05),
    x = "z", y = "F(z)"
  ) +
  panel_theme()
save_pair(p_die_p, p_norm_p, "distribution-cumulative.png")

# 3. Quantile function, the inverse of F. Marked: the die median and q(0.05).
alpha_grid <- seq(0.001, 0.999, length.out = 999)
die_q <- data.frame(a0 = (0:5) / 6, a1 = (1:6) / 6, q = 1:6)
p_die_q <- ggplot(die_q) +
  geom_segment(aes(x = a0, xend = a1, y = q, yend = q), colour = "#1d4e89", linewidth = 1.1) +
  annotate("segment", x = 0.5, xend = 0.5, y = 0.5, yend = 3, linetype = "dashed", colour = "#1c2b24") +
  annotate("point", x = 0.5, y = 3, colour = "#c45c26", size = 3) +
  scale_y_continuous(breaks = 1:6, limits = c(0.5, 6.2)) +
  labs(title = "Die: the face reached", subtitle = "q(0.5) = 3, the median", x = "α", y = "q(α)") +
  panel_theme()
p_norm_q <- ggplot(data.frame(a = alpha_grid, q = qnorm(alpha_grid)), aes(a, q)) +
  geom_line(colour = "#1d4e89", linewidth = 1.1) +
  annotate("segment", x = 0.05, xend = 0.05, y = -3.2, yend = q05, linetype = "dashed", colour = "#1c2b24") +
  annotate("point", x = 0.05, y = q05, colour = "#c45c26", size = 3) +
  coord_cartesian(ylim = c(-3.2, 3.2)) +
  labs(
    title = "Gaussian: qnorm",
    subtitle = sprintf("q(0.05) = %.2f", q05),
    x = "α", y = "q(α)"
  ) +
  panel_theme()
save_pair(p_die_q, p_norm_q, "distribution-quantile.png")

# 4. Simulation by inversion: Y = q(U), U uniform on (0, 1).
set.seed(seed)
u_die <- runif(600)
rolls <- ceiling(6 * u_die)
roll_freq <- as.data.frame(table(factor(rolls, levels = 1:6)) / length(rolls))
names(roll_freq) <- c("k", "share")
roll_freq$k <- as.integer(as.character(roll_freq$k))
u_norm <- runif(400)
draws <- data.frame(z = qnorm(u_norm))
p_die_r <- ggplot(roll_freq, aes(k, share)) +
  geom_col(fill = "#d9d3c7", colour = "white", width = 0.8) +
  geom_hline(yintercept = 1 / 6, linetype = "dashed", colour = "#1d4e89", linewidth = 0.9) +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(limits = c(0, 0.25)) +
  labs(title = "Die: 600 rolls", subtitle = "Dashed line: 1/6. Seed 20261005.", x = "Face k", y = "Share of rolls") +
  panel_theme()
p_norm_r <- ggplot(draws, aes(z)) +
  geom_histogram(aes(y = after_stat(density)), bins = 24, fill = "#d9d3c7", colour = "white", linewidth = 0.2) +
  geom_line(
    data = data.frame(z = z_grid, y = dnorm(z_grid)), aes(z, y),
    colour = "#1d4e89", linewidth = 1.1
  ) +
  labs(title = "Gaussian: 400 draws, rnorm", subtitle = "Curve: the density. Seed 20261005.", x = "z", y = "Density") +
  panel_theme()
save_pair(p_die_r, p_norm_r, "distribution-sampling.png")
cat("Die roll shares:", round(roll_freq$share, 3), "\n")


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

# Decision curves: net benefit of acting on a score at threshold t,
# NB(t) = TP/n - FP/n * t / (1 - t) (Vickers and Elkin, 2006). Simulated sites.
set.seed(seed)
n_dec <- 4000L
x_dec <- rnorm(n_dec)
pi_dec <- plogis(-1.1 + 1.6 * x_dec)
y_dec <- rbinom(n_dec, 1L, pi_dec)
score_good <- pi_dec
score_bad <- plogis(1.5 + 0.1 * qlogis(pi_dec) + rnorm(n_dec, sd = 1.2))
net_benefit <- function(score, y, t) {
  vapply(t, function(tt) {
    act <- score >= tt
    sum(act & y == 1L) / length(y) - sum(act & y == 0L) / length(y) * tt / (1 - tt)
  }, numeric(1))
}
t_dec <- seq(0.02, 0.8, by = 0.01)
prev_dec <- mean(y_dec)
nb_curves <- rbind(
  data.frame(
    panel = "A miscalibrated, weak score",
    t = t_dec,
    score = net_benefit(score_bad, y_dec, t_dec)
  ),
  data.frame(
    panel = "A calibrated, informative score",
    t = t_dec,
    score = net_benefit(score_good, y_dec, t_dec)
  )
)
nb_curves$panel <- factor(nb_curves$panel, levels = unique(nb_curves$panel))
nb_curves$all <- prev_dec - (1 - prev_dec) * nb_curves$t / (1 - nb_curves$t)
nb_curves$best_ref <- pmax(nb_curves$all, 0)
nb_curves$beats <- nb_curves$score > nb_curves$best_ref
p_nb <- ggplot(nb_curves, aes(t)) +
  geom_ribbon(
    aes(ymin = best_ref, ymax = ifelse(beats, score, best_ref)),
    fill = "#1d4e89", alpha = 0.18
  ) +
  geom_hline(yintercept = 0, colour = "#1c2b24", linewidth = 0.7) +
  geom_line(aes(y = all), colour = "#1c2b24", linetype = "dashed", linewidth = 0.7) +
  geom_line(aes(y = score, colour = panel), linewidth = 1.3) +
  annotate("text", x = 0.62, y = 0.012, label = "act on none", size = 3.6, colour = "#1c2b24") +
  annotate("text", x = 0.4, y = -0.045, label = "act on all", size = 3.6, colour = "#1c2b24", hjust = 0) +
  facet_wrap(~panel) +
  scale_colour_manual(values = c("#c45c26", "#1d4e89"), guide = "none") +
  coord_cartesian(ylim = c(-0.06, prev_dec + 0.02)) +
  labs(
    subtitle = sprintf(
      "Illustrative: %d simulated sites, prevalence %.2f. Seed 20261005. Shaded: the score beats both references.",
      n_dec, prev_dec
    ),
    x = "Decision threshold t",
    y = "Net benefit"
  ) +
  theme_slide() +
  theme(plot.subtitle = element_text(size = 11))
save_fig(p_nb, "utility-net-benefit.png", width = 9.6, height = 4.2)
cat(
  "Net benefit: bad score max excess over best reference =",
  round(max(nb_curves$score[nb_curves$panel == levels(nb_curves$panel)[1]] -
    nb_curves$best_ref[nb_curves$panel == levels(nb_curves$panel)[1]]), 4),
  "; good score beats both for t in",
  paste(range(nb_curves$t[nb_curves$panel == levels(nb_curves$panel)[2] & nb_curves$beats]), collapse = "-"),
  "\n"
)

# Predictive probability of success for the next trial (Saint-Hilary,
# Robert and Gasparini, 2018, section 3): d* | data ~ N(d, s^2 + s*^2), from
# the phase 2 summaries. Significance needs d* > c = 1.96 s*, clinical
# relevance d* > d_T = 3 points on HAM-D17. Exact normal formula, no simulation.
s_star <- 0.93
pos_dose <- data.frame(
  dose = c("Low dose", "High dose"),
  d = c(16.9 - 14.0, 16.9 - 12.6),
  s = c(1.37, 1.41)
)
pos_dose$sd_pred <- sqrt(pos_dose$s^2 + s_star^2)
c_sig <- 1.96 * s_star
d_rel <- 3
pos_dose$ppos1 <- 1 - pnorm((c_sig - pos_dose$d) / pos_dose$sd_pred)
pos_dose$ppos2 <- 1 - pnorm((d_rel - pos_dose$d) / pos_dose$sd_pred)
d_grid <- seq(-3, 10, length.out = 500)
pos_curves <- do.call(rbind, lapply(seq_len(nrow(pos_dose)), function(k) {
  data.frame(
    dose = pos_dose$dose[k],
    d = d_grid,
    density = dnorm(d_grid, pos_dose$d[k], pos_dose$sd_pred[k])
  )
}))
pos_curves$dose <- factor(pos_curves$dose, levels = pos_dose$dose)
p_pos <- ggplot(pos_curves, aes(d, density, colour = dose)) +
  geom_area(
    data = subset(pos_curves, d > d_rel),
    aes(fill = dose), alpha = 0.15, position = "identity", colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = c(c_sig, d_rel), linetype = "dashed", colour = "#1c2b24") +
  annotate("text", x = c_sig - 0.15, y = 0.3, label = "significance\nc = 1.82", hjust = 1, size = 4.6, colour = "#1c2b24") +
  annotate("text", x = d_rel + 0.15, y = 0.3, label = "clinical relevance\nd_T = 3", hjust = 0, size = 4.6, colour = "#1c2b24") +
  scale_colour_manual(values = c("Low dose" = "#1d4e89", "High dose" = "#c45c26")) +
  scale_fill_manual(values = c("Low dose" = "#1d4e89", "High dose" = "#c45c26"), guide = "none") +
  scale_y_continuous(limits = c(0, 0.33)) +
  labs(
    subtitle = sprintf(
      "Pr(d* > c): low %.0f%%, high %.0f%%.  Pr(d* > 3), shaded: low %.0f%%, high %.0f%%.",
      100 * pos_dose$ppos1[1], 100 * pos_dose$ppos1[2],
      100 * pos_dose$ppos2[1], 100 * pos_dose$ppos2[2]
    ),
    x = "Next-trial difference versus placebo, d* (HAM-D17 points)",
    y = "Predictive density",
    colour = NULL
  ) +
  theme_slide() +
  theme(plot.subtitle = element_text(size = 14))
save_fig(p_pos, "decision-predictive-success.png", width = 7.4, height = 3.7)
cat("PPoS1:", round(pos_dose$ppos1, 3), " PPoS2:", round(pos_dose$ppos2, 3), "\n")

# Linear MCDA loss against Scale Loss Score (Saint-Hilary et al., 2019,
# equations 4 and 6), one benefit u1 and one risk 1 - u2, weight 0.25 on benefit.
w_br <- 0.25
br_grid <- expand.grid(
  benefit = seq(0.005, 1, length.out = 200),
  risk = seq(0, 0.995, length.out = 200)
)
br_grid$linear <- 1 - (w_br * br_grid$benefit + (1 - w_br) * (1 - br_grid$risk))
br_grid$slos <- (1 / br_grid$benefit)^w_br + (1 / (1 - br_grid$risk))^(1 - w_br)
br_drugs <- data.frame(
  drug = c("Drug 1", "Drug 2"),
  benefit = c(0, 0.30),
  risk = c(0.09, 0.20)
)
br_drugs$linear <- 1 - (w_br * br_drugs$benefit + (1 - w_br) * (1 - br_drugs$risk))
br_drugs$slos <- (1 / br_drugs$benefit)^w_br + (1 / (1 - br_drugs$risk))^(1 - w_br)
br_panel <- function(fill_var, title, subtitle, limits, breaks) {
  ggplot(br_grid, aes(benefit, risk)) +
    geom_raster(aes(fill = pmin(.data[[fill_var]], max(limits)))) +
    geom_contour(aes(z = .data[[fill_var]]), breaks = breaks, colour = "white", linewidth = 0.4) +
    geom_point(data = br_drugs, colour = "#1c2b24", fill = "white", shape = 21, size = 3.2, stroke = 1.1) +
    geom_text(data = br_drugs, aes(label = drug), nudge_x = 0.04, nudge_y = 0.05, hjust = 0, size = 3.8, colour = "white") +
    scale_fill_viridis_c(direction = -1, limits = limits, oob = scales::squish, name = "Loss") +
    coord_cartesian(expand = FALSE) +
    labs(title = title, subtitle = subtitle, x = "Benefit u1", y = "Risk 1 − u2") +
    panel_theme() +
    theme(legend.position = "right", legend.title = element_text(size = 11), legend.text = element_text(size = 9))
}
p_linear <- br_panel(
  "linear", "Linear MCDA loss",
  sprintf("Drug 1 %.3f < Drug 2 %.3f: drug 1 wins", br_drugs$linear[1], br_drugs$linear[2]),
  c(0, 1), seq(0.1, 0.9, by = 0.1)
)
p_slos <- br_panel(
  "slos", "Scale Loss Score",
  sprintf("Drug 1 infinite, Drug 2 %.2f: drug 2 wins", br_drugs$slos[2]),
  c(2, 5), seq(2.2, 4.8, by = 0.4)
)
save_pair(p_linear, p_slos, "decision-linear-vs-slos.png", width = 9.6, height = 3.8)
cat("Linear loss drugs:", round(br_drugs$linear, 4), " SLoS drug 2:", round(br_drugs$slos[2], 3), "\n")

# sctransform-style regularised negative binomial regression on a simulated
# UMI matrix (Hafemeister and Satija, 2019; v2 regularisation, Choudhary and
# Satija, 2022). Illustrative: genes and cells are simulated, not a dataset.
set.seed(seed)
n_cell <- 800L
n_gene <- 1500L
sct_depth <- round(10^rnorm(n_cell, 3.6, 0.25))
sct_share <- 10^runif(n_gene, -5.5, -1.6)
sct_mean_true <- sct_share * mean(sct_depth)
sct_theta_true <- 10^(0.3 + 0.45 * log10(sct_mean_true)) * exp(rnorm(n_gene, 0, 0.25))
sct_mu_true <- outer(sct_share, sct_depth)
sct_y <- matrix(
  rnbinom(length(sct_mu_true), mu = sct_mu_true, size = rep(sct_theta_true, n_cell)),
  nrow = n_gene
)

# Step 1, per gene: offset model log mu = beta_0g + log n_c (slope fixed at 1),
# whose maximum-likelihood intercept is log(sum_c y / sum_c n_c); then theta by
# maximum likelihood for genes with evidence of overdispersion.
sct_rate <- rowSums(sct_y) / sum(sct_depth)
sct_mu_hat <- outer(sct_rate, sct_depth)
sct_amean <- rowMeans(sct_y)
sct_var <- apply(sct_y, 1, var)
sct_gmean <- exp(rowMeans(log1p(sct_y))) - 1
sct_keep <- sct_amean >= 0.001 & sct_var > sct_amean
sct_theta_hat <- rep(Inf, n_gene)
for (g in which(sct_keep)) {
  sct_theta_hat[g] <- tryCatch(
    suppressWarnings(MASS::theta.ml(sct_y[g, ], sct_mu_hat[g, ], limit = 100)),
    error = function(e) NA_real_
  )
}
sct_fit_ok <- sct_keep & is.finite(sct_theta_hat) & sct_theta_hat < 1e4

# Step 2: regularise theta against gene abundance (geometric mean), loess on the
# log10 scale. Genes without overdispersion keep theta = Inf (Poisson).
sct_loess <- loess(
  ly ~ lx,
  data = data.frame(lx = log10(sct_gmean[sct_fit_ok]), ly = log10(sct_theta_hat[sct_fit_ok])),
  span = 0.5
)
sct_theta_reg <- rep(Inf, n_gene)
sct_theta_reg[sct_keep] <- 10^predict(
  sct_loess,
  newdata = data.frame(lx = log10(sct_gmean[sct_keep]))
)
sct_theta_reg[sct_keep & !is.finite(sct_theta_reg)] <- Inf

# Step 3: Pearson residuals, clipped at sqrt(number of cells).
sct_clip <- sqrt(n_cell)
pearson_var <- function(theta) {
  sd_hat <- sqrt(sct_mu_hat + sct_mu_hat^2 / theta)
  r <- (sct_y - sct_mu_hat) / sd_hat
  r <- pmin(pmax(r, -sct_clip), sct_clip)
  apply(r, 1, var)
}
sct_resvar_nb <- pearson_var(matrix(sct_theta_reg, n_gene, n_cell))
sct_resvar_pois <- pearson_var(Inf)

sct_genes <- data.frame(
  amean = sct_amean,
  gmean = sct_gmean,
  var = sct_var,
  theta_hat = sct_theta_hat,
  theta_reg = sct_theta_reg,
  theta_true = sct_theta_true,
  resvar_nb = sct_resvar_nb,
  resvar_pois = sct_resvar_pois,
  keep = sct_keep
)

# Panel B: one abundant gene, counts against depth, slope fixed at 1.
g_top <- which.max(sct_amean)
sct_top <- data.frame(
  depth = sct_depth,
  y = sct_y[g_top, ],
  mu = sct_mu_hat[g_top, ]
)
p_sct_b1 <- ggplot(sct_top, aes(log10(depth), log10(y))) +
  geom_point(colour = "#c45c26", alpha = 0.45, size = 1.4) +
  geom_abline(
    intercept = log10(sct_rate[g_top]), slope = 1,
    colour = "#1d4e89", linewidth = 1.1
  ) +
  labs(
    title = "Counts against depth",
    subtitle = "Line: slope fixed at 1, intercept fitted",
    x = "log10 total UMI per cell", y = "log10 UMI of the gene"
  ) +
  panel_theme()
p_sct_b2 <- ggplot(sct_top, aes(log10(depth), log10(y / mu))) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#1c2b24") +
  geom_point(colour = "#1d4e89", alpha = 0.45, size = 1.4) +
  labs(
    title = "After the offset",
    subtitle = "No trend left with depth",
    x = "log10 total UMI per cell", y = "log10 (count / fitted mean)"
  ) +
  panel_theme()
save_pair(p_sct_b1, p_sct_b2, "sct-offset.png", width = 9, height = 3.6)

# Panel C: variance of the raw counts against variance of Pearson residuals.
p_sct_c1 <- ggplot(subset(sct_genes, amean > 0), aes(amean, var)) +
  geom_point(colour = "#c45c26", alpha = 0.35, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "#1c2b24") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Raw counts",
    subtitle = "Variance grows with the mean; dashed: Poisson",
    x = "Gene mean (UMI per cell)", y = "Variance"
  ) +
  panel_theme()
sct_resid_long <- rbind(
  data.frame(amean = sct_amean, resvar = sct_resvar_pois, model = "Poisson residuals"),
  data.frame(amean = sct_amean, resvar = sct_resvar_nb, model = "Regularised NB residuals")
)
sct_resid_long <- subset(sct_resid_long, amean > 0)
p_sct_c2 <- ggplot(sct_resid_long, aes(amean, resvar, colour = model)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "#1c2b24") +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_manual(values = c("Poisson residuals" = "#9aa0a6", "Regularised NB residuals" = "#1d4e89")) +
  labs(
    title = "Pearson residuals",
    subtitle = "Variance near 1 at every mean",
    x = "Gene mean (UMI per cell)", y = "Residual variance", colour = NULL
  ) +
  panel_theme() +
  theme(legend.text = element_text(size = 10))
save_pair(p_sct_c1, p_sct_c2, "sct-pearson.png", width = 9, height = 3.8)

# Panel D: per-gene theta estimates, the loess trend, and the true curve.
sct_fit_genes <- subset(sct_genes, is.finite(theta_hat) & theta_hat < 1e4 & keep)
sct_grid <- data.frame(lx = seq(
  log10(min(sct_fit_genes$gmean)), log10(max(sct_fit_genes$gmean)),
  length.out = 200
))
sct_grid$sct_gmean <- 10^sct_grid$lx
sct_grid$theta_reg <- 10^predict(sct_loess, newdata = sct_grid)
p_sct_d <- ggplot(sct_fit_genes, aes(gmean, theta_hat)) +
  geom_point(colour = "#9aa0a6", alpha = 0.5, size = 1.1) +
  geom_line(data = sct_grid, aes(sct_gmean, theta_reg), colour = "#6a3d9a", linewidth = 1.4) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    subtitle = "Grey: one maximum-likelihood estimate per gene. Purple: the loess trend, used as the regularised theta.",
    x = "Gene abundance (geometric mean, UMI per cell)",
    y = expression(hat(theta)[g])
  ) +
  theme_slide() +
  theme(plot.subtitle = element_text(size = 11))
save_fig(p_sct_d, "sct-theta-regularisation.png", width = 8.6, height = 4.2)
cat(
  "sctransform sim: genes fitted", nrow(sct_fit_genes),
  "; excluded", sum(!sct_keep),
  "; median NB residual variance", round(median(sct_resvar_nb[sct_keep]), 2),
  "; median Poisson residual variance (top decile mean)",
  round(median(sct_resvar_pois[sct_amean > quantile(sct_amean, 0.9)]), 2), "\n"
)
