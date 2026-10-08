# Figures fitted to the two workshop datasets. Nothing here is simulated.
#
#   Rscript scripts/prepare-teaching-data.R
#   Rscript scripts/generate-data-figures.R
#
# Every number that appears on a slide is written to
# figures/generated/data-estimates.json by this script.

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(MASS)
  library(patchwork)
  library(jsonlite)
})
source("scripts/slide-theme.R")

out_dir <- "figures/generated"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

casys <- read.csv("data/derived/casys_teaching.csv", stringsAsFactors = FALSE) |>
  mutate(
    system = factor(system, levels = c("SD1", "SD2", "TS1", "TS2")),
    year_f = factor(year),
    tillage = factor(tillage, levels = c("No-till", "Tilled"))
  )
spruce <- read.csv("data/derived/spruce_teaching.csv", stringsAsFactors = FALSE)

est <- list()

# The OTU table is needed for the per-OTU mean-variance cloud.
ps <- readRDS("data/16s_RNA_sequencing/casys_phyloseq_clean.rds")
otu <- unclass(attributes(ps)$otu_table)
attr(otu, "taxa_are_rows") <- NULL

# ----------------------------------------------- 1. sparsity and library size
prevalence <- rowMeans(otu > 0)
lib <- colSums(otu)

p_prev <- ggplot(data.frame(prevalence), aes(prevalence)) +
  geom_histogram(bins = 40, fill = "#0e4d3a", colour = "white") +
  scale_y_continuous(trans = "log1p", breaks = c(0, 10, 100, 1000, 5000)) +
  labs(
    title = "Most OTUs are absent from most samples",
    subtitle = sprintf("%.1f%% of the 10,633 x 595 entries are zero",
                       100 * mean(otu == 0)),
    x = "Fraction of samples where the OTU is present",
    y = "OTUs (log1p scale)"
  ) +
  theme_slide()

p_lib <- ggplot(data.frame(lib), aes(lib)) +
  geom_histogram(bins = 40, fill = "#1d4e89", colour = "white") +
  labs(
    title = "Sampling effort is not constant",
    subtitle = sprintf("Library size from %s to %s reads per sample",
                       format(min(lib), big.mark = ","),
                       format(max(lib), big.mark = ",")),
    x = "Reads retained per sample", y = "Samples"
  ) +
  theme_slide()

save_fig(p_prev | p_lib, "casys-sparsity.png", width = 12.5, height = 5.0)

est$casys_zero_fraction <- round(mean(otu == 0), 4)
est$casys_library_min <- as.integer(min(lib))
est$casys_library_max <- as.integer(max(lib))
est$casys_library_median <- as.integer(median(lib))

# ------------------------------------------------- 2. mean against variance
otu_mv <- data.frame(
  mean = rowMeans(otu),
  variance = apply(otu, 1, var)
) |>
  filter(mean > 0, variance > 0)

p_mv <- ggplot(otu_mv, aes(mean, variance)) +
  geom_point(alpha = 0.18, colour = "#1d4e89", size = 0.9) +
  geom_abline(slope = 1, intercept = 0, colour = "#8a5a00", linewidth = 1.1) +
  annotate("text", x = 0.05, y = 40000, hjust = 0,
           label = "Poisson: variance = mean", colour = "#8a5a00", size = 5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Nearly every OTU sits above the Poisson line",
    subtitle = sprintf(
      "%s of %s OTUs have variance above the mean, across the 595 samples",
      format(sum(otu_mv$variance > otu_mv$mean), big.mark = ","),
      format(nrow(otu_mv), big.mark = ",")
    ),
    x = "Mean reads per sample", y = "Variance across samples"
  ) +
  theme_slide()

save_fig(p_mv, "casys-mean-variance-real.png", width = 9.4, height = 5.4)

est$casys_otus_above_poisson_line <- as.integer(sum(otu_mv$variance > otu_mv$mean))
est$casys_otus_with_variance <- as.integer(nrow(otu_mv))

# ------------------------------------------------ 3. richness is overdispersed
fit_rich_pois <- glm(richness ~ system + year_f, family = poisson, data = casys)
disp_rich <- sum(residuals(fit_rich_pois, "pearson")^2) / fit_rich_pois$df.residual

p_rich <- ggplot(casys, aes(system, richness, colour = system)) +
  geom_boxplot(outlier.shape = NA, width = 0.6, colour = "#3d4a42") +
  geom_jitter(width = 0.18, alpha = 0.45, size = 1.1) +
  facet_wrap(~year, nrow = 1) +
  scale_colour_manual(values = c("#0e4d3a", "#2f8f6b", "#1d4e89", "#5c3d7a")) +
  guides(colour = "none") +
  labs(
    title = "Observed richness, by cropping system and year",
    subtitle = sprintf(
      paste(
        "A Poisson GLM on these counts gives Pearson chi-square / df = %.0f.",
        "\n2018 was sampled before the treatments were applied."
      ),
      disp_rich
    ),
    x = NULL, y = "OTUs observed"
  ) +
  theme_slide()

save_fig(p_rich, "casys-richness-system.png", width = 12.0, height = 5.2)

est$casys_richness_dispersion <- round(disp_rich, 1)
est$casys_richness_min <- as.integer(min(casys$richness))
est$casys_richness_max <- as.integer(max(casys$richness))

# --------------------------------- 4. the same mean, two variance assumptions
fit_pois <- glm(
  nitrospira_reads ~ tillage + year_f + offset(log(total_reads)),
  family = poisson, data = casys
)
fit_nb <- glm.nb(
  nitrospira_reads ~ tillage + year_f + offset(log(total_reads)),
  data = casys
)
disp_pois <- sum(residuals(fit_pois, "pearson")^2) / fit_pois$df.residual

ci <- function(fit, term) {
  b <- unname(coef(fit)[term])
  s <- unname(sqrt(diag(vcov(fit)))[term])
  c(est = exp(b), lo = exp(b - 1.96 * s), hi = exp(b + 1.96 * s), se = s)
}
cmp <- bind_rows(
  as.data.frame(t(ci(fit_pois, "tillageTilled"))) |> mutate(family = "Poisson"),
  as.data.frame(t(ci(fit_nb, "tillageTilled"))) |> mutate(family = "Negative binomial")
) |>
  mutate(family = factor(family, levels = c("Poisson", "Negative binomial")))

p_cmp <- ggplot(cmp, aes(est, family, colour = family)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "#8a5a00") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                width = 0.14, linewidth = 1.2) +
  geom_point(size = 4) +
  scale_colour_manual(values = c("#b03030", "#0e4d3a")) +
  guides(colour = "none") +
  labs(
    title = "One mean model, two variance assumptions",
    subtitle = sprintf(
      paste(
        "Nitrospira reads, log link, offset log(library size).",
        "Poisson chi-square / df = %.1f.\nThe negative binomial interval is %.1f times wider."
      ),
      disp_pois, cmp$se[2] / cmp$se[1]
    ),
    x = "Rate ratio, tilled against no-till", y = NULL
  ) +
  theme_slide()

save_fig(p_cmp, "casys-poisson-vs-nb.png", width = 11.0, height = 4.4)

est$casys_nitrospira_dispersion <- round(disp_pois, 1)
est$casys_nitrospira_nb_theta <- round(fit_nb$theta, 1)
est$casys_nitrospira_se_ratio <- round(cmp$se[2] / cmp$se[1], 1)
est$casys_nitrospira_rr_pois <- round(cmp$est[1], 3)
est$casys_nitrospira_rr_pois_lo <- round(cmp$lo[1], 3)
est$casys_nitrospira_rr_pois_hi <- round(cmp$hi[1], 3)
est$casys_nitrospira_rr_nb <- round(cmp$est[2], 3)
est$casys_nitrospira_rr_nb_lo <- round(cmp$lo[2], 3)
est$casys_nitrospira_rr_nb_hi <- round(cmp$hi[2], 3)
est$casys_nitrospira_total <- as.integer(sum(casys$nitrospira_reads))

# ------------------------------------------- 5. detection depends on effort
casys$log_lib_z <- as.numeric(scale(log(casys$total_reads)))
fit_logit <- glm(
  methylobacter_present ~ log_lib_z + tillage,
  family = binomial, data = casys
)
grid <- expand.grid(
  log_lib_z = seq(min(casys$log_lib_z), max(casys$log_lib_z), length.out = 120),
  tillage = factor(c("No-till", "Tilled"), levels = levels(casys$tillage))
)
pr <- predict(fit_logit, newdata = grid, type = "link", se.fit = TRUE)
grid$fit <- plogis(pr$fit)
grid$lo <- plogis(pr$fit - 1.96 * pr$se.fit)
grid$hi <- plogis(pr$fit + 1.96 * pr$se.fit)

p_logit <- ggplot(grid, aes(log_lib_z, fit, colour = tillage, fill = tillage)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(
    data = casys,
    aes(log_lib_z, methylobacter_present, colour = tillage),
    inherit.aes = FALSE, alpha = 0.25, size = 1.2,
    position = position_jitter(height = 0.03, width = 0)
  ) +
  scale_colour_manual(values = c("#0e4d3a", "#1d4e89")) +
  scale_fill_manual(values = c("#0e4d3a", "#1d4e89")) +
  labs(
    title = "Methylobacter is detected more often in deeper libraries",
    subtitle = sprintf(
      paste(
        "Logit on presence. Odds ratio %.2f per standard deviation of log library size.",
        "\nPrevalence across the 595 samples: %.0f%%."
      ),
      exp(coef(fit_logit)["log_lib_z"]), 100 * mean(casys$methylobacter_present)
    ),
    x = "log library size, standardised", y = "P(detected)",
    colour = NULL, fill = NULL
  ) +
  theme_slide()

save_fig(p_logit, "casys-logit-detection.png", width = 10.5, height = 5.4)

est$casys_methylobacter_prevalence <- round(mean(casys$methylobacter_present), 3)
est$casys_methylobacter_or_lib <- round(exp(coef(fit_logit)["log_lib_z"]), 2)
or_till <- ci(fit_logit, "tillageTilled")
est$casys_methylobacter_or_tillage <- round(or_till[["est"]], 2)
est$casys_methylobacter_or_tillage_lo <- round(or_till[["lo"]], 2)
est$casys_methylobacter_or_tillage_hi <- round(or_till[["hi"]], 2)

# --------------------------------------- 6. the wrong family, on a QQ plot
fit_lm_counts <- lm(nitrospira_reads ~ tillage + year_f, data = casys)
qr_nb <- {
  mu <- fitted(fit_nb)
  size <- fit_nb$theta
  y <- casys$nitrospira_reads
  lower <- pnbinom(y - 1, mu = mu, size = size)
  upper <- pnbinom(y, mu = mu, size = size)
  set.seed(20261005L)
  qnorm(runif(length(y), pmin(lower, 1 - 1e-10), pmin(upper, 1 - 1e-10)))
}

qq_df <- bind_rows(
  data.frame(
    panel = "Gaussian lm on the raw counts",
    residual = as.numeric(scale(residuals(fit_lm_counts)))
  ),
  data.frame(
    panel = "Negative binomial, quantile residual",
    residual = qr_nb[is.finite(qr_nb)]
  )
)

p_qq <- ggplot(qq_df, aes(sample = residual)) +
  stat_qq_line(colour = "#8a5a00", linewidth = 1) +
  stat_qq(alpha = 0.45, size = 1.1, colour = "#1d4e89") +
  facet_wrap(~panel) +
  labs(
    title = "The same response, two families",
    subtitle = "Nitrospira reads. Left, a Gaussian linear model on the raw counts.\nRight, the negative binomial with the library-size offset.",
    x = "Theoretical quantile", y = "Observed quantile"
  ) +
  theme_slide()

save_fig(p_qq, "casys-qq-family.png", width = 11.5, height = 5.0)

# --------------------------------- 7. the practice contrast and where it sits
zone_tab <- casys |>
  group_by(zone, tillage) |>
  summarise(n = dplyr::n(), mean_richness = mean(richness), .groups = "drop")

crude <- t.test(richness ~ tillage, data = casys)
mix <- casys |> filter(zone == "MIX")
inside <- t.test(richness ~ tillage, data = mix)

p_zone <- ggplot(casys, aes(tillage, richness, colour = tillage)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, colour = "#3d4a42") +
  geom_jitter(width = 0.16, alpha = 0.35, size = 1.1) +
  facet_wrap(~zone, nrow = 1,
             labeller = labeller(zone = c(
               MIX = "Mixed zone (both practices)",
               SD = "No-till zone",
               TS = "Tilled zone"
             ))) +
  scale_colour_manual(values = c("#0e4d3a", "#1d4e89")) +
  guides(colour = "none") +
  labs(
    title = "The practice contrast is partly a contrast between zones",
    subtitle = sprintf(
      paste(
        "No-till minus tilled: %+.0f OTUs over all 595 samples (95%% CI %.0f to %.0f).",
        "\nInside the mixed zone, where both practices occur: %+.0f (95%% CI %.0f to %.0f)."
      ),
      diff(rev(crude$estimate)), crude$conf.int[1], crude$conf.int[2],
      diff(rev(inside$estimate)), inside$conf.int[1], inside$conf.int[2]
    ),
    x = NULL, y = "OTUs observed"
  ) +
  theme_slide()

save_fig(p_zone, "casys-tillage-zone.png", width = 12.0, height = 5.2)

est$casys_tillage_crude <- round(as.numeric(diff(rev(crude$estimate))), 1)
est$casys_tillage_crude_lo <- round(crude$conf.int[1], 1)
est$casys_tillage_crude_hi <- round(crude$conf.int[2], 1)
est$casys_tillage_mix <- round(as.numeric(diff(rev(inside$estimate))), 1)
est$casys_tillage_mix_lo <- round(inside$conf.int[1], 1)
est$casys_tillage_mix_hi <- round(inside$conf.int[2], 1)
est$casys_zone_table <- zone_tab |>
  mutate(mean_richness = round(mean_richness, 0)) |>
  as.data.frame()

# ----------------------------------------- 8. SPRUCE: the scale of the response
spruce_ok <- spruce |> filter(!is.na(bacteria_copy_dry))

p_raw <- ggplot(spruce_ok, aes(depth_cm, bacteria_copy_dry)) +
  geom_point(alpha = 0.55, colour = "#1d4e89", size = 1.6) +
  labs(
    title = "Gene copies per gram, as measured",
    subtitle = "A few surface samples set the axis",
    x = "Depth (cm)", y = "Bacterial copies per g dry peat"
  ) +
  theme_slide()

p_log <- ggplot(spruce_ok, aes(depth_cm, log10_bacteria)) +
  geom_point(alpha = 0.55, colour = "#0e4d3a", size = 1.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "#8a5a00", fill = "#8a5a00", alpha = 0.15) +
  labs(
    title = "The same response on a log scale",
    subtitle = "A straight line is now a defensible first model",
    x = "Depth (cm)", y = expression(log[10] ~ "copies per g dry peat")
  ) +
  theme_slide()

save_fig(p_raw | p_log, "spruce-depth-scale.png", width = 12.5, height = 5.0)

fit_depth <- lm(log10_bacteria ~ depth_cm, data = spruce_ok)
est$spruce_rows <- as.integer(nrow(spruce))
est$spruce_rows_measured <- as.integer(nrow(spruce_ok))
est$spruce_plots <- as.integer(dplyr::n_distinct(spruce$plot))
est$spruce_depths <- as.integer(dplyr::n_distinct(spruce$depth_cm))
est$spruce_depth_slope_per_10cm <- round(10 * coef(fit_depth)[["depth_cm"]], 3)
est$spruce_depth_r2 <- round(summary(fit_depth)$r.squared, 3)

# --------------------------------------- 9. SPRUCE: one profile per enclosure
p_profiles <- ggplot(
  spruce_ok,
  aes(depth_cm, log10_bacteria, group = factor(plot), colour = chambered)
) +
  geom_line(alpha = 0.75, linewidth = 0.8) +
  geom_point(size = 1.1, alpha = 0.7) +
  facet_wrap(~month) +
  scale_colour_manual(
    values = c("TRUE" = "#1d4e89", "FALSE" = "#8a5a00"),
    labels = c("TRUE" = "Chambered enclosure", "FALSE" = "Ambient plot")
  ) +
  labs(
    title = "One line per plot, not one line per sample",
    subtitle = sprintf(
      paste(
        "%d plots, %d depth increments, two sampling months in 2021.",
        "\nThe plot is the unit the treatment sits on."
      ),
      dplyr::n_distinct(spruce$plot), dplyr::n_distinct(spruce$depth_cm)
    ),
    x = "Depth (cm)", y = expression(log[10] ~ "copies per g dry peat"),
    colour = NULL
  ) +
  theme_slide()

save_fig(p_profiles, "spruce-plot-profiles.png", width = 12.0, height = 5.4)

# ------------------------- 10. the two failures of a "significant" result
# The two risk ratios are the published example in Riley et al. (2022),
# BMJ 379:e072883. They are redrawn here, not recomputed.
riley <- data.frame(
  label = c(
    "Detected, and small\nRR 0.97 (0.95 to 0.99)",
    "Large, and uncertain\nRR 0.70 (0.40 to 1.10)"
  ),
  est = c(0.97, 0.70),
  lo = c(0.95, 0.40),
  hi = c(0.99, 1.10)
)
riley$label <- factor(riley$label, levels = rev(riley$label))

p_riley <- ggplot(riley, aes(est, label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "#3d4a42") +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y",
                width = 0.12, linewidth = 1.3, colour = "#0e4d3a") +
  geom_point(size = 4.2, colour = "#0e4d3a") +
  scale_x_log10(breaks = c(0.4, 0.6, 0.8, 1.0, 1.25)) +
  labs(
    title = "Two results, two different failures",
    subtitle = "A narrow interval away from 1 is a precise estimate of a small effect.\nA wide interval across 1 is an imprecise estimate, not evidence of no effect.",
    x = "Risk ratio (log scale)", y = NULL
  ) +
  theme_slide() +
  theme(plot.margin = margin(28, 16, 8, 8))

save_fig(p_riley, "pitfall-two-intervals.png", width = 11.0, height = 5.0)

# ------------------------- 11. what a threshold does to a real covariate
cut_point <- median(casys$resist_pc1, na.rm = TRUE)
dich <- casys |>
  filter(!is.na(resist_pc1)) |>
  mutate(split = if_else(resist_pc1 <= cut_point, "Low", "High"))

fit_cont <- lm(richness ~ resist_pc1, data = dich)
fit_split <- lm(richness ~ split, data = dich)

p_dich <- ggplot(dich, aes(resist_pc1, richness)) +
  geom_vline(xintercept = cut_point, linetype = "dashed", colour = "#b03030") +
  geom_point(aes(colour = split), alpha = 0.4, size = 1.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "#0e4d3a", fill = "#0e4d3a", alpha = 0.15) +
  geom_segment(
    data = data.frame(
      x = c(min(dich$resist_pc1), cut_point),
      xend = c(cut_point, max(dich$resist_pc1)),
      y = c(
        coef(fit_split)[[1]] + coef(fit_split)[[2]],
        coef(fit_split)[[1]]
      )
    ),
    aes(x = x, xend = xend, y = y, yend = y),
    inherit.aes = FALSE, colour = "#b03030", linewidth = 1.2
  ) +
  scale_colour_manual(values = c(Low = "#1d4e89", High = "#8a5a00")) +
  labs(
    title = "A threshold replaces a slope with a step",
    subtitle = sprintf(
      paste(
        "CA-SYS richness against soil resistivity PC1, with a median split at %.2f.",
        "\nGreen, the continuous fit. Red, the two group means the split allows."
      ),
      cut_point
    ),
    x = "Soil resistivity, first principal component",
    y = "OTUs observed", colour = "Split"
  ) +
  theme_slide()

save_fig(p_dich, "pitfall-dichotomisation.png", width = 10.5, height = 5.4)

est$casys_dich_cut <- round(cut_point, 2)
est$casys_dich_slope_per_unit <- round(coef(fit_cont)[["resist_pc1"]], 1)
est$casys_dich_r2_continuous <- round(summary(fit_cont)$r.squared, 4)
est$casys_dich_r2_split <- round(summary(fit_split)$r.squared, 4)

# ---------------------------------------------------------------- estimates
write_json(
  est,
  file.path(out_dir, "data-estimates.json"),
  auto_unbox = TRUE, pretty = TRUE, digits = 6
)

cat("Wrote real-data figures and", file.path(out_dir, "data-estimates.json"), "\n")
str(est[setdiff(names(est), "casys_zone_table")])
