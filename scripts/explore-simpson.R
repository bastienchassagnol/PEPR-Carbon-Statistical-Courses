# Scan the CA-SYS teaching table for aggregation effects: a sign reversal
# (Simpson) or a large crude-versus-adjusted change (confounding).
# Exploratory only. Nothing reaches a slide unless the numbers are real.

suppressMessages({
  library(dplyr)
  library(tidyr)
})

casys <- read.csv("data/derived/casys_teaching.csv", stringsAsFactors = FALSE)

cat("=== balance of soil type across the tillage contrast ===\n")
print(table(casys$tillage, casys$soil_type))
cat("\n=== balance of zone across the tillage contrast ===\n")
print(table(casys$tillage, casys$zone))
cat("\n=== samples per system and year ===\n")
print(table(casys$system, casys$year))

responses <- c(
  "richness", "nitrospira_rel", "methylobacter_rel",
  "actinobacteriota_rel", "methylobacter_present"
)
groups <- c("tillage", "ferti_n", "ferti_pk", "plowing", "superficial_tillage")
strata <- c("year", "soil_type", "zone", "system", "crop")

cat("\n\n=== sign reversals, and crude-vs-weighted gaps ===\n")

found <- 0L
for (resp in responses) {
  for (grp in groups) {
    for (st in strata) {
      d <- casys[, c(resp, grp, st)]
      names(d) <- c("y", "g", "s")
      d <- d[stats::complete.cases(d), ]
      if (length(unique(d$g)) != 2L) next
      if (length(unique(d$s)) < 2L) next

      lv <- sort(unique(as.character(d$g)))
      pooled <- tapply(d$y, d$g, mean)
      crude <- pooled[[lv[2]]] - pooled[[lv[1]]]

      w <- d |>
        group_by(s, g) |>
        summarise(m = mean(y), n = dplyr::n(), .groups = "drop") |>
        pivot_wider(names_from = g, values_from = c(m, n))

      hi <- paste0("m_", lv[2]); lo <- paste0("m_", lv[1])
      nhi <- paste0("n_", lv[2]); nlo <- paste0("n_", lv[1])
      if (!all(c(hi, lo) %in% names(w))) next
      w <- w[stats::complete.cases(w[, c(hi, lo)]), ]
      if (nrow(w) < 2L) next

      diffs <- w[[hi]] - w[[lo]]
      wt <- w[[nhi]] + w[[nlo]]
      adjusted <- sum(diffs * wt) / sum(wt)   # stratum-size weighted

      reversal <- (all(diffs > 0) && crude < 0) || (all(diffs < 0) && crude > 0)
      sign_flip <- sign(adjusted) != sign(crude) && adjusted != 0 && crude != 0
      big_shift <- abs(crude) > 1e-12 &&
        abs(adjusted - crude) / abs(crude) > 0.35

      if (!(reversal || sign_flip || big_shift)) next
      found <- found + 1L

      cat("\n-----------------------------------------------------\n")
      cat(sprintf("%s  by %s  stratified by %s\n", resp, grp, st))
      cat(sprintf("  contrast = %s minus %s\n", lv[2], lv[1]))
      cat(sprintf("  crude    = %+.5f\n", crude))
      cat(sprintf("  adjusted = %+.5f   (stratum-size weighted)\n", adjusted))
      cat(sprintf("  flags: %s%s%s\n",
                  if (reversal) "REVERSAL " else "",
                  if (sign_flip) "SIGN-FLIP " else "",
                  if (big_shift) "SHIFT>35%" else ""))
      cat("  strata:\n")
      print(as.data.frame(w), row.names = FALSE, digits = 5)
    }
  }
}
if (found == 0L) cat("\nNothing passed the thresholds.\n")
