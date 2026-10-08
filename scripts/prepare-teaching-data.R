# Derive the two teaching tables used by the workshop slides.
#
#   data/16s_RNA_sequencing/  ->  data/derived/casys_teaching.csv
#   data/SPRUCE/              ->  data/derived/spruce_teaching.csv
#
# Raw inputs are immutable. Run this before scripts/generate-data-figures.R.
#   Rscript scripts/prepare-teaching-data.R
#
# Sources
#   CA-SYS 16S: Klockenbring et al. (2026) Data in Brief 67:112976,
#     doi:10.1016/j.dib.2026.112976; data doi:10.57745/LO2KIF.
#   SPRUCE qPCR: Roth et al. (2024) ESS-DIVE,
#     doi:10.25581/spruce.110/1998878.

suppressMessages({
  library(dplyr)
})

out_dir <- "data/derived"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------- CA-SYS ----
# The published object is a phyloseq S4 object. phyloseq is a Bioconductor
# dependency the deck does not otherwise need, so the three slots are read
# directly. unclass() returns the data part of a class that contains "matrix".
casys_path <- "data/16s_RNA_sequencing/casys_phyloseq_clean.rds"
ps <- readRDS(casys_path)
slots <- attributes(ps)

counts <- unclass(slots$otu_table)
attr(counts, "taxa_are_rows") <- NULL
stopifnot(is.numeric(counts), nrow(counts) == 10633L, ncol(counts) == 595L)

taxonomy <- as.data.frame(unclass(slots$tax_table), stringsAsFactors = FALSE)
stopifnot(identical(rownames(taxonomy), rownames(counts)))

sample_slot <- unclass(slots$sam_data)
meta <- as.data.frame(
  sample_slot[names(sample_slot) != ""],
  stringsAsFactors = FALSE
)
meta$sample_id <- as.character(attr(sample_slot, "row.names"))
stopifnot(identical(meta$sample_id, colnames(counts)))

# Reads summed over a taxonomic group, for every sample.
group_reads <- function(rank, value) {
  keep <- !is.na(taxonomy[[rank]]) & taxonomy[[rank]] == value
  stopifnot(any(keep))
  colSums(counts[keep, , drop = FALSE])
}

total_reads <- colSums(counts)

casys <- tibble(
  sample_id = meta$sample_id,
  year = as.integer(meta$Year),
  plot = as.character(meta$Parcelle),
  point = as.character(meta$ID_Point),
  system = factor(meta$System, levels = c("SD1", "SD2", "TS1", "TS2")),
  zone = as.character(meta$SystAE),
  crop = as.character(meta$Landuse),
  plowing = as.character(meta$Plowing),
  superficial_tillage = as.character(meta$SupTillage),
  tillage_intensity = as.character(meta$TillageIntensity),
  ferti_n = as.character(meta$FertiN),
  ferti_pk = as.character(meta$FertiPK),
  soil_type = as.character(meta$SoilType),
  resist_pc1 = as.numeric(meta$Resist_PC1),
  # Library size recorded by the producers, and the same quantity recomputed
  # from the filtered count table. They are not identical: the published
  # SequencingDepth is the pre-filtering depth.
  sequencing_depth = as.integer(meta$SequencingDepth),
  total_reads = as.integer(total_reads),
  richness = as.integer(colSums(counts > 0)),
  nitrospira_reads = as.integer(group_reads("Genus", "Nitrospira")),
  nitrosospira_reads = as.integer(group_reads("Genus", "Nitrosospira")),
  methylobacter_reads = as.integer(group_reads("Genus", "Methylobacter")),
  actinobacteriota_reads = as.integer(group_reads("Phylum", "Actinobacteriota"))
)

casys <- casys |>
  mutate(
    # "tillage" is the contrast the systems were built around: SD1 and SD2 are
    # no-till, TS1 and TS2 are tilled.
    tillage = if_else(system %in% c("SD1", "SD2"), "No-till", "Tilled"),
    methylobacter_present = as.integer(methylobacter_reads > 0L),
    nitrospira_rel = nitrospira_reads / total_reads,
    methylobacter_rel = methylobacter_reads / total_reads,
    actinobacteriota_rel = actinobacteriota_reads / total_reads,
    # 2018 was sampled before the treatments were applied (dataset README).
    baseline_year = year == 2018L
  )

write.csv(casys, file.path(out_dir, "casys_teaching.csv"), row.names = FALSE)

# ---------------------------------------------------------------- SPRUCE ----
# "Missing numeric data are indicated by -9999" (dataset documentation,
# SPRUCE_qPCR_of_microbial_gene_copy_numbers_20260707.pdf). The sentinel is
# turned into NA here and nowhere else.
spruce_raw <- read.csv(
  "data/SPRUCE/SPRUCE_qPCR_copy_numbers_2021.csv",
  stringsAsFactors = FALSE
)

missing_code <- -9999
numeric_cols <- c(
  "Bacteria_copy_dry", "Log_bacteria_copy_dry",
  "Archaea_copy_dry", "Log_archaea_copy_dry",
  "Bacteria_copy_wet", "Archaea_copy_wet",
  "Fungal_copy_dry", "Log_fungal_copy_dry", "Fungal_copy_wet"
)
n_sentinel <- sum(sapply(spruce_raw[numeric_cols], \(v) sum(v == missing_code)))
for (nm in numeric_cols) {
  spruce_raw[[nm]][spruce_raw[[nm]] == missing_code] <- NA_real_
}

spruce <- tibble(
  sample_id = spruce_raw$Sample_ID,
  sample_date = as.Date(spruce_raw$Sample_date),
  year = as.integer(spruce_raw$Year),
  month = spruce_raw$Month,
  depth_range = spruce_raw$Depth_range,
  depth_cm = as.numeric(spruce_raw$Depth_avg),
  plot = as.integer(spruce_raw$Plot),
  # Temp_experimental is the assigned warming for the 10 chambered plots and
  # "Amb" for the 2 ambient plots, which were never assigned a level.
  chambered = spruce_raw$Temp_experimental != "Amb",
  warming_c = suppressWarnings(as.numeric(spruce_raw$Temp_experimental)),
  temp_annual_avg = as.numeric(spruce_raw$Temp_annual_avg),
  co2 = if_else(spruce_raw$CO2_treatment == "E", "Elevated", "Ambient"),
  bacteria_copy_dry = spruce_raw$Bacteria_copy_dry,
  archaea_copy_dry = spruce_raw$Archaea_copy_dry,
  fungal_copy_dry = spruce_raw$Fungal_copy_dry
) |>
  mutate(
    log10_bacteria = log10(bacteria_copy_dry),
    log10_archaea = log10(archaea_copy_dry),
    log10_fungi = log10(fungal_copy_dry)
  )

write.csv(spruce, file.path(out_dir, "spruce_teaching.csv"), row.names = FALSE)

# ------------------------------------------------------------- provenance ----
cat(sprintf("CA-SYS : %d samples, %d OTUs, %.1f%% zero entries\n",
            ncol(counts), nrow(counts), 100 * mean(counts == 0)))
cat(sprintf("CA-SYS : richness %d-%d, library size %d-%d reads\n",
            min(casys$richness), max(casys$richness),
            min(casys$total_reads), max(casys$total_reads)))
cat(sprintf("SPRUCE : %d rows, %d plots, %d depths, %d sentinel values set to NA\n",
            nrow(spruce), dplyr::n_distinct(spruce$plot),
            dplyr::n_distinct(spruce$depth_cm), n_sentinel))
cat(sprintf("Wrote %s\n", file.path(out_dir, "casys_teaching.csv")))
cat(sprintf("Wrote %s\n", file.path(out_dir, "spruce_teaching.csv")))
