# Check that every in-deck link resolves to a heading id, and that no id is
# defined twice. Run after editing the sections:
#   Rscript scripts/check-slide-links.R

files <- c("index.qmd", list.files("sections", pattern = "[.]qmd$", full.names = TRUE))
lines <- unlist(lapply(files, readLines, warn = FALSE))

# Ids declared on a heading, e.g. "## Title {#sec-foo}" or "{.cls #sec-foo}".
declared <- unlist(regmatches(
  lines,
  gregexpr("(?<=\\{)[^}]*#(sec-[A-Za-z0-9-]+|references)[^}]*(?=\\})", lines, perl = TRUE)
))
declared <- unlist(regmatches(
  declared,
  gregexpr("#(sec-[A-Za-z0-9-]+|references)", declared, perl = TRUE)
))
declared <- sub("^#", "", declared)

# Links of the form [label](#sec-foo).
used <- unlist(regmatches(
  lines,
  gregexpr("\\]\\(#(sec-[A-Za-z0-9-]+|references)\\)", lines, perl = TRUE)
))
used <- sub("^\\]\\(#", "", sub("\\)$", "", used))

missing <- sort(unique(setdiff(used, declared)))
duplicated_ids <- sort(unique(declared[duplicated(declared)]))
unused <- sort(unique(setdiff(declared, used)))

cat(sprintf("%d ids declared, %d link targets used\n",
            length(unique(declared)), length(unique(used))))

if (length(duplicated_ids)) {
  cat("\nDUPLICATED ids:\n"); cat(paste0("  ", duplicated_ids, collapse = "\n"), "\n")
}
if (length(missing)) {
  cat("\nBROKEN links (no such id):\n")
  for (m in missing) {
    where <- files[vapply(files, function(f)
      any(grepl(paste0("](#", m, ")"), readLines(f, warn = FALSE), fixed = TRUE)),
      logical(1))]
    cat(sprintf("  %-34s referenced in %s\n", m, paste(basename(where), collapse = ", ")))
  }
} else {
  cat("\nNo broken links.\n")
}
if (length(unused)) {
  cat(sprintf("\n%d ids are declared but never linked (fine for anchors):\n",
              length(unused)))
  cat(strwrap(paste(unused, collapse = ", "), width = 96, prefix = "  "), sep = "\n")
}

quit(status = if (length(missing) || length(duplicated_ids)) 1L else 0L)
