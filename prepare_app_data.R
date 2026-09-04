library(dplyr)
data_dir <- "~/Uni/Honours/RA/Code/DATA"
dir_full <- file.path(data_dir, "DEXTER NDW", "cleaned_2025")
dir_wr   <- file.path(data_dir, "DEXTER NDW", "cleaned_2025WR")

required_files <- c(
  "app_network_scale_summary.csv",
  "markov_accessibility_ranking.csv",
  "app_accessibility_cor_matrix.csv",
  "kfunction_component_summary.csv",
  "app_bridge_units.csv"
)

check_dir <- function(dir, label) {
  missing <- required_files[!file.exists(file.path(dir, required_files))]
  if (length(missing) > 0) {
    stop(
      "Missing file(s) in ", label, " (", dir, "): ", paste(missing, collapse = ", "),
      "\nDid you knit the .Rmd after adding the app-data export chunks, and is the path above correct?"
    )
  }
}
check_dir(dir_full, "Full network")
check_dir(dir_wr, "No residential roads")

# ---------------------------------------------------------------------------
# Read and reshape one scenario's exports into the shapes app.R expects.
# ---------------------------------------------------------------------------
read_scenario <- function(dir, scenario_label) {
  p <- function(f) file.path(dir, f)
  
  # -- network scale summary (one row) --
  scale_row <- read.csv(p("app_network_scale_summary.csv"), stringsAsFactors = FALSE)
  scale_row$scenario <- scenario_label
  
  # -- full accessibility ranking (used for both the top-10 table and the
  #    modelled-unit count that defines "network average" for bridge units) --
  full_ranking <- read.csv(p("markov_accessibility_ranking.csv"), stringsAsFactors = FALSE) %>%
    arrange(desc(stationary_prob))
  n_modelled_units <- nrow(full_ranking)
  
  topten <- full_ranking %>%
    head(10) %>%
    transmute(scenario = scenario_label, rank = row_number(),
              unit = unit_name, stationary_prob)
  
  # -- correlation matrix (largest component) --
  cor_raw <- read.csv(p("app_accessibility_cor_matrix.csv"), row.names = 1, check.names = FALSE)
  cor_mat <- as.matrix(cor_raw)
  
  # The .Rmd labels this matrix with long, report-friendly names
  # ("Markov (time)", "Baseline (Part F)", ...) for readability in the PDF;
  # app.R indexes it by the short names used throughout its own UI/plots
  # ("P_time", "Baseline", ...). Rename here so the two stay in sync without
  # app.R needing to know anything about the report's own labelling.
  cor_label_map <- c(
    "Markov (time)"     = "P_time",
    "Markov (speed)"    = "P_speed",
    "Markov (observed)" = "P_observed",
    "Baseline (Part F)" = "Baseline"
  )
  unmatched <- setdiff(c(rownames(cor_mat), colnames(cor_mat)), names(cor_label_map))
  if (length(unmatched) > 0) {
    stop(
      "app_accessibility_cor_matrix.csv (", scenario_label, ") has row/column name(s) ",
      "prepare_app_data.R doesn't recognise: ", paste(unmatched, collapse = ", "),
      ". Update cor_label_map above to match."
    )
  }
  dimnames(cor_mat) <- list(unname(cor_label_map[rownames(cor_mat)]),
                            unname(cor_label_map[colnames(cor_mat)]))
  
  # -- K-function classification, normalised to the short labels app.R uses --
  kfun <- read.csv(p("kfunction_component_summary.csv"), stringsAsFactors = FALSE) %>%
    transmute(
      scenario = scenario_label,
      component, n_sensors, max_r,
      classification = case_when(
        classification == "Clustered"          ~ "Clustered",
        classification == "Regular/Dispersed"  ~ "Regular/Dispersed",
        TRUE                                    ~ "Within envelope"
      ),
      reliability = ifelse(grepl("^Stable", reliability), "Stable", "N/A")
    )
  
  # -- bridge units, with network_avg and the dual-role "exception" flag
  #    computed dynamically (>2x the average stationary probability among
  #    modelled units) rather than hardcoded to a specific unit name, so
  #    this keeps working correctly if the underlying analysis changes --
  network_avg <- 1 / n_modelled_units
  bridges <- read.csv(p("app_bridge_units.csv"), stringsAsFactors = FALSE) %>%
    transmute(
      scenario = scenario_label,
      unit = unit_name,
      betweenness, stationary_prob,
      network_avg = network_avg,
      is_exception = stationary_prob > 2 * network_avg
    )
  
  list(scale = scale_row, topten = topten, cor_matrix = cor_mat, kfun = kfun, bridges = bridges)
}

full_data <- read_scenario(dir_full, "Full network")
wr_data   <- read_scenario(dir_wr, "No residential roads")

# ---------------------------------------------------------------------------
# Assemble into exactly the objects app.R expects (same names, same shapes
# as the tibbles it used to define inline).
# ---------------------------------------------------------------------------
real_scale <- bind_rows(full_data$scale, wr_data$scale) %>%
  select(scenario, segments, length_km, pct_30kmh, sig_components,
         largest_share_pct, admin_units_baseline, eigen_power_gap, pagerank_cor,
         n_modelled_units, betweenness_accessibility_cor)

real_topten <- bind_rows(full_data$topten, wr_data$topten)

real_corr_matrix <- list(
  "Full network"          = full_data$cor_matrix,
  "No residential roads"  = wr_data$cor_matrix
)

real_kfun <- bind_rows(full_data$kfun, wr_data$kfun)

real_bridges <- bind_rows(full_data$bridges, wr_data$bridges)

app_data <- list(
  real_scale       = real_scale,
  real_topten      = real_topten,
  real_corr_matrix = real_corr_matrix,
  real_kfun        = real_kfun,
  real_bridges     = real_bridges
)

setwd("~/Uni/Honours/RA/Code/RShiny")
saveRDS(app_data, "app_data.rds")

message("Wrote app_data.rds with:")
message("  real_scale:       ", nrow(real_scale), " rows")
message("  real_topten:      ", nrow(real_topten), " rows")
message("  real_corr_matrix: ", length(real_corr_matrix), " matrices")
message("  real_kfun:        ", nrow(real_kfun), " rows")
message("  real_bridges:     ", nrow(real_bridges), " rows")
message("Copy app_data.rds into the same folder as app.R, commit both, and redeploy.")

