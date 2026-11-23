## Copyright (c) 2023 Lene J. Kjaer, Michael P. Ward, Anette E. Boklund,
## Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby
##
## ENIGMA HPAI - weekly data/model preparation for Shiny (November 2026)
##
## PROCEDURE
## 1. Download the newest infur_YYYYMMDD.xlsx file to the ENIGMA2023 folder.
## 2. Run this script. It creates a candidate for_ai.car in tmp/ and verifies it.
## 3. Review the printed checks; if successful, the file is copied to ENIGMA2023/.
## 4. Test the app, then commit/push for_ai.car using GitHub Desktop.
##
## GPL-3.0-or-later. The software is provided without warranty.
## See https://www.gnu.org/licenses/gpl-3.0.html
##
## OPERATIONAL SPECIFICATION
## - Recent ~1.25-year window prepared by existing ENIGMA_DataPrep.R workflow
## - Three annual harmonics in both endemic and autoregressive components
## - Negative-binomial (NegBin1) with country-specific random effects
## - Fixed neighbourhood-order power-law exponent d = 16 (PROVISIONAL)
## - No bird-migration, flyway or ecological-region network
## - Last reported week excluded from fitting to account for reporting delays
## - Four forecast steps: reporting-delay week + three subsequent weeks
##
## IMPORTANT: d = 16 is supported by in-sample sensitivity analysis only;
## retain as provisional pending rolling out-of-sample validation.
## IMPORTANT: endemic offset is placed INSIDE end, as required by hhh4.
## This corrects the previous top-level offset (which was not a valid hhh4
## component setting). It can change estimates substantially: validate before
## deploying. The original wetland expression (log(AI_wet1 + 2)) is preserved.

# ----- Packages -----
library(tidyverse)
library(sf)
library(readxl)
library(surveillance)
library(shiny)
library(shinydashboard)
library(countrycode)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(spdep)
library(fanplot)
library(qs2)

# ----- Options -----
POWER_D <- 16
N_SIM_CHECK <- 20L         # small smoke test, not the app's 500 simulations
FIT_WARNING_POLICY <- "report" # print optimisation warnings, stop on failed convergence

# ----- Locate repository -----
filepaths <- c(
  "C:/Users/hzs315/Documents/GitHub/ckirkeby.github.io/ENIGMA2023/",
  "C:/Users/zxd598/Documents/GitHub/ckirkeby.github.io/ENIGMA2023/"
)
valid_paths <- filepaths[dir.exists(filepaths)]
if (length(valid_paths) == 0L) {
  stop("Cannot find ENIGMA2023 repository. Update 'filepaths' near the top of this script.")
}
filepath <- normalizePath(valid_paths[[1]], winslash = "/", mustWork = TRUE)
setwd(filepath)
cat("ENIGMA project folder:", filepath, "\n")

# ----- Read newest WOAH file -----
ff <- list.files(path = filepath, pattern = "^infur_[0-9]{8}\\.xlsx$", full.names = FALSE)
if (length(ff) == 0L) stop("No infur_YYYYMMDD.xlsx file found in ", filepath)
# Prefer the report date in the filename over filesystem modification time.
filename <- ff[which.max(as.integer(sub("^infur_([0-9]{8})\\.xlsx$", "\\1", ff)))]
if (length(filename) != 1L || grepl("[()]", filename)) stop("Invalid WOAH filename.")
sheetname <- tools::file_path_sans_ext(basename(filename))
cat("Selected WOAH file:", filename, "\n")
ai_data <- readxl::read_excel(file.path(filepath, filename), sheet = sheetname)
if (nrow(ai_data) == 0L) stop("WOAH input file contains no data.")

# ----- Existing ENIGMA data preparation (unchanged) -----
source("./src/ENIGMA_custom_functions.R")
source("./src/ENIGMA_DataPrep.R")

needed <- c("end_yearweek", "AI_weekly", "AI_wet", "AI_coast",
            "country_area", "europe_adjmat", "europeanCountries.sub",
            "endDate", "updateDate", "end_yearWeek", "europe_data",
            "europe_data_weekly", "europeanCountries")
missing_objects <- needed[!vapply(needed, exists, logical(1), inherits = TRUE)]
if (length(missing_objects)) stop("Data preparation did not create: ", paste(missing_objects, collapse = ", "))

# ----- Reproduce existing Shiny data window exactly -----
endWeek <- as.numeric(lubridate::isoweek(end_yearweek))
endYear <- as.numeric(lubridate::isoyear(end_yearweek))
start_yearweek <- end_yearweek - (52 + 13)

N <- nrow(AI_weekly)
subset_start <- N - (52 - endWeek) - (52 + 13) - 52
subset_end   <- N - 52 - (52 - endWeek) + 4
if (subset_start < 1L || subset_end > N || subset_end < subset_start) {
  stop("Invalid model window: ", subset_start, ":", subset_end, " for ", N, " total rows.")
}
ix <- subset_start:subset_end
AI_weekly1 <- AI_weekly[ix, , drop = FALSE]
AI_wet1 <- AI_wet[ix, , drop = FALSE]
AI_coast1 <- AI_coast[ix, , drop = FALSE]
area_selected <- country_area[ix, , drop = FALSE]
area_denominator <- rowSums(area_selected)
if (any(!is.finite(area_denominator) | area_denominator <= 0)) {
  stop("Invalid country area fractions for one or more weeks.")
}
area_frac <- area_selected / area_denominator

start_W <- lubridate::isoweek(start_yearweek)
start_Y <- lubridate::isoyear(start_yearweek)
minDate <- as.Date(tsibble::make_yearweek(year = start_Y, week = start_W, week_start = 1))
maxdate <- format(as.Date(updateDate, format = "%d/%m/%Y"), "%Y-%m-%d")

AI_sts <- surveillance::sts(
  observed = AI_weekly1,
  start = c(as.numeric(start_Y), as.numeric(start_W)),
  frequency = 52,
  neighbourhood = europe_adjmat,
  map = europeanCountries.sub
)

# The app expects these original country-selection objects.
districts2plot <- which(colSums(observed(AI_sts), na.rm = TRUE) > 10)
districts2plot1 <- countrycode::countrycode(
  as.character(names(districts2plot)), "iso3c", "country.name"
)
names(districts2plot) <- districts2plot1
districts2plot <- districts2plot[order(names(districts2plot))]
districts2plot <- setNames(
  c(districts2plot, 0),
  c(names(districts2plot), "Summed all countries")
)
districts2plot2 <- names(districts2plot)

# ----- Verify reporting-delay design -----
Y <- observed(AI_sts)
n_units <- ncol(Y)
full_weeks <- which(rowSums(!is.na(Y)) == n_units)
if (length(full_weeks) == 0L) stop("No completely observed weeks in AI_sts.")
last_report <- max(full_weeks)
last_reliable <- last_report - 1L
if (last_reliable < 2L) stop("Too few observed weeks to fit the model.")
if (anyNA(Y[seq_len(last_report), , drop = FALSE])) {
  stop("Missing observations inside the historical/reported period; inspect data prep.")
}
if (last_report != last_reliable + 1L) stop("Unexpected reporting-delay indexing.")
FORECAST <- (last_reliable + 1L):(last_reliable + 4L)
if (max(FORECAST) > nrow(Y)) {
  stop("Four forecast weeks do not fit in AI_sts. Check data preparation / subset_end.")
}
if (any(!is.na(Y[(last_report + 1L):nrow(Y), , drop = FALSE]))) {
  stop("Expected future placeholder rows to be NA.")
}
FINAL_TRAIN <- 2:last_reliable
cat("Last reported row:", last_report, "\n")
cat("Last reliable row (fit ends):", last_reliable, "\n")
cat("Forecast rows:", paste(FORECAST, collapse = ", "), "\n")
cat("Country units:", n_units, "; model rows:", nrow(Y), "\n")

# ----- Three annual harmonics -----
f_end <- surveillance::addSeason2formula(
  f = ~ 1 + t + ri(type = "iid") - 1,
  S = 3, period = 52
)
f_ar <- surveillance::addSeason2formula(
  f = ~ 1 + t + ri(type = "iid") - 1,
  S = 3, period = 52
)

# ----- Fixed power-law matrix, reproducible in every weekly update -----
nb <- as.matrix(neighbourhood(AI_sts))
if (!identical(dim(nb), c(n_units, n_units))) stop("Neighbourhood matrix dimensions do not match observed counts.")
finite_orders <- nb[is.finite(nb) & nb > 0]
if (!length(finite_orders)) stop("Neighbourhood matrix has no positive finite neighbourhood orders.")
maxlag <- max(finite_orders)

W_power_fixed <- surveillance::zetaweights(
  nbmat = nb,
  d = POWER_D,
  maxlag = maxlag,
  normalize = FALSE
)
# Explicitly exclude the own-country contribution (handled by ar).
diag(W_power_fixed) <- 0
rs <- rowSums(W_power_fixed)
W_power_fixed <- sweep(W_power_fixed, 1L, ifelse(rs > 0, rs, 1), "/")

if (any(!is.finite(W_power_fixed)) || any(W_power_fixed < 0) ||
    any(diag(W_power_fixed) != 0)) {
  stop("Invalid fixed power-law connectivity matrix.")
}
if (any(rs > 0 & abs(rowSums(W_power_fixed) - 1) > 1e-8)) {
  stop("Power-law connectivity rows were not normalized.")
}
cat("Fixed power-law d:", POWER_D,
    "; units without outgoing neighbourhood weights:", sum(rs == 0), "\n")

# ----- Endemic offset -----
# Keep the user's original mathematical expression, but put it in the
# correct 'end' component. This is a substantive fix relative to the
# previous top-level 'offset' and must be validated before deployment.
end_offset <- area_frac * (log(AI_coast1 + 1) + log(AI_wet1 + 2))
if (!identical(dim(end_offset), dim(Y)) ||
    any(!is.finite(end_offset)) || any(end_offset <= 0)) {
  stop("Endemic offset is not a finite, strictly positive time-by-country matrix.")
}

# ----- Operational model specification -----
final_model_base <- list(
  end = list(f = f_end, offset = end_offset),
  ar = list(f = f_ar),
  ne = list(
    f = ~ 1 + ri(type = "iid") - 1,
    weights = W_power_fixed
  ),
  family = "NegBin1",
  subset = FINAL_TRAIN,
  optimizer = list(
    stop = list(tol = 1e-5, niter = 500),
    regression = list(method = "nlminb", iter.max = 200, eval.max = 400),
    variance = list(method = "Nelder-Mead", maxit = 2000)
  ),
  keep.terms = TRUE
)

# ----- Fit and validate BEFORE saving -----
cat("Fitting three-harmonic ENIGMA model...\n")
final_model <- surveillance::hhh4(stsObj = AI_sts, control = final_model_base)
if (!isTRUE(final_model$convergence)) {
  stop("ENIGMA hhh4 fit did not converge. for_ai.car has NOT been overwritten.")
}
fixed_cf <- coef(final_model)
if (any(!is.finite(fixed_cf))) {
  stop("ENIGMA fit contains nonfinite coefficients. for_ai.car has NOT been overwritten.")
}
if (!identical(final_model$control$subset, FINAL_TRAIN)) {
  stop("Fitted training subset does not match reporting-delay cutoff.")
}
if (any(grepl("neweights\\.logd", names(fixed_cf)))) {
  stop("Decay exponent is unexpectedly estimated; expected fixed d.")
}
cat("Fit converged. Coefficient range:",
    paste(signif(range(fixed_cf), 4), collapse = " to "), "\n")
print(summary(final_model))

# Confirm the model can produce a four-week forecast from the last RELIABLE week.
# This mirrors the existing Shiny sim() reactive when its end date is the
# latest reporting week. The app itself uses 500 simulation runs.
cat("Checking four-step forecast simulation...\n")
smoke <- simulate(
  final_model,
  nsim = N_SIM_CHECK,
  seed = 1,
  subset = FORECAST,
  y.start = Y[last_reliable, ]
)
smoke_arr <- as.array(smoke)
if (length(dim(smoke_arr)) != 3L ||
    dim(smoke_arr)[1] != 4L ||
    dim(smoke_arr)[2] != n_units ||
    any(!is.finite(smoke_arr))) {
  stop("Four-step forecast smoke test failed. for_ai.car has NOT been overwritten.")
}
cat("Simulation succeeded: ", paste(dim(smoke_arr), collapse = " x "),
    " (forecast weeks x countries x simulations).\n", sep = "")
rm(smoke, smoke_arr)

# ----- Save exactly the objects expected by the existing Shiny app -----
save_date <- Sys.Date()
output_dir <- file.path(filepath, "tmp")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
candidate_file <- file.path(output_dir, "for_ai.car")
production_file <- file.path(filepath, "for_ai.car")

bundle <- list(
  ai_data = ai_data,
  save_date = save_date,
  end_yearWeek = end_yearWeek,
  filename = filename,
  endDate = endDate,
  updateDate = updateDate,
  europe_data = europe_data,
  europe_data_weekly = europe_data_weekly,
  AI_weekly = AI_weekly,
  AI_weekly1 = AI_weekly1,
  AI_wet = AI_wet,
  AI_coast = AI_coast,
  europe_adjmat = europe_adjmat,
  europeanCountries.sub = europeanCountries.sub,
  europeanCountries = europeanCountries,
  minDate = minDate,
  maxdate = maxdate,
  districts2plot = districts2plot,
  districts2plot2 = districts2plot2,
  AI_sts = AI_sts,
  final_model = final_model
)

# Never overwrite the production file until the candidate can be read back.
qs2::qs_save(bundle, file = candidate_file, compress_level = 14L)
verified_bundle <- qs2::qs_read(candidate_file, validate_checksum = TRUE)
if (!is.list(verified_bundle) ||
    !identical(sort(names(bundle)), sort(names(verified_bundle))) ||
    !isTRUE(verified_bundle$final_model$convergence)) {
  stop("Saved candidate failed read-back validation; production file unchanged.")
}
rm(verified_bundle, bundle)

# Preserve the previous local .car file before replacement, if it exists.
if (file.exists(production_file)) {
  backup_file <- file.path(
    output_dir,
    paste0("for_ai_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".car")
  )
  if (!file.copy(production_file, backup_file, overwrite = FALSE)) {
    stop("Could not back up old for_ai.car; production file unchanged.")
  }
  cat("Previous .car backed up to:", backup_file, "\n")
}

if (!file.copy(candidate_file, production_file, overwrite = TRUE)) {
  stop("Could not copy validated .car file to production location.")
}
cat("\nSUCCESS - validated for_ai.car saved to:\n", production_file, "\n")
cat("WOAH data:", filename, "\n")
cat("Training observations: rows", min(FINAL_TRAIN), "through", max(FINAL_TRAIN), "\n")
cat("Forecast: rows", paste(FORECAST, collapse = ", "), "\n")
cat("Fixed d:", POWER_D, "\n")
cat("Test the Shiny app before pushing changes to GitHub.\n")
