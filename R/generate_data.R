# Synthetic checkout experiment.
#
# The clean table is the analysis dataset. A second table is intentionally
# corrupted so the validation report has something real to catch. Ground-truth
# parameters are returned with the data and are never used as if they were
# estimated from a live experiment.

#' Default design used by [generate_experiment_data()].
#'
#' Covariate effects are stored before centering. Generation centers them so
#' that a control user has the baseline conversion probability on average.
#' @return A list of design settings.
default_generation_parameters <- function() {
  list(
    n_users = 20000L,
    seed = 20241119L,
    start_date = as.Date("2026-10-06"),
    n_days = 14L,
    treatment_probability = 0.5,
    baseline_conversion = 0.10,
    treatment_conversion_lift = 0.015,
    probability_bounds = c(0.01, 0.85),
    device_probs = c(mobile = 0.65, desktop = 0.35),
    device_effects = c(mobile = 0, desktop = 0.02),
    channel_probs = c(organic = 0.40, paid_search = 0.30, email = 0.15, social = 0.15),
    channel_effects = c(organic = 0, paid_search = -0.015, email = 0.025, social = -0.010),
    country_probs = c(US = 0.45, UK = 0.15, DE = 0.12, CA = 0.10, BR = 0.10, MX = 0.08),
    country_effects = c(US = 0, UK = 0.004, DE = 0.003, CA = 0.002, BR = -0.008, MX = -0.006),
    weekday_weight = 1.4,
    weekend_weight = 0.7,
    mean_order_value = c(control = 48, treatment = 52),
    revenue_sdlog = 0.55,
    error_base = 0.018,
    error_mobile = 0.006,
    error_treatment = 0,
    missing_country_rate = 0.015,
    missing_channel_rate = 0.008,
    n_duplicate_rows = 8L,
    n_missing_ids = 5L,
    n_invalid_variants = 6L,
    n_false_revenue = 7L,
    n_zero_purchases = 4L,
    n_early_timestamps = 5L,
    n_drop_treatment = 1500L,
    false_revenue_amount = 19.99
  )
}

#' Shift named effects so the probability-weighted mean is zero.
#'
#' @param effects Named numeric vector.
#' @param probs Named numeric vector of probabilities on the same names.
#' @return Centered effects.
center_effects <- function(effects, probs) {
  if (is.null(names(effects)) || is.null(names(probs))) {
    stop("Effects and probabilities must be named.", call. = FALSE)
  }
  if (!setequal(names(effects), names(probs))) {
    stop("Effect names and probability names must match.", call. = FALSE)
  }
  probs <- probs[names(effects)]
  if (any(probs < 0) || sum(probs) <= 0) {
    stop("Probabilities must be non-negative and sum to a positive value.", call. = FALSE)
  }
  probs <- probs / sum(probs)
  effects - sum(probs * effects)
}

#' Draw a reproducible synthetic experiment.
#'
#' @param n_users Users to randomize. `NULL` uses the default design.
#' @param seed RNG seed. `NULL` uses the default design.
#' @param start_date First experiment day.
#' @param n_days Length of the experiment in days.
#' @param treatment_probability Probability of assignment to treatment.
#' @param baseline_conversion Mean control conversion probability.
#' @param treatment_conversion_lift Absolute lift added to the treatment probability.
#' @param corrupt If `TRUE`, also build the intentionally corrupted table.
#' @return A list with `users`, `corrupted` (or `NULL`), and `parameters`.
generate_experiment_data <- function(
    n_users = NULL,
    seed = NULL,
    start_date = NULL,
    n_days = NULL,
    treatment_probability = NULL,
    baseline_conversion = NULL,
    treatment_conversion_lift = NULL,
    corrupt = TRUE) {
  parameters <- default_generation_parameters()
  overrides <- list(
    n_users = n_users,
    seed = seed,
    start_date = start_date,
    n_days = n_days,
    treatment_probability = treatment_probability,
    baseline_conversion = baseline_conversion,
    treatment_conversion_lift = treatment_conversion_lift
  )
  overrides <- overrides[!vapply(overrides, is.null, logical(1))]
  parameters[names(overrides)] <- overrides

  parameters$n_users <- as.integer(parameters$n_users)
  parameters$seed <- as.integer(parameters$seed)
  parameters$n_days <- as.integer(parameters$n_days)
  parameters$start_date <- as.Date(parameters$start_date)

  if (is.na(parameters$n_users) || parameters$n_users < 100L) {
    stop("n_users must be an integer of at least 100.", call. = FALSE)
  }
  if (is.na(parameters$seed)) {
    stop("seed must be a single integer.", call. = FALSE)
  }
  if (is.na(parameters$n_days) || parameters$n_days < 7L) {
    stop("n_days must be an integer of at least 7.", call. = FALSE)
  }
  if (parameters$treatment_probability <= 0 || parameters$treatment_probability >= 1) {
    stop("treatment_probability must be strictly between 0 and 1.", call. = FALSE)
  }
  if (parameters$baseline_conversion <= 0 || parameters$baseline_conversion >= 1) {
    stop("baseline_conversion must be strictly between 0 and 1.", call. = FALSE)
  }

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(parameters$seed)

  device_probs <- parameters$device_probs / sum(parameters$device_probs)
  channel_probs <- parameters$channel_probs / sum(parameters$channel_probs)
  country_probs <- parameters$country_probs / sum(parameters$country_probs)
  device_effects <- center_effects(parameters$device_effects, device_probs)
  channel_effects <- center_effects(parameters$channel_effects, channel_probs)
  country_effects <- center_effects(parameters$country_effects, country_probs)

  n <- parameters$n_users
  variant <- ifelse(
    stats::rbinom(n, size = 1L, prob = parameters$treatment_probability) == 1L,
    "treatment",
    "control"
  )
  device_type <- sample(names(device_probs), n, replace = TRUE, prob = device_probs)
  traffic_source <- sample(names(channel_probs), n, replace = TRUE, prob = channel_probs)
  country <- sample(names(country_probs), n, replace = TRUE, prob = country_probs)

  days <- parameters$start_date + seq.int(0L, parameters$n_days - 1L)
  weekend <- lubridate::wday(days, week_start = 1L) >= 6L
  day_weight <- ifelse(weekend, parameters$weekend_weight, parameters$weekday_weight)
  day_index <- sample.int(parameters$n_days, n, replace = TRUE, prob = day_weight)
  second_in_day <- sample.int(86400L, n, replace = TRUE) - 1L
  experiment_start <- as.POSIXct(parameters$start_date, tz = "UTC")
  assignment_timestamp <- experiment_start +
    lubridate::days(day_index - 1L) +
    lubridate::seconds(second_in_day)

  probability <- parameters$baseline_conversion +
    parameters$treatment_conversion_lift * (variant == "treatment") +
    unname(device_effects[device_type]) +
    unname(channel_effects[traffic_source]) +
    unname(country_effects[country])
  bounds <- parameters$probability_bounds
  n_clipped <- sum(probability < bounds[[1]] | probability > bounds[[2]])
  probability <- pmin(pmax(probability, bounds[[1]]), bounds[[2]])
  converted <- stats::rbinom(n, size = 1L, prob = probability)

  order_mean <- ifelse(
    variant == "treatment",
    parameters$mean_order_value[["treatment"]],
    parameters$mean_order_value[["control"]]
  )
  meanlog <- log(order_mean) - 0.5 * parameters$revenue_sdlog^2
  purchase_amount <- stats::rlnorm(n, meanlog = meanlog, sdlog = parameters$revenue_sdlog)
  purchase_amount <- pmax(round(purchase_amount, 2), 0.01)
  revenue <- ifelse(converted == 1L, purchase_amount, 0)

  error_probability <- parameters$error_base +
    parameters$error_mobile * (device_type == "mobile") +
    parameters$error_treatment * (variant == "treatment")
  error_probability <- pmin(pmax(error_probability, 0.001), 0.5)
  checkout_error <- stats::rbinom(n, size = 1L, prob = error_probability)

  country[stats::runif(n) < parameters$missing_country_rate] <- NA_character_
  traffic_source[stats::runif(n) < parameters$missing_channel_rate] <- NA_character_

  users <- tibble::tibble(
    user_id = sprintf(paste0("U%0", nchar(n), "d"), seq_len(n)),
    variant = variant,
    assignment_timestamp = assignment_timestamp,
    device_type = device_type,
    traffic_source = traffic_source,
    country = country,
    converted = as.integer(converted),
    revenue = as.numeric(revenue),
    checkout_error = as.integer(checkout_error)
  )

  control_p <- probability[variant == "control"]
  treatment_p <- probability[variant == "treatment"]
  parameters$experiment_start <- experiment_start
  parameters$experiment_end <- experiment_start + lubridate::days(parameters$n_days)
  parameters$device_effects_centered <- device_effects
  parameters$channel_effects_centered <- channel_effects
  parameters$country_effects_centered <- country_effects
  parameters$n_clipped <- as.integer(n_clipped)
  parameters$mean_control_probability <- mean(control_p)
  parameters$mean_treatment_probability <- mean(treatment_p)
  parameters$timezone <- "UTC"
  parameters$limitations <- c(
    "Users, assignments, and outcomes are simulated. They are not evidence about a real checkout.",
    "Outcomes are independent. The generator has no interference, no repeat sessions, and no conversions after the assignment window.",
    "Conversion probabilities are additive in variant, device, channel, and country, then clipped to the configured bounds.",
    "Covariate effects are centered, so the baseline is the mean control probability before random sampling of outcomes.",
    "Order value is lognormal and depends on variant, not on device or channel. Users who do not convert have revenue exactly 0.",
    "Checkout errors are generated independently of purchase. The treatment indicator does not change the error probability; mobile users have a higher error probability than desktop users.",
    "Missing country and traffic source values are deleted completely at random.",
    "Weekdays are more likely assignment days than weekends. The clock is UTC."
  )

  corrupted <- if (isTRUE(corrupt)) corrupt_experiment_data(users, parameters) else NULL

  list(users = users, corrupted = corrupted, parameters = parameters)
}

#' Inject a fixed set of data-quality failures.
#'
#' Treatment rows are removed first so later edits are not dropped with them.
#' @param data Clean experiment table.
#' @param parameters Design list from [default_generation_parameters()].
#' @return Corrupted tibble.
corrupt_experiment_data <- function(data, parameters = default_generation_parameters()) {
  if (!tibble::is_tibble(data) && !is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  corrupted <- tibble::as_tibble(data)
  treatment_idx <- which(corrupted$variant == "treatment")
  n_drop <- as.integer(parameters$n_drop_treatment)
  if (length(treatment_idx) < n_drop) {
    stop(
      sprintf(
        "Need at least %d treatment users to build the corrupted sample ratio mismatch.",
        n_drop
      ),
      call. = FALSE
    )
  }

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(parameters$seed + 1L)
  drop_idx <- treatment_idx[sample.int(length(treatment_idx), n_drop)]
  corrupted <- corrupted[-drop_idx, , drop = FALSE]

  last_touched <- parameters$n_duplicate_rows +
    parameters$n_missing_ids +
    parameters$n_invalid_variants +
    parameters$n_early_timestamps
  if (nrow(corrupted) < last_touched) {
    stop("Corrupted sample does not have enough rows for the injected issues.", call. = FALSE)
  }

  duplicates <- corrupted[seq_len(parameters$n_duplicate_rows), , drop = FALSE]
  corrupted <- dplyr::bind_rows(corrupted, duplicates)

  missing_from <- parameters$n_duplicate_rows + 1L
  corrupted$user_id[missing_from:(missing_from + parameters$n_missing_ids - 1L)] <- NA_character_

  variant_from <- missing_from + parameters$n_missing_ids
  corrupted$variant[variant_from:(variant_from + parameters$n_invalid_variants - 1L)] <- "unknown"

  early_from <- variant_from + parameters$n_invalid_variants
  early_rows <- early_from:(early_from + parameters$n_early_timestamps - 1L)
  corrupted$assignment_timestamp[early_rows] <- parameters$experiment_start - lubridate::days(7)

  non_purchasers <- which(corrupted$converted == 0 & corrupted$revenue == 0)
  purchasers <- which(corrupted$converted == 1 & corrupted$revenue > 0)
  if (length(non_purchasers) < parameters$n_false_revenue || length(purchasers) < parameters$n_zero_purchases) {
    stop("Not enough consistent purchasers and non-purchasers to inject revenue issues.", call. = FALSE)
  }
  corrupted$revenue[non_purchasers[seq_len(parameters$n_false_revenue)]] <- parameters$false_revenue_amount
  corrupted$revenue[purchasers[seq_len(parameters$n_zero_purchases)]] <- 0

  corrupted
}

#' Read an experiment CSV with the project column types.
#'
#' @param path File path.
#' @return A tibble. Timestamps are UTC.
read_experiment_data <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Experiment file not found: %s", path), call. = FALSE)
  }
  data <- readr::read_csv(
    path,
    col_types = readr::cols(
      user_id = readr::col_character(),
      variant = readr::col_character(),
      assignment_timestamp = readr::col_datetime(format = ""),
      device_type = readr::col_character(),
      traffic_source = readr::col_character(),
      country = readr::col_character(),
      converted = readr::col_double(),
      revenue = readr::col_double(),
      checkout_error = readr::col_double()
    ),
    show_col_types = FALSE
  )
  attr(data$assignment_timestamp, "tzone") <- "UTC"
  data
}

#' Write the clean table, the corrupted table, and the design parameters.
#'
#' @param experiment Output of [generate_experiment_data()].
#' @param directory Destination directory.
#' @return The experiment, invisibly.
write_experiment_outputs <- function(experiment, directory = "data") {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  readr::write_csv(experiment$users, file.path(directory, "synthetic_experiment.csv"))
  if (!is.null(experiment$corrupted)) {
    readr::write_csv(
      experiment$corrupted,
      file.path(directory, "synthetic_experiment_corrupted.csv")
    )
  }
  saveRDS(experiment$parameters, file.path(directory, "experiment_parameters.rds"))
  invisible(experiment)
}
