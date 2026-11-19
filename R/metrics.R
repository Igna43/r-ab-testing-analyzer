# Experiment metrics.
#
# Rates use every assigned user in the group. Revenue per user includes users
# who did not purchase. Average order value uses purchasers only.

#' Summarise the primary, secondary, and guardrail metrics by variant.
#'
#' @param data Validated experiment table.
#' @return A tibble with one row per variant, ordered control then treatment.
calculate_experiment_metrics <- function(data) {
  assert_analysis_frame(data, outcome_cols = c("converted", "checkout_error"))
  if (!"revenue" %in% names(data) || !is.numeric(data$revenue)) {
    stop("revenue must be a numeric column.", call. = FALSE)
  }
  if (anyNA(data$revenue) || any(data$revenue < 0)) {
    stop("revenue must be non-missing and non-negative.", call. = FALSE)
  }
  inconsistent <- (data$converted == 0 & data$revenue > 0) |
    (data$converted == 1 & data$revenue <= 0)
  if (any(inconsistent)) {
    stop(
      "Conversion and revenue disagree. Resolve validation issues before calculating metrics.",
      call. = FALSE
    )
  }

  data |>
    dplyr::group_by(variant) |>
    dplyr::summarise(
      users = dplyr::n(),
      conversions = sum(converted),
      conversion_rate = mean(converted),
      revenue_per_user = mean(revenue),
      purchasers = sum(converted == 1),
      average_order_value = dplyr::if_else(
        purchasers == 0,
        NA_real_,
        sum(revenue) / purchasers
      ),
      checkout_errors = sum(checkout_error),
      checkout_error_rate = mean(checkout_error),
      .groups = "drop"
    ) |>
    dplyr::arrange(match(variant, c("control", "treatment")))
}

#' Daily assignment volume and conversion by variant.
#'
#' @param data Validated experiment table.
#' @return A tibble with one row per day and variant. Dates are UTC.
summarise_daily <- function(data) {
  assert_analysis_frame(data, outcome_cols = "converted")
  if (!inherits(data$assignment_timestamp, "POSIXct") && !inherits(data$assignment_timestamp, "Date")) {
    stop("assignment_timestamp must be POSIXct or Date.", call. = FALSE)
  }
  data |>
    dplyr::mutate(day = lubridate::as_date(assignment_timestamp, tz = "UTC")) |>
    dplyr::group_by(day, variant) |>
    dplyr::summarise(
      users = dplyr::n(),
      conversions = sum(converted),
      conversion_rate = mean(converted),
      .groups = "drop"
    ) |>
    dplyr::arrange(day, match(variant, c("control", "treatment")))
}

#' User counts and conversion by a pre-treatment grouping.
#'
#' @param data Validated experiment table.
#' @param group_var Column name, usually device or traffic source.
#' @return A tibble. Missing group values are excluded and counted in an attribute.
summarise_by_group <- function(data, group_var) {
  assert_analysis_frame(data, outcome_cols = "converted")
  if (!group_var %in% names(data)) {
    stop(sprintf("Column not found: %s.", group_var), call. = FALSE)
  }
  n_missing <- sum(is.na(data[[group_var]]))
  summary <- data |>
    dplyr::filter(!is.na(.data[[group_var]])) |>
    dplyr::group_by(.data[[group_var]], variant) |>
    dplyr::summarise(
      users = dplyr::n(),
      conversions = sum(converted),
      conversion_rate = mean(converted),
      .groups = "drop"
    )
  names(summary)[1] <- "group"
  summary <- summary |>
    dplyr::mutate(group_var = group_var, .before = 1) |>
    dplyr::arrange(group, match(variant, c("control", "treatment")))
  attr(summary, "n_missing") <- n_missing
  summary
}

#' Wide metric table for display.
#'
#' @param metrics Output of [calculate_experiment_metrics()].
#' @return A tibble with one row per metric and one column per variant.
pivot_metrics <- function(metrics) {
  metrics |>
    dplyr::select(
      variant,
      users,
      conversion_rate,
      revenue_per_user,
      average_order_value,
      checkout_error_rate
    ) |>
    tidyr::pivot_longer(-variant, names_to = "metric", values_to = "value") |>
    tidyr::pivot_wider(names_from = variant, values_from = value) |>
    dplyr::arrange(match(
      metric,
      c("users", "conversion_rate", "revenue_per_user", "average_order_value", "checkout_error_rate")
    ))
}
