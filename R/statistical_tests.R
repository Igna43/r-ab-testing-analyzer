# Statistical comparisons.
#
# Every difference is treatment minus control. The conversion p-value is a
# pooled two-sided z-test. The reported interval is the unpooled Wald interval,
# so the interval and the p-value answer slightly different numerical questions.
# The p-value is checked against a hand-calculated pooled z-test, broom, and infer.
# stats::prop.test() returns the same Wald interval, truncated to [-1, 1].
# A Newcombe score interval is stored as a second method for the difference.

#' Compare a binary outcome between treatment and control.
#'
#' @param data Validated experiment table.
#' @param alpha Two-sided significance level.
#' @param outcome_col Binary column, usually `converted` or `checkout_error`.
#' @param treatment Treatment label.
#' @param control Control label.
#' @return A list of rates, the treatment-minus-control difference, interval, and checks.
analyze_conversion <- function(
    data,
    alpha = 0.05,
    outcome_col = "converted",
    treatment = "treatment",
    control = "control") {
  assert_analysis_frame(data, outcome_cols = outcome_col)
  alpha <- validate_alpha(alpha)
  counts <- group_binary_counts(data, outcome_col, treatment, control)
  estimate <- proportion_difference(counts$x_treatment, counts$n_treatment, counts$x_control, counts$n_control, alpha)

  proportion_test <- stats::prop.test(
    c(counts$x_treatment, counts$x_control),
    c(counts$n_treatment, counts$n_control),
    alternative = "two.sided",
    correct = FALSE,
    conf.level = 1 - alpha
  )
  tidy_test <- broom::tidy(proportion_test)
  manual_p <- pooled_z_p_value(estimate$absolute_difference, counts)
  infer_p <- infer_proportion_p_value(data, outcome_col, treatment, control, alpha)
  newcombe <- newcombe_difference_ci(
    counts$x_treatment,
    counts$n_treatment,
    counts$x_control,
    counts$n_control,
    alpha
  )
  # prop.test truncates the same unpooled Wald interval to [-1, 1].
  wald_matches_prop_test <- abs(estimate$ci_low - proportion_test$conf.int[[1]]) < 1e-8 &&
    abs(estimate$ci_high - proportion_test$conf.int[[2]]) < 1e-8

  p_checks_agree <- abs(tidy_test$p.value - manual_p) < 1e-8 &&
    abs(infer_p - manual_p) < 1e-6
  if (!p_checks_agree) {
    warning(
      "The proportion p-value checks disagreed. See p_value, p_value_pooled_z, and p_value_infer.",
      call. = FALSE
    )
  }

  excludes_zero <- estimate$ci_low > 0 || estimate$ci_high < 0
  list(
    outcome = outcome_col,
    control_rate = estimate$control_rate,
    treatment_rate = estimate$treatment_rate,
    control_successes = counts$x_control,
    treatment_successes = counts$x_treatment,
    control_n = counts$n_control,
    treatment_n = counts$n_treatment,
    absolute_difference = estimate$absolute_difference,
    relative_difference = estimate$relative_difference,
    ci_low = estimate$ci_low,
    ci_high = estimate$ci_high,
    ci_method = "Unpooled Wald interval for the treatment-minus-control difference",
    score_ci_low = newcombe[[1]],
    score_ci_high = newcombe[[2]],
    score_ci_method = "Newcombe hybrid score interval for the treatment-minus-control difference",
    wald_matches_prop_test = wald_matches_prop_test,
    alpha = alpha,
    p_value = unname(proportion_test$p.value),
    p_value_pooled_z = manual_p,
    p_value_infer = infer_p,
    p_checks_agree = p_checks_agree,
    statistic = unname(proportion_test$statistic),
    significant = unname(proportion_test$p.value) < alpha,
    wald_excludes_zero = excludes_zero,
    significance_matches_wald_ci = identical(excludes_zero, unname(proportion_test$p.value) < alpha),
    test_method = "Two-sided test of equal proportions (pooled z-test, continuity correction off)",
    estimate_direction = "treatment - control"
  )
}

#' Bootstrap the difference in mean revenue per assigned user.
#'
#' Users who did not purchase stay in the sample with revenue 0. Draws are
#' stratified by variant so each replicate keeps the original group sizes.
#' Revenue is zero-inflated and right-skewed, so the percentile bootstrap is
#' used instead of a normal interval for the mean.
#'
#' @param data Validated experiment table.
#' @param reps Bootstrap replicates. At least 2000.
#' @param seed Seed used only inside this function.
#' @param alpha Interval tail probability.
#' @return A list of means, the treatment-minus-control difference, and the percentile interval.
analyze_revenue <- function(data, reps = 2000L, seed = 20241119L, alpha = 0.05) {
  assert_analysis_frame(data, outcome_cols = "converted")
  if (!"revenue" %in% names(data) || anyNA(data$revenue) || any(data$revenue < 0)) {
    stop("revenue must be non-missing and non-negative.", call. = FALSE)
  }
  reps <- as.integer(reps)
  seed <- as.integer(seed)
  alpha <- validate_alpha(alpha)
  if (is.na(reps) || reps < 2000L) {
    stop("reps must be an integer of at least 2000.", call. = FALSE)
  }
  if (is.na(seed)) {
    stop("seed must be a single integer.", call. = FALSE)
  }

  control <- data$revenue[data$variant == "control"]
  treatment <- data$revenue[data$variant == "treatment"]
  control_mean <- mean(control)
  treatment_mean <- mean(treatment)
  absolute_difference <- treatment_mean - control_mean

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  draws <- replicate(reps, {
    mean(sample(treatment, length(treatment), replace = TRUE)) -
      mean(sample(control, length(control), replace = TRUE))
  })
  ci <- stats::quantile(draws, probs = c(alpha / 2, 1 - alpha / 2), type = 7, names = FALSE)

  list(
    control_mean = control_mean,
    treatment_mean = treatment_mean,
    absolute_difference = absolute_difference,
    relative_difference = if (control_mean == 0) NA_real_ else absolute_difference / control_mean,
    ci_low = unname(ci[[1]]),
    ci_high = unname(ci[[2]]),
    ci_method = "Percentile interval from a stratified bootstrap, treatment minus control",
    quantile_type = 7L,
    reps = reps,
    seed = seed,
    alpha = alpha,
    estimate_direction = "treatment - control",
    reason = paste(
      "Revenue per assigned user is zero-inflated and right-skewed.",
      "A normal interval for the mean can misstate the uncertainty.",
      "Resampling within each variant keeps the randomized group sizes fixed."
    )
  )
}

#' Compare checkout error rates.
#'
#' A nonsignificant result is not evidence that the new checkout is safe.
#' Use the upper bound of the difference against an explicit harm limit.
#'
#' @param data Validated experiment table.
#' @param alpha Two-sided significance level.
#' @return The [analyze_conversion()] result for `checkout_error`, plus an interpretation.
analyze_guardrails <- function(data, alpha = 0.05) {
  result <- analyze_conversion(data, alpha = alpha, outcome_col = "checkout_error")
  result$interpretation <- paste(
    "The difference is treatment minus control.",
    "An interval that includes zero does not prove the error rates are equal.",
    "Compare the upper bound with the largest error increase the business will accept."
  )
  result
}

#' Exploratory conversion effects by device and acquisition channel.
#'
#' These slices were not pre-registered as separate decisions. Intervals are
#' not adjusted for the number of comparisons. One significant slice and one
#' nonsignificant slice do not show that the effects differ.
#'
#' @param data Validated experiment table.
#' @param group_vars Grouping columns.
#' @param alpha Two-sided significance level used inside each slice.
#' @return A list with `estimates`, `caveat`, and `n_comparisons`.
analyze_subgroups <- function(
    data,
    group_vars = c("device_type", "traffic_source"),
    alpha = 0.05) {
  assert_analysis_frame(data, outcome_cols = "converted")
  alpha <- validate_alpha(alpha)
  missing_vars <- setdiff(group_vars, names(data))
  if (length(missing_vars) > 0) {
    stop(sprintf("Column not found: %s.", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  estimates <- purrr::map(group_vars, function(group_var) {
    levels <- sort(unique(data[[group_var]][!is.na(data[[group_var]])]))
    purrr::map(levels, function(level) {
      slice <- data[!is.na(data[[group_var]]) & data[[group_var]] == level, , drop = FALSE]
      arm_n <- c(
        control = sum(slice$variant == "control"),
        treatment = sum(slice$variant == "treatment")
      )
      base <- tibble::tibble(
        group_var = group_var,
        group = as.character(level),
        control_n = unname(arm_n[["control"]]),
        treatment_n = unname(arm_n[["treatment"]]),
        exploratory = TRUE
      )
      if (any(arm_n < 5L)) {
        return(dplyr::bind_cols(base, empty_effect_columns("Fewer than 5 users in one variant.")))
      }
      fit <- analyze_conversion(slice, alpha = alpha)
      dplyr::bind_cols(base, tibble::tibble(
        control_rate = fit$control_rate,
        treatment_rate = fit$treatment_rate,
        absolute_difference = fit$absolute_difference,
        relative_difference = fit$relative_difference,
        ci_low = fit$ci_low,
        ci_high = fit$ci_high,
        p_value = fit$p_value,
        note = "Estimated. Not adjusted for multiple comparisons."
      ))
    }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()

  list(
    estimates = estimates,
    caveat = paste(
      "Subgroup estimates are exploratory.",
      "Each interval is a separate 95% Wald interval, with no multiplicity adjustment.",
      "The chance of at least one false signal grows with the number of slices.",
      "A significant result in one slice and a nonsignificant result in another is not evidence that the effects differ."
    ),
    n_comparisons = nrow(estimates)
  )
}

validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single probability strictly between 0 and 1.", call. = FALSE)
  }
  alpha
}

group_binary_counts <- function(data, outcome_col, treatment, control) {
  list(
    x_treatment = sum(data[[outcome_col]][data$variant == treatment]),
    n_treatment = sum(data$variant == treatment),
    x_control = sum(data[[outcome_col]][data$variant == control]),
    n_control = sum(data$variant == control)
  )
}

proportion_difference <- function(x_treatment, n_treatment, x_control, n_control, alpha) {
  treatment_rate <- x_treatment / n_treatment
  control_rate <- x_control / n_control
  absolute_difference <- treatment_rate - control_rate
  se <- sqrt(
    treatment_rate * (1 - treatment_rate) / n_treatment +
      control_rate * (1 - control_rate) / n_control
  )
  z <- stats::qnorm(1 - alpha / 2)
  list(
    treatment_rate = treatment_rate,
    control_rate = control_rate,
    absolute_difference = absolute_difference,
    relative_difference = if (control_rate == 0) NA_real_ else absolute_difference / control_rate,
    ci_low = absolute_difference - z * se,
    ci_high = absolute_difference + z * se
  )
}

pooled_z_p_value <- function(absolute_difference, counts) {
  p_pool <- (counts$x_treatment + counts$x_control) / (counts$n_treatment + counts$n_control)
  se_pool <- sqrt(p_pool * (1 - p_pool) * (1 / counts$n_treatment + 1 / counts$n_control))
  if (se_pool == 0) {
    return(if (absolute_difference == 0) 1 else 0)
  }
  z_stat <- absolute_difference / se_pool
  2 * stats::pnorm(-abs(z_stat))
}

infer_proportion_p_value <- function(data, outcome_col, treatment, control, alpha) {
  # infer requires a categorical response. The stored column stays numeric.
  checked <- data
  checked[[outcome_col]] <- factor(checked[[outcome_col]], levels = c(0, 1))
  fit <- infer::prop_test(
    checked,
    stats::as.formula(paste(outcome_col, "~ variant")),
    order = c(treatment, control),
    success = "1",
    alternative = "two.sided",
    conf_int = TRUE,
    conf_level = 1 - alpha,
    correct = FALSE
  )
  if ("p_value" %in% names(fit)) {
    return(fit$p_value[[1]])
  }
  fit$p.value[[1]]
}

wilson_bounds <- function(successes, n, z) {
  estimate <- successes / n
  denominator <- 1 + z^2 / n
  center <- (estimate + z^2 / (2 * n)) / denominator
  half <- z * sqrt(estimate * (1 - estimate) / n + z^2 / (4 * n^2)) / denominator
  c(center - half, center + half)
}

newcombe_difference_ci <- function(x_treatment, n_treatment, x_control, n_control, alpha) {
  z <- stats::qnorm(1 - alpha / 2)
  treatment <- wilson_bounds(x_treatment, n_treatment, z)
  control <- wilson_bounds(x_control, n_control, z)
  p_treatment <- x_treatment / n_treatment
  p_control <- x_control / n_control
  difference <- p_treatment - p_control
  c(
    difference - sqrt((p_treatment - treatment[[1]])^2 + (control[[2]] - p_control)^2),
    difference + sqrt((treatment[[2]] - p_treatment)^2 + (p_control - control[[1]])^2)
  )
}

empty_effect_columns <- function(note) {
  tibble::tibble(
    control_rate = NA_real_,
    treatment_rate = NA_real_,
    absolute_difference = NA_real_,
    relative_difference = NA_real_,
    ci_low = NA_real_,
    ci_high = NA_real_,
    p_value = NA_real_,
    note = note
  )
}
