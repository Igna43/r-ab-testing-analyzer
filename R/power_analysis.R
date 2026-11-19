# Sample-size planning for a two-arm conversion test.
#
# This is a pre-experiment calculation. It is not observed power, and it is not
# a substitute for the estimate and confidence interval from the experiment.

#' Estimate the sample size for a two-sample conversion test.
#'
#' The calculation is the normal approximation used by [stats::power.prop.test()]
#' for a two-sided test without a continuity correction. For equal allocation it
#' is checked against that function. For unequal allocation, `allocation_ratio`
#' is treatment users per control user, and the null variance uses the
#' allocation-weighted average of the two rates.
#'
#' @param baseline_rate Control conversion probability.
#' @param mde Absolute treatment-minus-control difference to detect.
#' @param alpha Two-sided type I error.
#' @param power Desired power.
#' @param allocation_ratio `n_treatment / n_control`.
#' @return A list of rounded and unrounded sample sizes, plus the assumptions.
estimate_sample_size <- function(
    baseline_rate,
    mde,
    alpha = 0.05,
    power = 0.80,
    allocation_ratio = 1) {
  if (!is.numeric(baseline_rate) || length(baseline_rate) != 1 || baseline_rate <= 0 || baseline_rate >= 1) {
    stop("baseline_rate must be a probability strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(mde) || length(mde) != 1 || mde == 0 || !is.finite(mde)) {
    stop("mde must be a non-zero finite absolute difference.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a probability strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(power) || length(power) != 1 || power <= 0 || power >= 1) {
    stop("power must be a probability strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.numeric(allocation_ratio) || length(allocation_ratio) != 1 || allocation_ratio <= 0) {
    stop("allocation_ratio must be a positive treatment-to-control ratio.", call. = FALSE)
  }

  alternative_rate <- baseline_rate + mde
  if (alternative_rate <= 0 || alternative_rate >= 1) {
    stop("baseline_rate + mde must stay strictly between 0 and 1.", call. = FALSE)
  }

  ratio <- allocation_ratio
  p_bar <- (baseline_rate + ratio * alternative_rate) / (1 + ratio)
  z_alpha <- stats::qnorm(1 - alpha / 2)
  z_power <- stats::qnorm(power)
  null_component <- z_alpha * sqrt(p_bar * (1 - p_bar) * (1 + ratio) / ratio)
  alternative_component <- z_power * sqrt(
    baseline_rate * (1 - baseline_rate) + alternative_rate * (1 - alternative_rate) / ratio
  )
  n_control_unrounded <- ((null_component + alternative_component) / mde)^2
  n_treatment_unrounded <- ratio * n_control_unrounded

  reference_n <- NA_real_
  if (isTRUE(all.equal(ratio, 1))) {
    reference_n <- stats::power.prop.test(
      p1 = baseline_rate,
      p2 = alternative_rate,
      sig.level = alpha,
      power = power,
      alternative = "two.sided"
    )$n
  }

  list(
    baseline_rate = baseline_rate,
    alternative_rate = alternative_rate,
    mde = mde,
    alpha = alpha,
    power = power,
    allocation_ratio = ratio,
    n_control_unrounded = n_control_unrounded,
    n_treatment_unrounded = n_treatment_unrounded,
    n_total_unrounded = n_control_unrounded + n_treatment_unrounded,
    n_control = as.integer(ceiling(n_control_unrounded)),
    n_treatment = as.integer(ceiling(n_treatment_unrounded)),
    n_total = as.integer(ceiling(n_control_unrounded) + ceiling(n_treatment_unrounded)),
    reference_n_per_group = reference_n,
    method = paste(
      "Normal approximation to a two-sided two-sample proportion test,",
      "without a continuity correction."
    ),
    assumptions = paste(
      "Outcomes are independent Bernoulli trials.",
      "The minimum detectable effect is an absolute difference in conversion probability.",
      "The calculation sizes a future experiment. It is not the observed power of a completed one.",
      "Observed power is not used to interpret the estimate or the confidence interval."
    )
  )
}
