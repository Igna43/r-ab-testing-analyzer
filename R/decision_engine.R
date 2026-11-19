# Business decision from pre-specified interval rules.
#
# The p-value is not an input to the decision. A checkout-error interval that
# includes zero is not treated as proof that the new experience is safe.

#' Thresholds used by [make_experiment_decision()].
#'
#' Rates are proportions. `0.005` is 0.5 percentage points, not 0.5%.
#' @return A list of analysis and decision settings.
default_decision_settings <- function() {
  list(
    alpha = 0.05,
    min_practical_lift = 0.005,
    max_acceptable_error_increase = 0.01,
    bootstrap_reps = 2000L,
    bootstrap_seed = 20241119L,
    planning_baseline_rate = 0.10,
    planning_mde = 0.01,
    planning_power = 0.80,
    planning_allocation_ratio = 1
  )
}

#' Rules, in the order they are applied.
#' @return A tibble.
decision_rules <- function() {
  tibble::tibble(
    order = 1:5,
    outcome = c(
      "Invalid experiment",
      "Do not ship",
      "Do not ship",
      "Ship",
      "Inconclusive"
    ),
    rule = c(
      "Any critical validation check failed.",
      "The checkout-error interval lies entirely above zero.",
      "The conversion interval lies entirely below zero, or entirely below the minimum practical lift.",
      "The conversion interval lies entirely at or above the minimum practical lift, and the checkout-error interval lies at or below the maximum acceptable increase.",
      "None of the earlier rules apply."
    )
  )
}

#' Recommend ship, do not ship, inconclusive, or invalid.
#'
#' @param validation Output of [validate_experiment()].
#' @param conversion Output of [analyze_conversion()].
#' @param guardrail Output of [analyze_guardrails()].
#' @param min_practical_lift Smallest conversion lift, as a proportion, that is worth shipping.
#' @param max_acceptable_error_increase Largest checkout-error increase, as a proportion, that is still acceptable.
#' @return A list with `decision`, `reasons`, and the thresholds used.
make_experiment_decision <- function(
    validation,
    conversion,
    guardrail,
    min_practical_lift = default_decision_settings()$min_practical_lift,
    max_acceptable_error_increase = default_decision_settings()$max_acceptable_error_increase) {
  if (!is.list(validation) || !is.logical(validation$ok) || length(validation$ok) != 1 || anyNA(validation$ok)) {
    stop("validation$ok must be a single TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(min_practical_lift) || length(min_practical_lift) != 1 ||
    min_practical_lift < 0 || min_practical_lift >= 1) {
    stop("min_practical_lift must be a proportion in [0, 1).", call. = FALSE)
  }
  if (!is.numeric(max_acceptable_error_increase) || length(max_acceptable_error_increase) != 1 ||
    max_acceptable_error_increase < 0 || max_acceptable_error_increase >= 1) {
    stop("max_acceptable_error_increase must be a proportion in [0, 1).", call. = FALSE)
  }

  thresholds <- list(
    min_practical_lift = min_practical_lift,
    max_acceptable_error_increase = max_acceptable_error_increase
  )
  if (!isTRUE(validation$ok)) {
    return(decision_result(
      "Invalid experiment",
      "Critical validation checks failed, so the metrics are not used to make a shipping decision.",
      thresholds
    ))
  }

  conversion_ci <- extract_interval(conversion, "conversion")
  guardrail_ci <- extract_interval(guardrail, "guardrail")

  reasons <- character()
  if (guardrail_ci[[1]] > 0) {
    reasons <- c(
      reasons,
      "Checkout errors are higher under treatment and the 95% interval excludes zero."
    )
  }
  if (conversion_ci[[2]] < 0) {
    reasons <- c(reasons, "The conversion interval lies entirely below zero.")
  } else if (conversion_ci[[2]] < min_practical_lift) {
    reasons <- c(
      reasons,
      "The conversion interval lies entirely below the minimum practical lift, so a meaningful improvement is ruled out."
    )
  }
  if (length(reasons) > 0) {
    return(decision_result("Do not ship", reasons, thresholds))
  }

  meaningful <- conversion_ci[[1]] >= min_practical_lift
  harm_bounded <- guardrail_ci[[2]] <= max_acceptable_error_increase
  if (meaningful && harm_bounded) {
    return(decision_result(
      "Ship",
      c(
        "The conversion interval lies entirely at or above the minimum practical lift.",
        "The checkout-error interval lies at or below the maximum acceptable increase."
      ),
      thresholds
    ))
  }
  if (meaningful && !harm_bounded) {
    return(decision_result(
      "Inconclusive",
      paste(
        "The conversion interval clears the practical bar, but the checkout-error interval still allows an increase above the acceptable limit.",
        "A nonsignificant error test is not treated as proof of safety."
      ),
      thresholds
    ))
  }
  decision_result(
    "Inconclusive",
    "The conversion interval still includes effects below the minimum practical lift, so it does not justify shipping or rejecting the redesign.",
    thresholds
  )
}

extract_interval <- function(result, label) {
  if (!is.list(result) || is.null(result$ci_low) || is.null(result$ci_high)) {
    stop(sprintf("%s must include ci_low and ci_high.", label), call. = FALSE)
  }
  interval <- c(result$ci_low, result$ci_high)
  if (length(interval) != 2 || any(!is.finite(interval)) || interval[[1]] > interval[[2]]) {
    stop(sprintf("%s confidence limits must be finite and ordered.", label), call. = FALSE)
  }
  interval
}

decision_result <- function(decision, reasons, thresholds) {
  list(
    decision = decision,
    reasons = as.character(reasons),
    summary = paste(as.character(reasons), collapse = " "),
    min_practical_lift = thresholds$min_practical_lift,
    max_acceptable_error_increase = thresholds$max_acceptable_error_increase
  )
}
