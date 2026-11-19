# Experiment validation.
#
# Critical issues block analysis. The functions below report them and leave the
# input untouched. They do not delete or repair rows.

#' Columns required by the analysis.
#' @return Character vector.
required_experiment_columns <- function() {
  c(
    "user_id",
    "variant",
    "assignment_timestamp",
    "device_type",
    "traffic_source",
    "country",
    "converted",
    "revenue",
    "checkout_error"
  )
}

#' Stop when a table is not ready for estimation.
#'
#' This is narrower than [validate_experiment()]: it checks the fields an
#' estimator needs and refuses to drop bad rows.
#' @param data Experiment table.
#' @param outcome_cols Outcome columns that must be 0/1 with no missing values.
#' @return The input, invisibly.
assert_analysis_frame <- function(data, outcome_cols = "converted") {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  required <- c("user_id", "variant", outcome_cols)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      sprintf("Missing required columns: %s.", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  if (nrow(data) == 0) {
    stop("data has no rows.", call. = FALSE)
  }
  user_id <- as.character(data$user_id)
  if (anyNA(user_id) || any(!nzchar(user_id))) {
    stop("user_id contains missing values. Resolve validation issues before analysis.", call. = FALSE)
  }
  if (anyDuplicated(user_id) > 0) {
    stop("user_id has duplicates. Resolve validation issues before analysis.", call. = FALSE)
  }
  if (any(!data$variant %in% c("control", "treatment"))) {
    stop("variant must contain only control and treatment.", call. = FALSE)
  }
  if (!all(c("control", "treatment") %in% data$variant)) {
    stop("Both control and treatment must be present.", call. = FALSE)
  }
  for (outcome in outcome_cols) {
    values <- data[[outcome]]
    if (anyNA(values) || any(!values %in% c(0, 1))) {
      stop(
        sprintf("%s must be 0 or 1 with no missing values.", outcome),
        call. = FALSE
      )
    }
  }
  invisible(data)
}

#' Validate an experiment table and build a severity report.
#'
#' Sample-ratio mismatch uses a chi-squared goodness-of-fit test. The critical
#' threshold is 0.001, which is the usual bar for this check: at large sample
#' sizes a 0.05 cutoff flags healthy experiments. Covariate balance is
#' descriptive. A small p-value there is a warning to inspect, not proof that
#' randomization failed, and it does not block analysis.
#'
#' @param data One row per assigned user.
#' @param expected_allocation Named probabilities for control and treatment.
#' @param experiment_start Inclusive UTC start. `NULL` uses the generator default.
#' @param experiment_end Exclusive UTC end. `NULL` uses the generator default.
#' @param srm_alpha Critical p-value for sample ratio mismatch.
#' @return A list with `ok`, `issues`, `balance`, `srm`, and issue counts.
validate_experiment <- function(
    data,
    expected_allocation = c(control = 0.5, treatment = 0.5),
    experiment_start = NULL,
    experiment_end = NULL,
    srm_alpha = 0.001) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (!is.numeric(srm_alpha) || length(srm_alpha) != 1 || srm_alpha <= 0 || srm_alpha >= 1) {
    stop("srm_alpha must be a single probability strictly between 0 and 1.", call. = FALSE)
  }
  defaults <- default_generation_parameters()
  if (is.null(experiment_start) || is.null(experiment_end)) {
    start_default <- as.POSIXct(defaults$start_date, tz = "UTC")
    if (is.null(experiment_start)) {
      experiment_start <- start_default
    }
    if (is.null(experiment_end)) {
      experiment_end <- start_default + lubridate::days(defaults$n_days)
    }
  }
  experiment_start <- as.POSIXct(experiment_start, tz = "UTC")
  experiment_end <- as.POSIXct(experiment_end, tz = "UTC")
  if (is.na(experiment_start) || is.na(experiment_end) || experiment_end <= experiment_start) {
    stop("experiment_end must be later than experiment_start.", call. = FALSE)
  }
  allocation <- validate_allocation(expected_allocation)

  issues <- list()
  add_issue <- function(check_id, severity, n_affected, message) {
    issues[[length(issues) + 1]] <<- tibble::tibble(
      check_id = check_id,
      severity = severity,
      n_affected = as.integer(n_affected),
      message = message
    )
  }

  missing_cols <- setdiff(required_experiment_columns(), names(data))
  if (length(missing_cols) > 0) {
    add_issue(
      "required_columns",
      "critical",
      length(missing_cols),
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
    return(validation_result(issues, balance = empty_balance(), srm = NULL))
  }
  add_issue("required_columns", "info", 0L, "All required columns are present.")

  if (nrow(data) == 0) {
    add_issue("data_types", "critical", 0L, "The table has no rows.")
    return(validation_result(issues, balance = empty_balance(), srm = NULL))
  }

  type_problems <- collect_type_problems(data)
  if (length(type_problems) > 0) {
    add_issue(
      "data_types",
      "critical",
      nrow(data),
      paste(type_problems, collapse = " ")
    )
  } else {
    add_issue("data_types", "info", 0L, "Column types can be used for the analysis.")
  }

  user_id <- as.character(data$user_id)
  missing_id <- is.na(data$user_id) | !nzchar(trimws(user_id))
  if (any(missing_id)) {
    add_issue(
      "missing_user_id",
      "critical",
      sum(missing_id),
      sprintf("%d rows have a missing user_id.", sum(missing_id))
    )
  } else {
    add_issue("missing_user_id", "info", 0L, "No user_id values are missing.")
  }

  present_ids <- user_id[!missing_id]
  n_extra_rows <- sum(duplicated(present_ids))
  n_repeated_ids <- length(unique(present_ids[duplicated(present_ids)]))
  if (n_extra_rows > 0) {
    add_issue(
      "duplicate_user_id",
      "critical",
      n_extra_rows,
      sprintf(
        "%d user IDs are repeated, adding %d extra rows.",
        n_repeated_ids,
        n_extra_rows
      )
    )
  } else {
    add_issue("duplicate_user_id", "info", 0L, "No user_id values are duplicated.")
  }

  invalid_variant <- is.na(data$variant) | !data$variant %in% names(allocation)
  if (any(invalid_variant)) {
    add_issue(
      "invalid_variant",
      "critical",
      sum(invalid_variant),
      sprintf(
        "%d rows have a variant outside {%s}.",
        sum(invalid_variant),
        paste(names(allocation), collapse = ", ")
      )
    )
  } else {
    add_issue("invalid_variant", "info", 0L, "Every variant is control or treatment.")
  }

  bad_converted <- is.na(data$converted) | !data$converted %in% c(0, 1)
  bad_error <- is.na(data$checkout_error) | !data$checkout_error %in% c(0, 1)
  bad_revenue <- is.na(data$revenue) | data$revenue < 0
  n_invalid_outcome <- sum(bad_converted) + sum(bad_error) + sum(bad_revenue)
  if (n_invalid_outcome > 0) {
    add_issue(
      "invalid_outcome",
      "critical",
      n_invalid_outcome,
      paste(
        c(
          invalid_binary(data$converted, "converted"),
          invalid_binary(data$checkout_error, "checkout_error"),
          if (any(bad_revenue)) sprintf("%d rows have missing or negative revenue.", sum(bad_revenue))
        ),
        collapse = " "
      )
    )
  } else {
    add_issue("invalid_outcome", "info", 0L, "Outcomes and revenue have usable values.")
  }

  comparable <- !is.na(data$converted) & data$converted %in% c(0, 1) & !bad_revenue
  inconsistent <- comparable & (
    (data$converted == 0 & data$revenue > 0) |
      (data$converted == 1 & data$revenue <= 0)
  )
  if (any(inconsistent)) {
    add_issue(
      "inconsistent_conversion_revenue",
      "critical",
      sum(inconsistent),
      sprintf(
        "%d rows disagree: non-purchasers must have revenue 0, and purchasers must have revenue above 0.",
        sum(inconsistent)
      )
    )
  } else {
    add_issue(
      "inconsistent_conversion_revenue",
      "info",
      0L,
      "Conversion and revenue agree on every row."
    )
  }

  timestamps <- parse_assignment_time(data$assignment_timestamp)
  if (is.null(timestamps$values)) {
    add_issue("invalid_timestamp", "critical", nrow(data), timestamps$message)
  } else {
    if (!is.null(timestamps$message)) {
      add_issue("invalid_timestamp", "warning", nrow(data), timestamps$message)
    }
    missing_time <- is.na(timestamps$values)
    outside <- !missing_time & (
      timestamps$values < experiment_start | timestamps$values >= experiment_end
    )
    n_time <- sum(missing_time | outside)
    if (n_time > 0) {
      add_issue(
        "invalid_timestamp",
        "critical",
        n_time,
        sprintf(
          "%d timestamps are missing and %d fall outside [%s, %s).",
          sum(missing_time),
          sum(outside),
          format(experiment_start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          format(experiment_end, "%Y-%m-%d %H:%M:%S", tz = "UTC")
        )
      )
    } else if (is.null(timestamps$message)) {
      add_issue("invalid_timestamp", "info", 0L, "Every timestamp falls inside the experiment window.")
    }
  }

  srm <- calculate_srm(data$variant, allocation)
  srm_severity <- if (is.na(srm$p_value)) {
    "critical"
  } else if (srm$p_value < srm_alpha) {
    "critical"
  } else {
    "info"
  }
  add_issue(
    "sample_ratio_mismatch",
    srm_severity,
    srm$n_affected,
    sprintf(
      "Chi-squared goodness of fit is %.2f on %d df (p = %s). Observed %s; expected allocation %s. Critical when p < %s.",
      srm$statistic,
      srm$df,
      format_p(srm$p_value),
      paste(sprintf("%s %d", names(srm$observed), srm$observed), collapse = ", "),
      paste(sprintf("%s %.0f%%", names(allocation), 100 * allocation), collapse = ", "),
      format(srm_alpha, scientific = FALSE)
    )
  )

  for (covariate in c("device_type", "traffic_source", "country")) {
    missing_covariate <- is.na(data[[covariate]]) | !nzchar(as.character(data[[covariate]]))
    if (any(missing_covariate)) {
      add_issue(
        "missing_covariate",
        "warning",
        sum(missing_covariate),
        sprintf("%d rows are missing %s. Primary analysis keeps these users.", sum(missing_covariate), covariate)
      )
    }
  }

  balance <- covariate_balance(data)
  for (i in seq_len(nrow(balance))) {
    if (is.na(balance$statistic[[i]])) {
      add_issue(
        "covariate_balance",
        "info",
        balance$n[[i]],
        sprintf(
          "%s was not compared because it has fewer than two observed levels. This is descriptive, not a randomization verdict.",
          balance$covariate[[i]]
        )
      )
      next
    }
    add_issue(
      "covariate_balance",
      balance$severity[[i]],
      balance$n[[i]],
      sprintf(
        "%s balance: chi-squared = %.2f, df = %d, p = %s. %s",
        balance$covariate[[i]],
        balance$statistic[[i]],
        balance$df[[i]],
        format_p(balance$p_value[[i]]),
        if (balance$severity[[i]] == "warning") {
          "Extreme imbalance is a reason to inspect assignment, not proof that randomization failed."
        } else {
          "Descriptive only. This is not treated as proof of a randomization failure."
        }
      )
    )
  }

  validation_result(issues, balance = balance, srm = srm)
}

#' Allocation weights that sum to one.
#' @param expected_allocation Named numeric vector.
#' @return Named probabilities in control, treatment order.
validate_allocation <- function(expected_allocation) {
  if (is.null(names(expected_allocation)) || !setequal(names(expected_allocation), c("control", "treatment"))) {
    stop("expected_allocation must be named control and treatment.", call. = FALSE)
  }
  allocation <- expected_allocation[c("control", "treatment")]
  if (any(!is.finite(allocation)) || any(allocation <= 0)) {
    stop("expected_allocation values must be positive and finite.", call. = FALSE)
  }
  allocation / sum(allocation)
}

validation_result <- function(issues, balance, srm) {
  issue_table <- dplyr::bind_rows(issues)
  list(
    ok = !any(issue_table$severity == "critical"),
    issues = issue_table,
    balance = balance,
    srm = srm,
    n_critical = sum(issue_table$severity == "critical"),
    n_warning = sum(issue_table$severity == "warning"),
    n_info = sum(issue_table$severity == "info")
  )
}

empty_balance <- function() {
  tibble::tibble(
    covariate = character(),
    statistic = double(),
    df = integer(),
    p_value = double(),
    n = integer(),
    n_missing = integer(),
    severity = character()
  )
}

collect_type_problems <- function(data) {
  problems <- character()
  if (!is.character(data$user_id) && !is.factor(data$user_id) && !is.numeric(data$user_id)) {
    problems <- c(problems, "user_id must be character or numeric.")
  }
  if (!is.character(data$variant) && !is.factor(data$variant)) {
    problems <- c(problems, "variant must be character.")
  }
  if (!inherits(data$assignment_timestamp, "POSIXct") &&
    !inherits(data$assignment_timestamp, "Date") &&
    !is.character(data$assignment_timestamp)) {
    problems <- c(problems, "assignment_timestamp must be POSIXct, Date, or character.")
  }
  for (column in c("device_type", "traffic_source", "country")) {
    if (!is.character(data[[column]]) && !is.factor(data[[column]])) {
      problems <- c(problems, sprintf("%s must be character.", column))
    }
  }
  for (column in c("converted", "revenue", "checkout_error")) {
    if (!is.numeric(data[[column]])) {
      problems <- c(problems, sprintf("%s must be numeric.", column))
    }
  }
  problems
}

invalid_binary <- function(values, label) {
  bad <- is.na(values) | !values %in% c(0, 1)
  if (!any(bad)) {
    return(character())
  }
  sprintf("%d rows have a %s value outside {0, 1}.", sum(bad), label)
}

parse_assignment_time <- function(timestamps) {
  if (inherits(timestamps, "POSIXct")) {
    return(list(values = lubridate::with_tz(timestamps, tzone = "UTC"), message = NULL))
  }
  if (inherits(timestamps, "Date")) {
    return(list(
      values = as.POSIXct(timestamps, tz = "UTC"),
      message = "assignment_timestamp was a Date and was read as UTC midnight."
    ))
  }
  if (is.character(timestamps)) {
    parsed <- lubridate::parse_date_time(timestamps, orders = c("Ymd HMS", "Ymd HM", "Ymd"), tz = "UTC")
    return(list(
      values = parsed,
      message = "assignment_timestamp was text and was parsed as UTC."
    ))
  }
  list(values = NULL, message = "assignment_timestamp could not be parsed.")
}

calculate_srm <- function(variant, allocation) {
  observed <- vapply(names(allocation), function(level) {
    sum(variant == level, na.rm = TRUE)
  }, integer(1))
  names(observed) <- names(allocation)
  n <- sum(observed)
  if (n == 0) {
    return(list(
      statistic = NA_real_,
      df = length(allocation) - 1L,
      p_value = NA_real_,
      observed = observed,
      expected = observed,
      n_affected = 0L
    ))
  }
  expected <- n * allocation
  statistic <- sum((observed - expected)^2 / expected)
  df <- length(allocation) - 1L
  p_value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)
  list(
    statistic = unname(statistic),
    df = df,
    p_value = p_value,
    observed = observed,
    expected = expected,
    n_affected = as.integer(round(sum(abs(observed - expected)) / 2))
  )
}

covariate_balance <- function(data) {
  rows <- lapply(c("device_type", "traffic_source", "country"), function(covariate) {
    values <- as.character(data[[covariate]])
    n_missing <- sum(is.na(data[[covariate]]) | !nzchar(values))
    keep <- !is.na(data[[covariate]]) & nzchar(values) & data$variant %in% c("control", "treatment")
    tab <- table(values[keep], data$variant[keep])
    if (nrow(tab) < 2 || ncol(tab) < 2) {
      return(tibble::tibble(
        covariate = covariate,
        statistic = NA_real_,
        df = NA_integer_,
        p_value = NA_real_,
        n = sum(keep),
        n_missing = n_missing,
        severity = "info"
      ))
    }
    test <- stats::chisq.test(tab, correct = FALSE)
    tidy <- broom::tidy(test)
    severity <- if (!is.na(tidy$p.value) && tidy$p.value < 0.001) "warning" else "info"
    tibble::tibble(
      covariate = covariate,
      statistic = tidy$statistic,
      df = as.integer(tidy$parameter),
      p_value = tidy$p.value,
      n = sum(keep),
      n_missing = as.integer(n_missing),
      severity = severity
    )
  })
  dplyr::bind_rows(rows)
}

format_p <- function(p_value) {
  if (length(p_value) != 1 || is.na(p_value)) {
    return("NA")
  }
  if (p_value < 0.001) {
    return(formatC(p_value, format = "e", digits = 2))
  }
  sprintf("%.3f", p_value)
}
