# Plots for the checkout experiment.
#
# Group-level intervals are 95% Wilson intervals from prop.test. They describe
# each variant on its own. They are not the interval for the treatment effect.
# Rate axes start at zero so small differences are not visually inflated.

variant_levels <- function() c("control", "treatment")

variant_labels <- function() c(control = "Control", treatment = "Treatment")

variant_colors <- function() c(Control = "#5C6B73", Treatment = "#1F4E79")

theme_experiment <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(color = "#4A4A4A"),
      plot.caption = ggplot2::element_text(color = "#5C5C5C", hjust = 0),
      legend.position = "bottom",
      axis.title = ggplot2::element_text(color = "#222222")
    )
}

label_variants <- function(data) {
  out <- data
  out$variant <- factor(
    out$variant,
    levels = variant_levels(),
    labels = unname(variant_labels())
  )
  out
}

rate_interval <- function(successes, n, conf_level = 0.95) {
  fit <- stats::prop.test(successes, n, conf.level = conf_level, correct = FALSE)
  c(low = unname(fit$conf.int[[1]]), high = unname(fit$conf.int[[2]]))
}

add_rate_intervals <- function(summary, success_col, n_col = "users") {
  intervals <- purrr::map2(summary[[success_col]], summary[[n_col]], rate_interval)
  summary$ci_low <- vapply(intervals, function(x) x[["low"]], numeric(1))
  summary$ci_high <- vapply(intervals, function(x) x[["high"]], numeric(1))
  summary
}

#' Conversion by variant with 95% intervals.
#' @param data Validated experiment table.
#' @return A ggplot.
plot_conversion_comparison <- function(data) {
  assert_analysis_frame(data, outcome_cols = "converted")
  summary <- data |>
    dplyr::group_by(variant) |>
    dplyr::summarise(users = dplyr::n(), conversions = sum(converted), rate = mean(converted), .groups = "drop") |>
    add_rate_intervals("conversions") |>
    label_variants()

  ggplot2::ggplot(summary, ggplot2::aes(variant, rate, fill = variant)) +
    ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low, ymax = ci_high), width = 0.12) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_fill_manual(values = variant_colors()) +
    ggplot2::labs(
      title = "Purchase conversion by variant",
      subtitle = "Share of assigned users who purchased",
      x = NULL,
      y = "Conversion rate",
      caption = "Bars are conversion rates. Intervals are 95% Wilson intervals for each variant, not for the difference."
    ) +
    theme_experiment()
}

#' Purchaser revenue by variant.
#' @param data Validated experiment table.
#' @return A ggplot.
plot_revenue_distribution <- function(data) {
  assert_analysis_frame(data, outcome_cols = "converted")
  if (!"revenue" %in% names(data)) {
    stop("revenue is required.", call. = FALSE)
  }
  purchasers <- data[data$converted == 1 & data$revenue > 0, , drop = FALSE]
  if (nrow(purchasers) == 0) {
    stop("No purchaser revenue to plot.", call. = FALSE)
  }
  purchasers <- label_variants(purchasers)
  upper <- stats::quantile(purchasers$revenue, 0.99, names = FALSE)

  ggplot2::ggplot(purchasers, ggplot2::aes(revenue, fill = variant)) +
    ggplot2::geom_histogram(bins = 40, show.legend = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(variant), ncol = 1) +
    ggplot2::coord_cartesian(xlim = c(0, upper)) +
    ggplot2::scale_x_continuous(labels = scales::label_dollar()) +
    ggplot2::scale_fill_manual(values = variant_colors()) +
    ggplot2::labs(
      title = "Purchase amount by variant",
      subtitle = "Purchasers only. Non-purchasers have revenue of zero and stay in the per-user mean.",
      x = "Purchase amount",
      y = "Purchasers",
      caption = "The horizontal axis stops at the 99th percentile so one extreme order does not flatten the distribution. It starts at zero."
    ) +
    theme_experiment()
}

#' Daily conversion by variant.
#' @param data Validated experiment table.
#' @return A ggplot.
plot_daily_conversion <- function(data) {
  daily <- summarise_daily(data) |>
    label_variants()

  ggplot2::ggplot(daily, ggplot2::aes(day, conversion_rate, color = variant)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_color_manual(values = variant_colors()) +
    ggplot2::scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
    ggplot2::labs(
      title = "Daily conversion rate",
      subtitle = "Each point is that day's assigned users",
      x = NULL,
      y = "Conversion rate",
      color = NULL,
      caption = "The vertical axis starts at zero. Day-to-day movement is mostly sampling noise."
    ) +
    theme_experiment()
}

#' Daily assignment volume by variant.
#' @param data Validated experiment table.
#' @return A ggplot.
plot_daily_volume <- function(data) {
  daily <- summarise_daily(data) |>
    label_variants()

  ggplot2::ggplot(daily, ggplot2::aes(day, users, fill = variant)) +
    ggplot2::geom_col(position = "dodge", width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08)), limits = c(0, NA)) +
    ggplot2::scale_fill_manual(values = variant_colors()) +
    ggplot2::scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
    ggplot2::labs(
      title = "Daily assignments",
      subtitle = "Weekdays are weighted more heavily in the simulator",
      x = NULL,
      y = "Assigned users",
      fill = NULL
    ) +
    theme_experiment()
}

#' Conversion by a pre-treatment group.
#' @param data Validated experiment table.
#' @param group_var Grouping column.
#' @param title Plot title.
#' @return A ggplot.
plot_conversion_by_group <- function(data, group_var, title = NULL) {
  summary <- summarise_by_group(data, group_var)
  n_missing <- attr(summary, "n_missing")
  summary <- summary |>
    add_rate_intervals("conversions", "users") |>
    label_variants() |>
    dplyr::rename(segment = group, rate = conversion_rate)
  if (is.null(title)) {
    title <- paste("Conversion rate by", gsub("_", " ", group_var))
  }

  ggplot2::ggplot(summary, ggplot2::aes(segment, rate, color = variant)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.4), size = 2.4) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci_low, ymax = ci_high),
      position = ggplot2::position_dodge(width = 0.4),
      width = 0.15
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_color_manual(values = variant_colors()) +
    ggplot2::labs(
      title = title,
      subtitle = "Exploratory slice of the same experiment",
      x = NULL,
      y = "Conversion rate",
      color = NULL,
      caption = if (n_missing > 0) {
        sprintf(
          "95%% Wilson intervals. %s users with a missing value are omitted from this chart only.",
          format(n_missing, big.mark = ",")
        )
      } else {
        "95% Wilson intervals for each group. These intervals are not multiplicity-adjusted."
      }
    ) +
    theme_experiment()
}

#' Checkout error rate by variant.
#' @param data Validated experiment table.
#' @return A ggplot.
plot_checkout_errors <- function(data) {
  assert_analysis_frame(data, outcome_cols = "checkout_error")
  summary <- data |>
    dplyr::group_by(variant) |>
    dplyr::summarise(users = dplyr::n(), conversions = sum(checkout_error), rate = mean(checkout_error), .groups = "drop") |>
    add_rate_intervals("conversions") |>
    label_variants()

  ggplot2::ggplot(summary, ggplot2::aes(variant, rate, fill = variant)) +
    ggplot2::geom_col(width = 0.68, show.legend = FALSE) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ci_low, ymax = ci_high), width = 0.12) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0, 0.08)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_fill_manual(values = variant_colors()) +
    ggplot2::labs(
      title = "Checkout error rate by variant",
      subtitle = "Guardrail, not a success metric",
      x = NULL,
      y = "Checkout error rate",
      caption = "95% Wilson intervals for each variant. Overlap does not prove the rates are equal."
    ) +
    theme_experiment()
}

#' Build the standard plot list.
#' @param data Validated experiment table.
#' @return A named list of ggplots.
build_experiment_plots <- function(data) {
  list(
    conversion_comparison = plot_conversion_comparison(data),
    revenue_distribution = plot_revenue_distribution(data),
    daily_conversion = plot_daily_conversion(data),
    daily_volume = plot_daily_volume(data),
    conversion_by_device = plot_conversion_by_group(data, "device_type", "Conversion rate by device"),
    conversion_by_channel = plot_conversion_by_group(
      data,
      "traffic_source",
      "Conversion rate by acquisition channel"
    ),
    checkout_errors = plot_checkout_errors(data)
  )
}

#' Write plots to PNG files.
#' @param plots Named list from [build_experiment_plots()].
#' @param directory Output directory.
#' @return The directory, invisibly.
save_experiment_plots <- function(plots, directory = "figures") {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  purrr::iwalk(plots, function(plot, name) {
    ggplot2::ggsave(
      filename = file.path(directory, paste0(name, ".png")),
      plot = plot,
      width = 8,
      height = 4.8,
      dpi = 150,
      bg = "white"
    )
  })
  invisible(directory)
}
