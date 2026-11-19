# Generate the synthetic experiment, validate it, and print the decision.
# Run from the project root:
#   Rscript scripts/run_analysis.R

Sys.setenv(TZ = "UTC")

if (!file.exists(file.path("R", "metrics.R"))) {
  stop("Run this script from the project root.", call. = FALSE)
}

library(tidyverse)
library(broom)
library(infer)
library(scales)
library(knitr)

invisible(lapply(sort(list.files("R", pattern = "\\.R$", full.names = TRUE)), source))

experiment <- generate_experiment_data()
write_experiment_outputs(experiment)

validation <- validate_experiment(
  experiment$users,
  experiment_start = experiment$parameters$experiment_start,
  experiment_end = experiment$parameters$experiment_end
)
if (!isTRUE(validation$ok)) {
  print(validation$issues)
  stop("Critical validation checks failed. Analysis was not run.", call. = FALSE)
}

settings <- default_decision_settings()
users <- experiment$users
metrics <- calculate_experiment_metrics(users)
conversion <- analyze_conversion(users, alpha = settings$alpha)
revenue <- analyze_revenue(
  users,
  reps = settings$bootstrap_reps,
  seed = settings$bootstrap_seed,
  alpha = settings$alpha
)
guardrail <- analyze_guardrails(users, alpha = settings$alpha)
subgroups <- analyze_subgroups(users, alpha = settings$alpha)
sample_size <- estimate_sample_size(
  baseline_rate = settings$planning_baseline_rate,
  mde = settings$planning_mde,
  alpha = settings$alpha,
  power = settings$planning_power,
  allocation_ratio = settings$planning_allocation_ratio
)
decision <- make_experiment_decision(
  validation,
  conversion,
  guardrail,
  min_practical_lift = settings$min_practical_lift,
  max_acceptable_error_increase = settings$max_acceptable_error_increase
)

score_conversion <- conversion
score_conversion$ci_low <- conversion$score_ci_low
score_conversion$ci_high <- conversion$score_ci_high
score_decision <- make_experiment_decision(
  validation,
  score_conversion,
  guardrail,
  min_practical_lift = settings$min_practical_lift,
  max_acceptable_error_increase = settings$max_acceptable_error_increase
)

plots <- build_experiment_plots(users)
save_experiment_plots(plots, "figures")

pct <- function(x) scales::percent(x, accuracy = 0.01)
pp <- function(x) sprintf("%+.2f pp", 100 * x)

cat("\nCheckout experiment\n")
cat("Synthetic data. Seed ", experiment$parameters$seed, ", ", nrow(users), " users.\n", sep = "")
cat("Decision: ", decision$decision, "\n", sep = "")
cat(decision$summary, "\n\n")
cat("Conversion control: ", pct(conversion$control_rate), "\n", sep = "")
cat("Conversion treatment: ", pct(conversion$treatment_rate), "\n", sep = "")
cat("Conversion difference: ", pp(conversion$absolute_difference), "\n", sep = "")
cat("Conversion relative: ", pct(conversion$relative_difference), "\n", sep = "")
cat(
  "Conversion 95% Wald CI: ",
  pp(conversion$ci_low), " to ", pp(conversion$ci_high), "\n",
  sep = ""
)
cat("Conversion p-value: ", format(conversion$p_value, digits = 4), "\n", sep = "")
cat("P-value checks agree: ", conversion$p_checks_agree, "\n", sep = "")
cat(
  "Newcombe-interval decision: ", score_decision$decision,
  if (score_decision$decision == decision$decision) " (same recommendation)" else " (recommendation changes)",
  "\n",
  sep = ""
)
cat("Revenue control: ", scales::dollar(revenue$control_mean), "\n", sep = "")
cat("Revenue treatment: ", scales::dollar(revenue$treatment_mean), "\n", sep = "")
cat("Revenue difference: ", scales::dollar(revenue$absolute_difference), "\n", sep = "")
cat(
  "Revenue 95% bootstrap CI: ",
  scales::dollar(revenue$ci_low), " to ", scales::dollar(revenue$ci_high), "\n",
  sep = ""
)
cat("Error control: ", pct(guardrail$control_rate), "\n", sep = "")
cat("Error treatment: ", pct(guardrail$treatment_rate), "\n", sep = "")
cat("Error difference: ", pp(guardrail$absolute_difference), "\n", sep = "")
cat(
  "Error 95% CI: ", pp(guardrail$ci_low), " to ", pp(guardrail$ci_high), "\n",
  sep = ""
)
cat("Error p-value: ", format(guardrail$p_value, digits = 4), "\n", sep = "")
cat(
  "Planning sample size per variant at a 1 pp MDE: ",
  format(sample_size$n_control, big.mark = ","), "\n",
  sep = ""
)
cat("Subgroup comparisons: ", subgroups$n_comparisons, "\n", sep = "")
cat("Validation warnings: ", validation$n_warning, "\n", sep = "")
cat("Figures written to figures/\n")
