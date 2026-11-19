validation_ok <- function() list(ok = TRUE)
validation_bad <- function() list(ok = FALSE)

interval_result <- function(low, high) list(ci_low = low, ci_high = high)

test_that("a practical positive effect and a bounded guardrail ship", {
  decision <- make_experiment_decision(
    validation_ok(),
    interval_result(0.008, 0.020),
    interval_result(-0.002, 0.004),
    min_practical_lift = 0.005,
    max_acceptable_error_increase = 0.01
  )
  expect_equal(decision$decision, "Ship")
})

test_that("a confidently negative conversion effect does not ship", {
  decision <- make_experiment_decision(
    validation_ok(),
    interval_result(-0.030, -0.005),
    interval_result(-0.002, 0.004)
  )
  expect_equal(decision$decision, "Do not ship")
  expect_match(decision$summary, "below zero")
})

test_that("an effect below the practical bar does not ship even if it is positive", {
  decision <- make_experiment_decision(
    validation_ok(),
    interval_result(0.001, 0.004),
    interval_result(-0.001, 0.001),
    min_practical_lift = 0.005
  )
  expect_equal(decision$decision, "Do not ship")
  expect_match(decision$summary, "minimum practical lift")
})

test_that("a wide conversion interval is inconclusive", {
  decision <- make_experiment_decision(
    validation_ok(),
    interval_result(-0.002, 0.020),
    interval_result(-0.002, 0.004),
    min_practical_lift = 0.005
  )
  expect_equal(decision$decision, "Inconclusive")
})

test_that("a positive conversion result with an unresolved guardrail is inconclusive", {
  decision <- make_experiment_decision(
    validation_ok(),
    interval_result(0.008, 0.020),
    interval_result(-0.001, 0.020),
    min_practical_lift = 0.005,
    max_acceptable_error_increase = 0.01
  )
  expect_equal(decision$decision, "Inconclusive")
  expect_match(decision$summary, "not treated as proof of safety")
})

test_that("a detected checkout-error increase does not ship", {
  decision <- make_experiment_decision(
    validation_ok(),
    interval_result(0.008, 0.020),
    interval_result(0.001, 0.020),
    min_practical_lift = 0.005,
    max_acceptable_error_increase = 0.05
  )
  expect_equal(decision$decision, "Do not ship")
  expect_match(decision$summary, "Checkout errors")
})

test_that("critical validation makes the experiment invalid without reading the metrics", {
  decision <- make_experiment_decision(validation_bad(), conversion = list(), guardrail = list())
  expect_equal(decision$decision, "Invalid experiment")
})

test_that("decision thresholds must be proportions", {
  expect_error(
    make_experiment_decision(validation_ok(), interval_result(0, 1), interval_result(0, 1), min_practical_lift = 2),
    "min_practical_lift"
  )
})
