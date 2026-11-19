test_that("conversion effects use treatment minus control and an unpooled Wald interval", {
  fit <- analyze_conversion(hand_users())
  difference <- 0.50 - 0.25
  standard_error <- sqrt(0.50 * 0.50 / 4 + 0.25 * 0.75 / 4)
  z <- stats::qnorm(0.975)

  expect_equal(fit$control_rate, 0.25)
  expect_equal(fit$treatment_rate, 0.50)
  expect_equal(fit$absolute_difference, difference)
  expect_equal(fit$relative_difference, 1)
  expect_equal(fit$ci_low, difference - z * standard_error)
  expect_equal(fit$ci_high, difference + z * standard_error)
  expect_equal(fit$estimate_direction, "treatment - control")
  expect_true(fit$p_checks_agree)
  expect_equal(fit$p_value, fit$p_value_pooled_z, tolerance = 1e-8)
  expect_equal(fit$p_value, fit$p_value_infer, tolerance = 1e-6)
  expect_true(fit$wald_matches_prop_test)
  expect_gt(fit$score_ci_high, 0)
  expect_lt(fit$score_ci_low, fit$absolute_difference)
  expect_gt(fit$score_ci_high, fit$absolute_difference)
  expect_match(fit$score_ci_method, "Newcombe")
})

test_that("swapping variant labels flips the sign of the conversion effect", {
  original <- analyze_conversion(hand_users())
  swapped <- hand_users()
  swapped$variant <- ifelse(swapped$variant == "control", "treatment", "control")
  flipped <- analyze_conversion(swapped)

  expect_equal(flipped$absolute_difference, -original$absolute_difference)
  expect_equal(flipped$control_rate, original$treatment_rate)
  expect_equal(flipped$treatment_rate, original$control_rate)
})

test_that("the revenue bootstrap is reproducible and includes non-purchasers", {
  first <- analyze_revenue(hand_users(), reps = 2000, seed = 11)
  second <- analyze_revenue(hand_users(), reps = 2000, seed = 11)

  expect_equal(first$control_mean, 2.5)
  expect_equal(first$treatment_mean, 12.5)
  expect_equal(first$absolute_difference, 10)
  expect_equal(first$relative_difference, 4)
  expect_equal(first$ci_low, second$ci_low)
  expect_equal(first$ci_high, second$ci_high)
  expect_lt(first$ci_low, first$absolute_difference)
  expect_gt(first$ci_high, first$absolute_difference)
  expect_equal(first$reps, 2000)
  expect_equal(first$seed, 11)
})

test_that("a larger treatment order value moves the revenue difference", {
  baseline <- analyze_revenue(hand_users(), reps = 2000, seed = 11)
  richer <- hand_users()
  richer$revenue[richer$variant == "treatment" & richer$revenue > 0] <-
    richer$revenue[richer$variant == "treatment" & richer$revenue > 0] + 100
  shifted <- analyze_revenue(richer, reps = 2000, seed = 11)

  expect_gt(shifted$absolute_difference, baseline$absolute_difference)
})

test_that("equal checkout error rates have a difference of zero", {
  guardrail <- analyze_guardrails(hand_users())
  expect_equal(guardrail$absolute_difference, 0)
  expect_equal(guardrail$ci_low, -guardrail$ci_high)
  expect_match(guardrail$interpretation, "does not prove")
})

test_that("subgroup output is marked exploratory", {
  subgroups <- analyze_subgroups(hand_users(), group_vars = "device_type")
  expect_true(all(subgroups$estimates$exploratory))
  expect_match(subgroups$caveat, "not evidence that the effects differ")
  expect_equal(subgroups$n_comparisons, nrow(subgroups$estimates))
})

test_that("revenue bootstrap rejects too few replicates", {
  expect_error(analyze_revenue(hand_users(), reps = 100, seed = 1), "2000")
})
