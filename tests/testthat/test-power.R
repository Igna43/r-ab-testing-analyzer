test_that("equal-allocation sample size matches power.prop.test", {
  estimated <- estimate_sample_size(0.10, 0.01, alpha = 0.05, power = 0.80, allocation_ratio = 1)
  reference <- stats::power.prop.test(
    p1 = 0.10,
    p2 = 0.11,
    sig.level = 0.05,
    power = 0.80,
    alternative = "two.sided"
  )

  expect_lt(abs(estimated$n_control_unrounded - reference$n) / reference$n, 0.01)
  expect_equal(estimated$n_control, as.integer(ceiling(estimated$n_control_unrounded)))
  expect_gt(estimated$n_control, 10000)
})

test_that("a smaller minimum detectable effect requires a larger sample", {
  small_effect <- estimate_sample_size(0.10, 0.01)
  large_effect <- estimate_sample_size(0.10, 0.02)
  stricter <- estimate_sample_size(0.10, 0.01, alpha = 0.01)
  higher_power <- estimate_sample_size(0.10, 0.01, power = 0.90)

  expect_gt(small_effect$n_control, large_effect$n_control)
  expect_gt(stricter$n_control, small_effect$n_control)
  expect_gt(higher_power$n_control, small_effect$n_control)
})

test_that("unequal allocation assigns more users to the larger arm and is less efficient", {
  equal <- estimate_sample_size(0.10, 0.01, allocation_ratio = 1)
  unequal <- estimate_sample_size(0.10, 0.01, allocation_ratio = 3)

  expect_gt(unequal$n_treatment_unrounded, unequal$n_control_unrounded)
  expect_gt(unequal$n_total_unrounded, equal$n_total_unrounded)
  expect_true(is.na(unequal$reference_n_per_group))
})

test_that("impossible sample-size inputs are rejected", {
  expect_error(estimate_sample_size(0, 0.01), "baseline_rate")
  expect_error(estimate_sample_size(0.10, 0), "mde")
  expect_error(estimate_sample_size(0.10, 0.95), "between 0 and 1")
  expect_error(estimate_sample_size(0.10, 0.01, alpha = 1), "alpha")
  expect_error(estimate_sample_size(0.10, 0.01, power = 0), "power")
  expect_error(estimate_sample_size(0.10, 0.01, allocation_ratio = 0), "allocation_ratio")
})
