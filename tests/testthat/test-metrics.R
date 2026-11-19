test_that("metrics use assigned users and keep the treatment-minus-control direction", {
  metrics <- calculate_experiment_metrics(hand_users())

  control <- metrics[metrics$variant == "control", ]
  treatment <- metrics[metrics$variant == "treatment", ]

  expect_equal(control$users, 4)
  expect_equal(treatment$users, 4)
  expect_equal(control$conversion_rate, 0.25)
  expect_equal(treatment$conversion_rate, 0.50)
  expect_equal(control$revenue_per_user, 2.5)
  expect_equal(treatment$revenue_per_user, 12.5)
  expect_equal(control$average_order_value, 10)
  expect_equal(treatment$average_order_value, 25)
  expect_equal(control$checkout_error_rate, 0.25)
  expect_equal(treatment$checkout_error_rate, 0.25)
  expect_equal(treatment$revenue_per_user - control$revenue_per_user, 10)
})

test_that("revenue per assigned user includes users who did not purchase", {
  metrics <- calculate_experiment_metrics(hand_users())
  treatment <- metrics$revenue_per_user[metrics$variant == "treatment"]
  expect_equal(treatment, 50 / 4)
  expect_false(isTRUE(all.equal(treatment, 25)))
})

test_that("wide metrics keep control and treatment columns", {
  wide <- pivot_metrics(calculate_experiment_metrics(hand_users()))
  expect_true(all(c("metric", "control", "treatment") %in% names(wide)))
  expect_equal(wide$treatment[wide$metric == "conversion_rate"], 0.5)
  expect_equal(wide$control[wide$metric == "revenue_per_user"], 2.5)
})

test_that("metric calculations reject duplicate users and inconsistent revenue", {
  duplicated_users <- dplyr::bind_rows(hand_users(), hand_users()[1, ])
  expect_error(calculate_experiment_metrics(duplicated_users), "duplicates")

  inconsistent <- hand_users()
  inconsistent$revenue[1] <- 15
  expect_error(calculate_experiment_metrics(inconsistent), "disagree")

  missing_revenue <- hand_users()[, setdiff(names(hand_users()), "revenue")]
  expect_error(calculate_experiment_metrics(missing_revenue), "revenue")
})
