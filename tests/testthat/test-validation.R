test_that("a balanced table passes and is not modified", {
  users <- valid_users()
  snapshot <- users
  validation <- validate_experiment(users)

  expect_true(validation$ok)
  expect_equal(validation$n_critical, 0)
  expect_identical(users, snapshot)
  expect_false(is.data.frame(validation))
})

test_that("missing required columns are critical and stop the report", {
  users <- valid_users()
  users$user_id <- NULL
  validation <- validate_experiment(users)

  expect_false(validation$ok)
  expect_equal(validation$issues$check_id, "required_columns")
  expect_equal(validation$issues$severity, "critical")
  expect_match(validation$issues$message, "user_id")
})

test_that("duplicate user IDs are critical", {
  users <- dplyr::bind_rows(valid_users(), valid_users()[1, ])
  validation <- validate_experiment(users)
  issue <- validation$issues[validation$issues$check_id == "duplicate_user_id", ]

  expect_false(validation$ok)
  expect_equal(issue$severity, "critical")
  expect_equal(issue$n_affected, 1)
})

test_that("missing user IDs are critical", {
  users <- valid_users()
  users$user_id[2] <- NA_character_
  validation <- validate_experiment(users)
  issue <- validation$issues[validation$issues$check_id == "missing_user_id", ]

  expect_false(validation$ok)
  expect_equal(issue$severity, "critical")
  expect_equal(issue$n_affected, 1)
})

test_that("invalid variants are critical", {
  users <- valid_users()
  users$variant[5] <- "champion"
  validation <- validate_experiment(users)
  issue <- validation$issues[validation$issues$check_id == "invalid_variant", ]

  expect_false(validation$ok)
  expect_equal(issue$severity, "critical")
  expect_equal(issue$n_affected, 1)
})

test_that("conversion and revenue disagreements are critical", {
  non_purchaser <- valid_users()
  non_purchaser$revenue[1] <- 10
  purchaser <- valid_users()
  purchaser$revenue[which(purchaser$converted == 1)[1]] <- 0

  non_purchaser_issue <- validate_experiment(non_purchaser)$issues
  purchaser_issue <- validate_experiment(purchaser)$issues
  non_purchaser_issue <- non_purchaser_issue[non_purchaser_issue$check_id == "inconsistent_conversion_revenue", ]
  purchaser_issue <- purchaser_issue[purchaser_issue$check_id == "inconsistent_conversion_revenue", ]

  expect_equal(non_purchaser_issue$severity, "critical")
  expect_equal(non_purchaser_issue$n_affected, 1)
  expect_equal(purchaser_issue$severity, "critical")
  expect_equal(purchaser_issue$n_affected, 1)
})

test_that("timestamps outside the experiment window are critical", {
  users <- valid_users()
  users$assignment_timestamp[5] <- lubridate::ymd_hms("2026-09-01 00:00:00", tz = "UTC")
  users$assignment_timestamp[6] <- lubridate::ymd_hms("2026-11-01 00:00:00", tz = "UTC")
  validation <- validate_experiment(users)
  issue <- validation$issues[validation$issues$check_id == "invalid_timestamp" & validation$issues$severity == "critical", ]

  expect_false(validation$ok)
  expect_equal(issue$n_affected, 2)
})

test_that("sample ratio mismatch matches a hand-calculated chi-squared test", {
  validation <- validate_experiment(valid_users(200, 50))

  expect_false(validation$ok)
  expect_equal(validation$srm$observed[["control"]], 200)
  expect_equal(validation$srm$observed[["treatment"]], 50)
  expect_equal(validation$srm$statistic, 90)
  expect_lt(validation$srm$p_value, 0.001)
  expect_equal(
    validation$issues$severity[validation$issues$check_id == "sample_ratio_mismatch"],
    "critical"
  )
})

test_that("missing covariates are warnings and do not fail the experiment", {
  users <- valid_users()
  users$country[1:3] <- NA_character_
  validation <- validate_experiment(users)

  expect_true(validation$ok)
  expect_true(any(validation$issues$check_id == "missing_covariate" & validation$issues$severity == "warning"))
})

test_that("character timestamps inside the window are not a critical failure", {
  users <- valid_users()
  users$assignment_timestamp <- format(users$assignment_timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  validation <- validate_experiment(users)

  expect_true(validation$ok)
  expect_true(any(
    validation$issues$check_id == "invalid_timestamp" &
      validation$issues$severity == "warning"
  ))
})
