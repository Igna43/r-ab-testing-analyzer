test_that("centered covariate effects have mean zero", {
  probs <- c(mobile = 0.65, desktop = 0.35)
  centered <- center_effects(c(mobile = 0, desktop = 0.02), probs)
  expect_equal(sum(probs * centered), 0, tolerance = 1e-12)
})

test_that("the generator builds consistent users and a corrupted table that fails validation", {
  experiment <- generate_experiment_data(n_users = 4000, seed = 7, corrupt = TRUE)
  users <- experiment$users

  expect_equal(nrow(users), 4000)
  expect_true(all(users$variant %in% c("control", "treatment")))
  expect_true(all(users$revenue[users$converted == 0] == 0))
  expect_true(all(users$revenue[users$converted == 1] > 0))
  expect_equal(experiment$parameters$n_clipped, 0)
  expect_lt(abs(experiment$parameters$mean_control_probability - 0.10), 0.01)
  expect_lt(abs(experiment$parameters$mean_treatment_probability - 0.115), 0.01)
  expect_gt(
    mean(users$revenue[users$converted == 1]),
    stats::median(users$revenue[users$converted == 1])
  )

  clean <- validate_experiment(
    users,
    experiment_start = experiment$parameters$experiment_start,
    experiment_end = experiment$parameters$experiment_end
  )
  corrupted <- validate_experiment(
    experiment$corrupted,
    experiment_start = experiment$parameters$experiment_start,
    experiment_end = experiment$parameters$experiment_end
  )
  critical <- corrupted$issues$check_id[corrupted$issues$severity == "critical"]

  expect_true(clean$ok)
  expect_false(corrupted$ok)
  expect_true(all(c(
    "duplicate_user_id",
    "missing_user_id",
    "invalid_variant",
    "inconsistent_conversion_revenue",
    "invalid_timestamp",
    "sample_ratio_mismatch"
  ) %in% critical))
})

test_that("the committed synthetic dataset passes validation and the corrupted file does not", {
  root <- project_root()
  clean_path <- file.path(root, "data", "synthetic_experiment.csv")
  corrupted_path <- file.path(root, "data", "synthetic_experiment_corrupted.csv")
  parameter_path <- file.path(root, "data", "experiment_parameters.rds")
  expect_true(file.exists(clean_path))
  expect_true(file.exists(corrupted_path))
  expect_true(file.exists(parameter_path))

  users <- read_experiment_data(clean_path)
  parameters <- readRDS(parameter_path)
  validation <- validate_experiment(
    users,
    experiment_start = parameters$experiment_start,
    experiment_end = parameters$experiment_end
  )
  corrupted <- validate_experiment(
    read_experiment_data(corrupted_path),
    experiment_start = parameters$experiment_start,
    experiment_end = parameters$experiment_end
  )

  expect_equal(nrow(users), 20000)
  expect_equal(parameters$seed, 20241119)
  expect_true(validation$ok)
  expect_false(corrupted$ok)
  expect_true(all(users$revenue[users$converted == 0] == 0))
  expect_true(all(users$revenue[users$converted == 1] > 0))
})
