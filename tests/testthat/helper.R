Sys.setenv(TZ = "UTC")

library(tidyverse)
library(broom)
library(infer)
library(scales)
library(testthat)

project_root <- function() {
  env_root <- Sys.getenv("AB_PROJECT_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root))
  }
  candidates <- unique(c(getwd(), dirname(getwd()), dirname(dirname(getwd()))))
  for (path in candidates) {
    if (file.exists(file.path(path, "R", "metrics.R"))) {
      return(normalizePath(path))
    }
  }
  stop("Could not locate the project root.", call. = FALSE)
}

source_project <- function() {
  root <- project_root()
  files <- sort(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE))
  invisible(lapply(files, source))
}

source_project()

valid_users <- function(n_control = 50, n_treatment = 50) {
  n <- n_control + n_treatment
  converted <- rep(0, n)
  converted[c(n - 3, n - 2, n - 1, n)] <- 1
  tibble::tibble(
    user_id = sprintf("u%03d", seq_len(n)),
    variant = c(rep("control", n_control), rep("treatment", n_treatment)),
    assignment_timestamp = lubridate::ymd_hms("2026-10-08 10:00:00", tz = "UTC"),
    device_type = rep(c("mobile", "desktop"), length.out = n),
    traffic_source = rep("organic", n),
    country = rep("US", n),
    converted = converted,
    revenue = ifelse(converted == 1, 25, 0),
    checkout_error = rep(0, n)
  )
}

hand_users <- function() {
  tibble::tibble(
    user_id = c("c1", "c2", "c3", "c4", "t1", "t2", "t3", "t4"),
    variant = c(rep("control", 4), rep("treatment", 4)),
    assignment_timestamp = lubridate::ymd_hms("2026-10-08 10:00:00", tz = "UTC"),
    device_type = c("mobile", "desktop", "mobile", "desktop", "mobile", "desktop", "mobile", "desktop"),
    traffic_source = rep("organic", 8),
    country = rep("US", 8),
    converted = c(0, 0, 1, 0, 1, 1, 0, 0),
    revenue = c(0, 0, 10, 0, 20, 30, 0, 0),
    checkout_error = c(0, 1, 0, 0, 0, 1, 0, 0)
  )
}
