context("model")

test_that("lm", {
  ans <- run_lm(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"))
  expect_true(grepl("\"r.squared\": 0.7072", ans, fixed = TRUE))
})

test_that("quad", {
  ans <- run_quadratic(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"), c("Sepal.Width"))
  expect_true(grepl("\"estimate\": [4.754, -0.4641, 0.984, 0.1393],", ans, fixed = TRUE))
  expect_true(grepl("\"Sepal.Width2\"", ans, fixed = TRUE))
})

test_that("loess", {
  ans <- run_loess(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"))
  expect_true(grepl("\"enp\": 9.9762,", ans, fixed = TRUE))
})
