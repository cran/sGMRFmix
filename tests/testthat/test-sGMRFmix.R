context("main function")

test_that("main function", {
  set.seed(314)
  x <- generate_train_data()
  fit <- sGMRFmix(x, K = 7, rho = 100)

  expect_equal(fit$pi, c(0.51, 0.49), tolerance = 1e-5)
})

test_that("compute anomaly", {
  set.seed(314)
  x <- generate_train_data()
  fit <- sGMRFmix(x, K = 7, rho = 100)
  anomaly_score <- compute_anomaly_score(fit, generate_test_data())

  expect_equal(anomaly_score[1, 1], 1.022136, tolerance = 1e-5)
})
