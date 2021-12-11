library(tester)
library(testthat)

test_check("tester")

context("test decay rate project")

test_that("teat linear model", {
  expect_equal(k_guess, structure(0.91334264981829, .Names = "x"))
  expect_equal(A_guess, structure(1.88521591555031, .Names = "(Intercept)"))
})


test_that("test non-linear model", {
  expect_equal(ncol(d_errors_A), 6)
  expect_equal(ncol(d_errors_k), 6)
  expect_equal(ncol(n_nest), 7)
})


test_that("test model predictions", {
  expect_equal(ncol(preds), 2)
  expect_equal(nrow(preds), 1000)
})
