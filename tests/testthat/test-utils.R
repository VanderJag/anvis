test_that("named list test throws error when input is not list", {
  a <- matrix()
  b <- data.frame()
  c <- c(1:3, 4)

  expect_error(named_list_check(a), "must be a list")
  expect_error(named_list_check(b), "must be a list")
  expect_error(named_list_check(c), "must be a list")
})


test_that("checking for names of list elements works", {
  a <- list(a = 1, b = 2, 3)
  b <- list()

  expect_error(named_list_check(a), "must be named")
  expect_error(named_list_check(b), NA)
})
