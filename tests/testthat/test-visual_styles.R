test_that("color selection gives error for non-numeric", {
  expect_error(n_distinct_cols("a"), "provide a single number")
})

test_that("color selection gives error for length n>1", {
  expect_error(n_distinct_cols(c(1,2)), "provide a single number")
})

test_that("color selection works only when at least one color is requested", {
  expect_error(n_distinct_cols(0.99), "select at least one color")
  expect_error(n_distinct_cols(-1), "select at least one color")
})

test_that("color selection gives correct number of colors", {
  expect_equal(n_distinct_cols(1) %>% length(), 1)
  expect_equal(n_distinct_cols(4) %>% length(), 4)
  expect_equal(n_distinct_cols(14) %>% length(), 14)
  expect_equal(n_distinct_cols(24) %>% length(), 24)
  expect_equal(n_distinct_cols(51) %>% length(), 51)
  expect_equal(n_distinct_cols(1, c("green", "red", "blue")) %>% length(), 1)
  expect_equal(n_distinct_cols(51, c("green", "red", "blue")) %>% length(), 51)
})

test_that("color selection with custom colors fails when those are not character", {
  expect_error(n_distinct_cols(3, c(1,2,3)), "colors as character vector")
})
