test_that("creating edgelist works for matrix", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Check if no error occurs
  expect_error(adj_matrix_to_edgelist(Mat1), NA)
})


test_that("creating edgelist works for data frame", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  df <- as.data.frame(Mat1)

  # Check if no error occurs
  expect_error(adj_matrix_to_edgelist(df), NA)
})


test_that("creating edgelist from data frame or matrix gives same result", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  df <- as.data.frame(Mat1)

  # Check if no error occurs
  expect_equal(adj_matrix_to_edgelist(Mat1), adj_matrix_to_edgelist(df))
})


test_that("creating edgelist throws error when adj. matrix is not symmetrical", {
  # Adjacency matrix
  mat1 <- matrix(rep(1:3, each = 3), ncol =3, nrow = 3)
  row.names(mat1) <- colnames(mat1) <- c("A", "B", "C")

  # Check if error occurs
  expect_error(adj_matrix_to_edgelist(mat1), "identical upper and lower")
})


test_that("creating edgelist does not fail when adj. matrix is symmetrical", {
  # Adjacency matrix
  mat1 <- matrix(c(1, 2, 3,
                   2, 2, 3,
                   3, 3, 3), ncol =3, nrow = 3)
  row.names(mat1) <- colnames(mat1) <- c("A", "B", "C")


  # Check if no error occurs
  expect_error(adj_matrix_to_edgelist(mat1), NA)
})
