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

test_that("group vector must be provided to add node groups", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  expect_error(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "group",
                                     edge_attrs = "none", group_vec = NULL),
               "Must provide grouping vector")
})

test_that("group column is added to node table when 'group' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "group",
                        edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node", "group"), ignore.order = TRUE)

})

test_that("group color column error when there is no group", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  expect_error(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "color",
                        edge_attrs = "none")[['node_table']],
               "Must provide node table with group info")

})

test_that("group color column is added to node table when 'group' and 'color' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = c("group", "color"),
                        edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node", "group", "color"), ignore.order = TRUE)

})

# TODO change this one to another attribute
test_that("group column is added to node table when 'group' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "group",
                        edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node", "group"), ignore.order = TRUE)

})

# one test to check if everyting is there when all is selected

