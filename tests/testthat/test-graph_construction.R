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


test_that("group column is added to node table when 'group' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "group",
                        edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node", "group"), ignore.order = TRUE)
})


test_that("size column is added to node table when 'size' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "size",
                        edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node", "size"), ignore.order = TRUE)
})


test_that("size, group, color is added to node table when 'all' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "all",
                                     edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node", "group", "color", "size"), ignore.order = TRUE)
})


test_that("no additional column is added to node table when 'none' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1, node_attrs = "none",
                                     edge_attrs = "none", group_vec = group_vec)[['node_table']],
               c("node"), ignore.order = TRUE)
})


test_that("no additional column is added to node table when node_attrs is blank", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1,
                                     edge_attrs = "none")[['node_table']],
               c("node"), ignore.order = TRUE)
})


test_that("width column is added to edge table when 'width' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1,
                                     edge_attrs = "width")[['edge_table']],
               c("source", "target", "weight", "width"), ignore.order = TRUE)
})


test_that("color column is added to edge table when 'color' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1,
                                     edge_attrs = "color")[['edge_table']],
               c("source", "target", "weight", "color"), ignore.order = TRUE)
})


test_that("color and width column is added to edge table when 'all' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1,
                                     edge_attrs = "all")[['edge_table']],
               c("source", "target", "weight", "width", "color"), ignore.order = TRUE)
})


test_that("no additional column is added to edge table when 'none' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1,
                                     edge_attrs = "none")[['edge_table']],
               c("source", "target", "weight"), ignore.order = TRUE)
})


test_that("no additional column is added to edge table when edge_attrs is left blank", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adj_matrix_to_network(adj_matrix = Mat1)[['edge_table']],
               c("source", "target", "weight"), ignore.order = TRUE)
})


test_that("creating adj matrix from edgelist creates original matrix",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    names_order <- colnames(Mat1)
    diag(Mat1) <- 0
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network_list <- adj_matrix_to_network(Mat1,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = "partcor")
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

    new_adj <- edgelist_to_adj(edge_table) %>% as.matrix()
    new_adj <- new_adj[names_order,names_order]

    expect_equal(new_adj, Mat1)
})


test_that("error is shown by edge to adj when to weight column is not found", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
    network_list <- adj_matrix_to_network(Mat1, width_type = "partcor",
                                          edge_attrs = "width")
    edge_table <- network_list[["edge_table"]]
    edge_table2 <- edge_table %>% dplyr::select(-weight)

    expect_error(edgelist_to_adj(edge_table2, weight_col = "weight"),
                 "Weight column name must be NULL or present in names")
    expect_error(edgelist_to_adj(edge_table2, weight_col = "width"),
                 NA)
    expect_error(edgelist_to_adj(edge_table2, weight_col = NULL),
                 NA)
})

# the two plots created by this function should be identical
test_that("other functions can make use of the adj. matrix created from edgelist", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    # data to make matrix
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    names_order <- colnames(Mat1)
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
    network_list <- adj_matrix_to_network(Mat1,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = "partcor")
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

    expect_error(vis_igraph(edge_table, node_table, radial_labs = T, export_type = "print"),
                 NA)

    # use matrix for visualization
    new_adj <- edgelist_to_adj(edge_table)
    new_adj <- new_adj[names_order,names_order]
    network_list <- adj_matrix_to_network(new_adj,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = "partcor")
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

    node_table <- sort_avg_connectivity(node_table)

    expect_error(vis_igraph(edge_table, node_table, radial_labs = T, export_type = "print"),
                 NA)
})
