test_that("igraph visualization throws error when no network input is provided", {
  expect_error(vis_igraph(edge_table = NULL,
                          node_table = NULL,
                          igraph_obj = NULL), "provide either edge and node table or igraph object")
})

test_that("igraph visualization throws error when only edge_table is provided", {
  expect_error(vis_igraph(edge_table = T,
                          node_table = NULL,
                          igraph_obj = NULL), "node table parameter is NULL")
})

test_that("igraph visualization throws error when only node_table is provided", {
  expect_error(vis_igraph(edge_table = NULL,
                          node_table = T,
                          igraph_obj = NULL), "edge table parameter is NULL")
})

test_that("igraph_obj input must be of class igraph", {
  graph_obj <- matrix(1:9, 3, 3)
  class(graph_obj) <- "other_class"

  expect_error(vis_igraph(edge_table = NULL,
                          node_table = NULL,
                          igraph_obj = graph_obj), "must be of class igraph")
})




test_that("igraph basic visualization runs without error",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          radial_labs_opts = list(cex = 0.8)),
               NA)
})


test_that("igraph scales widths", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T, scale_width = 5,
                          radial_labs_opts = list()),
               NA)
})


test_that("igraph visualizates with additional parameters",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  expect_error(vis_igraph(edge_table, node_table, radial_labs = F,
                          vertex.color = c(rep("black", 18), rep("yellow", 18)),
                          vertex.label.cex = 1.2,
                          vertex.label.color = "darkgreen",
                          vertex.label = "ABC"
                          ),
               NA)
})


test_that("igraph vis. leaves visual styling out when it is not an attribute",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  edge_table <- edge_table %>% dplyr::select(!color)
  node_table <- network_list[["node_table"]]

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          radial_labs_opts = list(cex = 0.8)),
               NA)
  node_table <- node_table %>% dplyr::select(!color)
  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          radial_labs_opts = list(cex = 0.8)),
               NA)
})


test_that("igraph vis. doesn't complain when overwriting args. for text()",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          rad_lab_opts = list(x = 0, y = 0)),
               NA)
  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          rad_lab_opts = list(labels = "test", cex = 4, adj = 1, srt = 45)),
               NA)
})
