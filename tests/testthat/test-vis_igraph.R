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


# test_that("igraph visualization runs without error",{
#
#   Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
#
#   group_vec <- rep("A", times = nrow(Mat1))
#   group_vec[colnames(Mat1) |> stringr::str_detect("IL")] <- "B"
#   group_vec[colnames(Mat1) |> stringr::str_detect("CCL")] <- "C"
#   group_vec[colnames(Mat1) |> stringr::str_detect("CXCL")] <- "D"
#
#   network_list <- adj_matrix_to_network(Mat1,
#                                         group_vec = group_vec,
#                                         width_type = 1)
#   edge_table <- network_list[["edge_table"]]
#   node_table <- network_list[["node_table"]]
#
#   expect_error(vis_igraph(edge_table, node_table, rot_labs = T,
#                           rot_labs_opts = list(cex = 1.2),
#                           vertex.color = c(rep("black", 18),
#                                            rep("yellow", 18))),
#                NA)
# })



