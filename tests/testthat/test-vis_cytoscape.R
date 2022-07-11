test_that("basic visualization works",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  # Test for presence of cytoscape
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  expect_error(vis_in_cytoscape(edge_table = edge_table, node_table = node_table, save_session = FALSE),
               NA)
})
