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


test_that("error when node names cannot be found", {
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

  node_table <- node_table %>% dplyr::select(-node)

  expect_error(vis_in_cytoscape(edge_table = edge_table, node_table = node_table, save_session = FALSE),
               "must contain column named 'node'")
})


test_that("error when edge source or target cannot be found", {
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


  edge_table00 <- edge_table %>% dplyr::select(-source)
  expect_error(vis_in_cytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
               "must contain columns 'source' and 'target'")
  edge_table00 <- edge_table %>% dplyr::select(-target)
  expect_error(vis_in_cytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
               "must contain columns 'source' and 'target'")
  edge_table00 <- edge_table %>% dplyr::select(-c(source, target))
  expect_error(vis_in_cytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
               "must contain columns 'source' and 'target'")
})




test_that("Cytoscape saves session", {
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

  # tf <- with_tempfile("tf", {write.csv(iris, tf); file.size(tf)})


  expect_error(vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                                save_session = TRUE), NA)
})


test_that("saving without name works a previous save is present")


# test_that("no error when node or edge attributes are missing", {
#   Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
#   group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#   network_list <- adj_matrix_to_network(Mat1,
#                                         node_attrs = "all",
#                                         edge_attrs = "all",
#                                         group_vec = group_vec,
#                                         width_type = "partcor",
#                                         size_type = "cytoscape")
#   edge_table <- network_list[["edge_table"]]
#   node_table <- network_list[["node_table"]]
#
#   node_table00 <- node_table %>% dplyr::select(-group)
#   edge_table00 <- edge_table %>% dplyr::select(-width)
#
#   # Test for presence of cytoscape
#   cytosc <- RCy3::cytoscapePing() %>% capture_condition()
#   skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
#               message = "this test runs only when cytoscape is active")
#
#   expect_error(vis_in_cytoscape(edge_table = edge_table, node_table = node_table00, save_session = FALSE),
#                NA)
#   # expect_error(vis_in_cytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
#   #              NA)
# })
