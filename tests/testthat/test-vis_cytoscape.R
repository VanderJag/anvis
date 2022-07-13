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

  withr::with_tempfile("temp_network", {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, save_name = "temp_network")})

})


test_that("Cytoscape exports imagewith name", {
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

  # perform save and check
  withr::with_file("temp_network.png", {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE,
                     save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), "temp_network.png")
  })
  # don't save and check
  withr::with_file("temp_network.png", {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = FALSE,
                     save_name = "temp_network");
    expect_equal(list.files(pattern = "temp_network"), character(0))
  })
})


test_that("cytoscape saves image when no name is provided", {
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

  # perform save and check
  withr::with_file("network.png", {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)

    expect_equal(list.files(pattern = "network"), "network.png")
  })
})


test_that("image saving without name works while a previous save is present", {
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

  # perform save and check
  withr::with_file(list("network.png", "network_2.png", "network_3.png"), {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)

    expect_equal(list.files(pattern = "network"), c("network.png",
                                                    "network_2.png",
                                                    "network_3.png"))
  })
})


test_that("image saving file name sequence works with non default names", {
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

  # perform save and check
  withr::with_file(list("temp_network.png", "temp_network_2.png", "temp_network_3.png"), {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), c("temp_network.png",
                                                    "temp_network_2.png",
                                                    "temp_network_3.png"))
  })
})


test_that("session saving without name works while a previous save is present", {
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

  # perform save and check
  withr::with_file(list("network.cys", "network_2.cys", "network_3.cys"), {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE)
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE)
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE)

    expect_equal(list.files(pattern = "network"), c("network.cys",
                                                    "network_2.cys",
                                                    "network_3.cys"))
  })
})


test_that("image saving file name sequence works with non default names", {
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

  # perform save and check
  withr::with_file(list("temp_network.cys", "temp_network_2.cys", "temp_network_3.cys"), {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), c("temp_network.cys",
                                                         "temp_network_2.cys",
                                                         "temp_network_3.cys"))
  })
})


test_that("image and session save number will be matching (default names)", {
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

  # Check if image adjusts to session names
  # withr::with_file(list("network.cys", "network_2.cys",
  #                       "network_3.cys", "network_3.png"), {
  #                         vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
  #                                          save_session = TRUE, export_image = FALSE)
  #                         vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
  #                                          save_session = TRUE, export_image = FALSE)
  #                         vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
  #                                          save_session = TRUE, export_image = TRUE)
  #
  #                         expect_true(all(c("network_3.png", "network_3.cys") %in% list.files()))
  #                       })

  # Check if session adjusts to image names
  withr::with_file(list("network.png", "network_2.png",
                        "network_3.png", "network_3.cys"), {
                          vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = FALSE, export_image = TRUE)
                          vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = FALSE, export_image = TRUE)
                          vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = TRUE, export_image = TRUE)

                          expect_true(all(c("network_3.png", "network_3.cys") %in% list.files()))
                        })
})


test_that("image and session save number will be matching (non default names)", {
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

  # Check if image adjusts to session names
  withr::with_file(list("temp_network.cys", "temp_network_2.cys",
                        "temp_network_3.cys", "temp_network_3.png"), {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = TRUE, save_name = "temp_network")

    expect_true(all(c("temp_network_3.png", "temp_network_3.cys") %in% list.files()))
  })

    # Check if session adjusts to image names
  withr::with_file(list("temp_network.png", "temp_network_2.png",
                        "temp_network_3.png", "temp_network_3.cys"), {
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    vis_in_cytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = TRUE, save_name = "temp_network")

    expect_true(all(c("temp_network_3.png", "temp_network_3.cys") %in% list.files()))
  })
})



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
