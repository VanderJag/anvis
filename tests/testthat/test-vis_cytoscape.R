test_that("basic visualization works",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")

  # Test for presence of cytoscape
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  expect_error(visCytoscape(edge_table = edge_table, node_table = node_table,
                                save_session = FALSE),
               NA)
})


test_that("error when node names cannot be found", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  node_table <- node_table %>% dplyr::select(-node)

  expect_error(visCytoscape(edge_table = edge_table, node_table = node_table, save_session = FALSE),
               "must contain column named 'node'")
})


test_that("error when edge source or target cannot be found", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]


  edge_table00 <- edge_table %>% dplyr::select(-source)
  expect_error(visCytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
               "must contain columns 'source' and 'target'")
  edge_table00 <- edge_table %>% dplyr::select(-target)
  expect_error(visCytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
               "must contain columns 'source' and 'target'")
  edge_table00 <- edge_table %>% dplyr::select(-c(source, target))
  expect_error(visCytoscape(edge_table = edge_table00, node_table = node_table, save_session = FALSE),
               "must contain columns 'source' and 'target'")
})


test_that("Cytoscape saves session", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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

  withr::with_file("temp_network.cys", {
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     export_image = FALSE,
                     save_session = TRUE, save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), "temp_network.cys")
    })

})


test_that("Cytoscape exports image with name", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE,
                     save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), "temp_network.png")
  })
  # don't save and check
  withr::with_file("temp_network.png", {
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = FALSE,
                     save_name = "temp_network");
    expect_equal(list.files(pattern = "temp_network"), character(0))
  })
})


test_that("cytoscape saves image when no name is provided", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)

    expect_equal(list.files(pattern = "network"), "network.png")
  })
})


test_that("image saving without name works while a previous save is present", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE)

    expect_equal(list.files(pattern = "network"), c("network.png",
                                                    "network_2.png",
                                                    "network_3.png"))
  })
})


test_that("image saving file name sequence works with non default names", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), c("temp_network.png",
                                                    "temp_network_2.png",
                                                    "temp_network_3.png"))
  })
})


test_that("session saving without name works while a previous save is present", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE)
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE)
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE)

    expect_equal(list.files(pattern = "network"), c("network.cys",
                                                    "network_2.cys",
                                                    "network_3.cys"))
  })
})


test_that("image saving file name sequence works with non default names", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")

    expect_equal(list.files(pattern = "temp_network"), c("temp_network.cys",
                                                         "temp_network_2.cys",
                                                         "temp_network_3.cys"))
  })
})


test_that("image and session save number will be matching (default names)", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
  withr::with_file(list("network.cys", "network_2.cys",
                        "network_3.cys", "network_3.png"), {
                          visCytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = TRUE, export_image = FALSE)
                          visCytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = TRUE, export_image = FALSE)
                          visCytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = TRUE, export_image = TRUE)

                          expect_true(all(c("network_3.png", "network_3.cys") %in% list.files()))
                        })

  # Check if session adjusts to image names
  withr::with_file(list("network.png", "network_2.png",
                        "network_3.png", "network_3.cys"), {
                          visCytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = FALSE, export_image = TRUE)
                          visCytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = FALSE, export_image = TRUE)
                          visCytoscape(edge_table = edge_table, node_table = node_table,
                                           save_session = TRUE, export_image = TRUE)

                          expect_true(all(c("network_3.png", "network_3.cys") %in% list.files()))
                        })
})


test_that("image and session save number will be matching (non default names)", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
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
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = FALSE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = TRUE, save_name = "temp_network")

    expect_true(all(c("temp_network_3.png", "temp_network_3.cys") %in% list.files()))
  })

    # Check if session adjusts to image names
  withr::with_file(list("temp_network.png", "temp_network_2.png",
                        "temp_network_3.png", "temp_network_3.cys"), {
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
    visCytoscape(edge_table = edge_table, node_table = node_table,
                     save_session = TRUE, export_image = TRUE, save_name = "temp_network")

    expect_true(all(c("temp_network_3.png", "temp_network_3.cys") %in% list.files()))
  })
})



test_that("no error when node or edge attributes are missing", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  node_table00 <- node_table %>% dplyr::select(-color)
  edge_table00 <- edge_table %>% dplyr::select(-c("width", "weight", "color"))

  # Test for presence of cytoscape
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  expect_error(visCytoscape(edge_table = edge_table, node_table = node_table00,
                                save_session = FALSE, export_image = FALSE),
               NA)
  expect_error(visCytoscape(edge_table = edge_table00, node_table = node_table,
                                save_session = FALSE, export_image = FALSE),
               NA)
})


test_that("radial labels work when nodes are arranged by connectivity",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]
  node_table <- sort_avg_connectivity(node_table)[[1]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")

  # Test for presence of cytoscape
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  expect_error(visCytoscape(edge_table = edge_table, node_table = node_table,
                                save_session = FALSE),
               NA)
})


test_that("variation of edge width works",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adjToNetwork(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor",
                                        size_type = "cytoscape")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")

  # Test for presence of cytoscape
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  expect_error(visCytoscape(edge_table = edge_table, node_table = node_table,
                                save_session = FALSE, export_image = F, close_session = F,
                                scale_width = 8),
               NA)
})


test_that("visualization of directed networks works",{
    dir_mat <- readRDS(testthat::test_path("fixtures", "directed_adj_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network_list <- adjToNetwork(dir_mat,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          directed = T,
                                          group_vec = group_vec,
                                          width_type = "partcor",
                                          size_type = "cytoscape")
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    expect_error(visCytoscape(edge_table = edge_table, node_table = node_table,
                                  save_session = FALSE, scale_width = 3, close_session = F,
                                  export_image = F, directed = T),
                 NA)
})


test_that("visualization of directed networks works even for undirected networks",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network_list <- adjToNetwork(Mat1,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          directed = T,
                                          group_vec = group_vec,
                                          width_type = "partcor",
                                          size_type = "cytoscape")
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    expect_error(visCytoscape(edge_table = edge_table, node_table = node_table,
                                  save_session = FALSE, close_session = F,
                                  export_image = F, scale_width = 3, directed = T),
                 NA)
})
