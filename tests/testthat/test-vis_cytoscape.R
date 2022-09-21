test_that("basic visualization works",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, close_session = F,
                              export_image = F),
                 NA)
})


test_that("error when node names cannot be found", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    network <- dfs_from_graphNEL(network)

    network$vertices <- network$vertices %>% dplyr::select(-node)

    expect_error(visCytoscape(network = network, save_session = FALSE),
                 "must contain column named 'node'")
})


test_that("error when edge source or target cannot be found", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    network2 <- dfs_from_graphNEL(network)
    network3 <- dfs_from_graphNEL(network)
    network4 <- dfs_from_graphNEL(network)

    network2$edges <- network2$edges %>% dplyr::select(-source)
    expect_error(visCytoscape(network = network2, save_session = FALSE),
                 "must contain columns 'source' and 'target'")
    network3$edges <- network3$edges %>% dplyr::select(-target)
    expect_error(visCytoscape(network = network3, save_session = FALSE),
                 "must contain columns 'source' and 'target'")
    network4$edges <- network4$edges %>% dplyr::select(-c(source, target))
    expect_error(visCytoscape(network = network4, save_session = FALSE),
                 "must contain columns 'source' and 'target'")
})


test_that("Cytoscape saves session", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    withr::with_file("temp_network.cys", {
        visCytoscape(network = network,
                     export_image = FALSE,
                     save_session = TRUE, save_name = "temp_network")

        expect_equal(list.files(pattern = "temp_network"), "temp_network.cys")
    })
})


test_that("Cytoscape exports image with name", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # perform save and check
    withr::with_file("temp_network.png", {
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE,
                     save_name = "temp_network")

        expect_equal(list.files(pattern = "temp_network"), "temp_network.png")
    })
    # don't save and check
    withr::with_file("temp_network.png", {
        visCytoscape(network = network,
                     save_session = FALSE, export_image = FALSE,
                     save_name = "temp_network");
        expect_equal(list.files(pattern = "temp_network"), character(0))
    })
})


test_that("cytoscape saves image when no name is provided", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # perform save and check
    withr::with_file("network.png", {
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE)

        expect_equal(list.files(pattern = "network"), "network.png")
    })
})


test_that("image saving without name works while a previous save is present", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # perform save and check
    withr::with_file(list("network.png", "network_2.png", "network_3.png"), {
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE)
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE)
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE)

        expect_equal(list.files(pattern = "network"), c("network.png",
                                                        "network_2.png",
                                                        "network_3.png"))
    })
})


test_that("image saving file name sequence works with non default names", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # perform save and check
    withr::with_file(list("temp_network.png", "temp_network_2.png", "temp_network_3.png"), {
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")
        visCytoscape(network = network,
                     save_session = FALSE, export_image = TRUE, save_name = "temp_network")

        expect_equal(list.files(pattern = "temp_network"), c("temp_network.png",
                                                             "temp_network_2.png",
                                                             "temp_network_3.png"))
    })
})


test_that("session saving without name works while a previous save is present", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # perform save and check
    withr::with_file(list("network.cys", "network_2.cys", "network_3.cys"), {
        visCytoscape(network = network,
                     save_session = TRUE, export_image = FALSE)
        visCytoscape(network = network,
                     save_session = TRUE, export_image = FALSE)
        visCytoscape(network = network,
                     save_session = TRUE, export_image = FALSE)

        expect_equal(list.files(pattern = "network"), c("network.cys",
                                                        "network_2.cys",
                                                        "network_3.cys"))
    })
})


test_that("image saving file name sequence works with non default names", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # perform save and check
    withr::with_file(list("temp_network.cys", "temp_network_2.cys",
                          "temp_network_3.cys"), {
        visCytoscape(network = network,
                     save_session = TRUE, export_image = FALSE,
                     save_name = "temp_network")
        visCytoscape(network = network,
                     save_session = TRUE, export_image = FALSE,
                     save_name = "temp_network")
        visCytoscape(network = network,
                     save_session = TRUE, export_image = FALSE,
                     save_name = "temp_network")

        expect_equal(list.files(pattern = "temp_network"),
                     c("temp_network.cys", "temp_network_2.cys",
                       "temp_network_3.cys"))
    })
})


test_that("image and session save number will be matching (default names)", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # Check if image adjusts to session names
    withr::with_file(list("network.cys", "network_2.cys",
                          "network_3.cys", "network_3.png"), {
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = FALSE)
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = FALSE)
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = TRUE)

                              expect_true(all(c("network_3.png",
                                                "network_3.cys") %in% list.files()))
                          })

    # Check if session adjusts to image names
    withr::with_file(list("network.png", "network_2.png",
                          "network_3.png", "network_3.cys"), {
                              visCytoscape(network = network,
                                           save_session = FALSE,
                                           export_image = TRUE)
                              visCytoscape(network = network,
                                           save_session = FALSE,
                                           export_image = TRUE)
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = TRUE)

                              expect_true(all(c("network_3.png",
                                                "network_3.cys") %in% list.files()))
                          })
})


test_that("image and session save number will be matching (non default names)", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    # Check if image adjusts to session names
    withr::with_file(list("temp_network.cys", "temp_network_2.cys",
                          "temp_network_3.cys", "temp_network_3.png"), {
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = FALSE,
                                           save_name = "temp_network")
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = FALSE,
                                           save_name = "temp_network")
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = TRUE,
                                           save_name = "temp_network")

                              expect_true(all(c("temp_network_3.png",
                                                "temp_network_3.cys") %in%

                                                  list.files()))
                          })

    # Check if session adjusts to image names
    withr::with_file(list("temp_network.png", "temp_network_2.png",
                          "temp_network_3.png", "temp_network_3.cys"), {
                              visCytoscape(network = network,
                                           save_session = FALSE,
                                           export_image = TRUE,
                                           save_name = "temp_network")
                              visCytoscape(network = network,
                                           save_session = FALSE,
                                           export_image = TRUE,
                                           save_name = "temp_network")
                              visCytoscape(network = network,
                                           save_session = TRUE,
                                           export_image = TRUE,
                                           save_name = "temp_network")

                              expect_true(all(c("temp_network_3.png",
                                                "temp_network_3.cys") %in%
                                                  list.files()))
                          })
})



test_that("no error when node or edge attributes are missing", {
    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    network2 <- network
    graph::nodeDataDefaults(network, attr = "color") <- NULL
    graph::edgeDataDefaults(network2, attr = "color") <- NULL
    graph::edgeDataDefaults(network2, attr = "weight") <- NULL
    graph::edgeDataDefaults(network2, attr = "width") <- NULL

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, export_image = FALSE),
                 NA)
    expect_error(visCytoscape(network = network2,
                              save_session = FALSE, export_image = FALSE),
                 NA)
})


test_that("radial labels work when nodes are arranged by connectivity",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    network <- dfs_from_graphNEL(network)

    network$vertices <- sort_avg_connectivity(network$vertices)[[1]]

    expect_error(visCytoscape(network = network,
                              save_session = FALSE),
                 NA)
})


test_that("variation of edge width works",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, export_image = F, close_session = F,
                              scale_width = 8),
                 NA)
})


test_that("visualization of directed networks works",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    dir_mat <- readRDS(testthat::test_path("fixtures", "directed_adj_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(dir_mat,
                            node_attrs = "all",
                            edge_attrs = "all",
                            directed = T,
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, scale_width = 3, close_session = F,
                              export_image = F, directed = T),
                 NA)
})


test_that("visualization of directed networks works even for undirected networks",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            directed = T,
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, close_session = F,
                              export_image = F, scale_width = 3, directed = T),
                 NA)
})


test_that("visualization works with igraph input",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    network <- igraph::graph_from_graphnel(network)

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, close_session = F,
                              export_image = F),
                 NA)
})


test_that("visualization works with list data frames input",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    network <- network %>% dfs_from_graphNEL()

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, close_session = F,
                              export_image = F),
                 NA)
})


test_that("visualization works with graphNEL input",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, close_session = F,
                              export_image = F),
                 NA)
})


test_that("visualization of self loops works for directed network",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    dir_mat <- readRDS(testthat::test_path("fixtures", "directed_adj_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(dir_mat,
                            node_attrs = "all",
                            edge_attrs = "all",
                            directed = T,
                            self_loops = T,
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape")

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, scale_width = 3, close_session = F,
                              export_image = F, directed = T),
                 NA)
})


test_that("visualization of self loops works for undirected network",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Test for presence of cytoscape
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    dir_mat <- readRDS(testthat::test_path("fixtures", "directed_adj_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(dir_mat,
                            node_attrs = "all",
                            edge_attrs = "all",
                            directed = F,
                            self_loops = T,
                            group_vec = group_vec,
                            width_type = "partcor",
                            size_type = "cytoscape") %>% suppressWarnings()

    expect_error(visCytoscape(network = network,
                              save_session = FALSE, scale_width = 3, close_session = F,
                              export_image = F, directed = T),
                 NA)
})
