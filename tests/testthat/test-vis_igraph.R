test_that("igraph basic visualization runs without error",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("igraph scales widths", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T, scale_width = 5,
                           radial_labs_opts = list(), export_type = "print"),
                 NA)
})


test_that("igraph visualizates with additional parameters",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = F,
                           export_type = "print",
                           vertex.color = c(rep("black", 18), rep("yellow", 18)),
                           vertex.label.cex = 1.2,
                           vertex.label.color = "darkgreen",
                           vertex.label = "ABC"),
                 NA)
})


test_that("igraph vis. leaves visual styling out when it is not an attribute",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    graph::edgeDataDefaults(self = network, attr = "color") <- NULL

    expect_error(visIgraph(network, radial_labs = T,
                           radial_labs_opts = list(cex = 0.8), export_type = "print"),
                 NA)

    graph::nodeDataDefaults(self = network, attr = "color") <- NULL

    expect_error(visIgraph(network, radial_labs = T,
                           radial_labs_opts = list(cex = 0.8),
                           export_type = "print"),
                 NA)
})


test_that("igraph vis. doesn't complain when overwriting args. for text()",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    # different location
    expect_error(visIgraph(network, radial_labs = T,
                           rad_lab_opts = list(x = 0, y = 0), export_type = "print"),
                 NA)
    # different rotation
    expect_error(visIgraph(network, radial_labs = T, export_type = "print",
                           rad_lab_opts = list(labels = "test", cex = 4, adj = 1, srt = 45)),
                 NA)
})


test_that("igraph vis. shows vertex sizes",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")
    # Nodes should have different sizes
    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("igraph vis. shows no vertex sizes when its not in node table",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    graph::nodeDataDefaults(self = network, attr = "size") <- NULL

    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("igraph vis. allows user to overwrite vertex sizes",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T,
                           vertex.size = rep(c(5,15), each = 18), export_type = "print"),
                 NA)
})


test_that("igraph plot saving works in multiple formats", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    withr::with_file(c("network.png", "network.pdf", "network.svg", "network.ps"), {
        visIgraph(network, radial_labs = T, export_type = "png")
        visIgraph(network, radial_labs = T, export_type = "svg")
        visIgraph(network, radial_labs = T, export_type = "pdf")
        visIgraph(network, radial_labs = T, export_type = "ps")

        expect_setequal(list.files(pattern = "network"),
                        c("network.png", "network.pdf", "network.svg", "network.ps"))
    })
})


test_that("igraph plot saving creates name series", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    withr::with_file(c("network.png", "network_2.png", "network_3.png"), {
        visIgraph(network, radial_labs = T, export_type = "png")
        visIgraph(network, radial_labs = T, export_type = "png")
        visIgraph(network, radial_labs = T, export_type = "png")

        expect_setequal(list.files(pattern = "network"),
                        c("network.png", "network_2.png", "network_3.png"))
    })
})


test_that("igraph plot saving works with custom names", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    withr::with_file("my_vis.png", {
        visIgraph(network, radial_labs = T, save_name = "my_vis", export_type = "png")

        expect_setequal(list.files(pattern = "my_vis"),
                        c("my_vis.png"))
    })
})


test_that("igraph plot print option doesn't save additional files", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    list_files0 <- list.files()

    visIgraph(network, radial_labs = T, export_type = "print")

    expect_setequal(list_files0,
                    list.files())
})


test_that("igraph vis. saves plots with changed margins",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "png", par_opts = list(mar=c(1,1,1,1))),
                 NA)
    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "png", par_opts = list(mar=c(6,6,6,6))),
                 NA)
})


test_that("igraph vis names list check works",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "print", par_opts = list(c(1,2))),
                 "must be named")
})


test_that("igraph vis names list check works",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "print", par_opts = list(c(1,2))),
                 "must be named")
})


test_that("igraph vis save_name input validation works",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "png", save_name = c("a", "b")),
                 "single save name")
    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "png", save_name = 6),
                 "must be of class character")
    expect_error(visIgraph(network, radial_labs = T,
                           export_type = "png", save_name = ""),
                 "Save name must be 1 or more characters")
})


test_that("igraph vis scale_width input validation works",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network,
                           export_type = "print", scale_width = c(8, 4)),
                 "provide a single number")
    expect_error(visIgraph(network,
                           export_type = "print", scale_width = "8"),
                 "must be of class numeric")
})


test_that("igraph visualizes directed graphs", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    dir_mat <- readRDS(testthat::test_path("fixtures", "directed_adj_matrix.rds"))

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(dir_mat,
                            directed = T,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network,
                           export_type = "print", directed = TRUE),
                 NA)
})


test_that("igraph visualizes directed graphs even when the matrix is symmetric", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            directed = T,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")


    expect_error(visIgraph(network,
                           export_type = "print", directed = TRUE),
                 NA)
})


test_that("visualizations in Rstudio stay present even after graphical device is started", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            directed = T,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            size_type = "igraph",
                            width_type = "partcor")

    expect_error(visIgraph(network,
                           export_type = "print", directed = FALSE),
                 NA)
    Sys.sleep(4)
    expect_error(visIgraph(network,
                           export_type = "png", directed = FALSE),
                 NA)
})


test_that("basic visualization works for graphNEL input",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("basic visualization works for igraph input",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    network <- igraph::graph_from_graphnel(network)

    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("basic visualization works for list of dataframes input",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network <- adjToNetwork(Mat1,
                            node_attrs = "all",
                            edge_attrs = "all",
                            group_vec = group_vec,
                            width_type = "partcor")

    network <- dfs_from_graphNEL(network)

    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("self loops work even when no vis attrs are added",{
    # When NULL is passed to edge width igraph will keep normal edges but
    #     discard self loops. Now the code prevents passing NULL, so the
    #     the problem should be fixed

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")


    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    net <- adjToNetwork(adj_mats, edge_attrs = "none", node_attrs = "none",
                        directed = T, self_loops = T, group_vec = group_vec) %>%
        dfs_from_graphNEL()

    expect_error(visIgraph(net, export_type = "print", edge.width = NULL,
                           scale_width = 1),
                 NA)
})

