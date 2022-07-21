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
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T),
               NA)
})


test_that("igraph scales widths", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T, scale_width = 5,
                          radial_labs_opts = list()),
               NA)
})


test_that("igraph visualizates with additional parameters",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

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
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  edge_table <- edge_table %>% dplyr::select(!color)
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

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
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          rad_lab_opts = list(x = 0, y = 0)),
               NA)
  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          rad_lab_opts = list(labels = "test", cex = 4, adj = 1, srt = 45)),
               NA)
})


test_that("igraph vis. shows vertex sizes",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        size_type = "igraph",
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T),
               NA)
})


test_that("igraph vis. shows no vertex sizes when its not in node table",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        size_type = "igraph",
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  node_table <- node_table %>%
    dplyr::select(-size)

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T),
               NA)
})


test_that("igraph vis. allows user to overwrite vertex sizes",{
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        size_type = "igraph",
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(vis_igraph(edge_table, node_table, radial_labs = T,
                          vertex.size = rep(c(5,15), each = 18)),
               NA)
})


test_that("igraph plot saving works in multiple formats", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  node_table <- sort_avg_connectivity(node_table)

  withr::with_file(c("network.png", "network.pdf", "network.svg", "network.ps"), {
    vis_igraph(edge_table, node_table, radial_labs = T)
    vis_igraph(edge_table, node_table, radial_labs = T, out_format = "svg")
    vis_igraph(edge_table, node_table, radial_labs = T, out_format = "pdf")
    vis_igraph(edge_table, node_table, radial_labs = T, out_format = "ps")

    expect_setequal(list.files(pattern = "network"),
                 c("network.png", "network.pdf", "network.svg", "network.ps"))
  })
})


test_that("igraph plot saving creates name series", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  withr::with_file(c("network.png", "network_2.png", "network_3.png"), {
    vis_igraph(edge_table, node_table, radial_labs = T)
    vis_igraph(edge_table, node_table, radial_labs = T)
    vis_igraph(edge_table, node_table, radial_labs = T)

    expect_setequal(list.files(pattern = "network"),
                    c("network.png", "network_2.png", "network_3.png"))
  })
})


test_that("igraph plot saving works with custom names", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  withr::with_file("my_vis.png", {
    vis_igraph(edge_table, node_table, radial_labs = T, save_name = "my_vis")

    expect_setequal(list.files(pattern = "my_vis"),
                    c("my_vis.png"))
  })
})


test_that("igraph plot print option doesn't save additional files", {
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        node_attrs = "all",
                                        edge_attrs = "all",
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]

  list_files0 <- list.files()

  vis_igraph(edge_table, node_table, radial_labs = T, out_format = "print")

  expect_setequal(list_files0,
                  list.files())
})
