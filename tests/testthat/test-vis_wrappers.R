test_that("when cytoscape is not available RCy3 will give error", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  # Presence of cytoscape test, not currently working because
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if(cytosc$message == "You are connected to Cytoscape!\n",
          message = "this test runs only when cytoscape is inactive")

  # Visualize the network
  expect_message(VisualiseNetwork(Mat1, group_vec = group_vec, output_type = "cyto"),
                 "Please check that Cytoscape is running") %>%
  expect_error("object 'res' not found|Failed to connect to") %>%
  expect_error("object 'res' not found|argument is of length zero")
})


test_that("when cytoscape is available wrapper runs without error", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  # Presence of cytoscape test
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()

  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  # Visualize the network
  expect_error(VisualiseNetwork(Mat1, group_vec = group_vec, output_type = "cyto",
                                node_attrs = "all", edge_attrs = "all", arrange_co = T), NA)
})


test_that("group vec list and adj mat list are checked for equal size", {
  mat0 <- matrix(rep(1, 4), ncol = 2)
  mat_list <- list(mat1 = mat0, mat2 = mat0, mat3 = mat0)
  groupings <- c("a", "b")
  group_vec <- list(group1 = groupings, group2 = groupings, group3 = groupings)

  group_vec <- group_vec[-2]
  expect_error(VisualiseNetwork(adj_mats = mat_list, group_vec = group_vec),
               "must be of equal length")
})


test_that("Cytoscape visualizations are made for each network in the list", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  # Presence of cytoscape test
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()

  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  withr::with_file(c("network.png", paste0("network_", 2:3, ".png")), {
    VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "cyto",
                     edge_attrs = "all", node_attrs = "all", arrange_co = T,
                     width_type = "partcor", cyto_save_session = F,
                     cyto_close_session = T, vis_save = T)

    expect_setequal(list.files(pattern = "network"),
                    c("network.png", paste0("network_", 2:3, ".png")))
  })
})


test_that("Igraph visualizations are made for each network in the list", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = "partcor"),
               NA)
})


test_that("grouping vector of length 1 or same as data works", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  networks <- VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "return_only",
                               edge_attrs = "all", node_attrs = "all")

  all_equal <- all(vapply(seq_along(networks$nodes),
         function (x) all(networks$nodes[[1]]$group == networks$nodes[[x]]$group),
         FUN.VALUE = T))

  expect_true(all_equal)

  grouping_list <- replicate(length(adj_mats), sample(group_vec), simplify = FALSE)

  networks <- VisualiseNetwork(adj_mats, group_vec = grouping_list, output_type = "return_only",
                               edge_attrs = "all", node_attrs = "all")

  expect_equal(lapply(seq_along(networks$nodes),
                      function (x) {
                        # resort to original order
                        tmp_idx <- networks$nodes[[x]]$node %>%
                          {match(colnames(adj_mats[[x]]), .)}
                        # Get original order group names
                        networks$nodes[[x]][tmp_idx,]$group}),
                      grouping_list)

})


test_that("grouping vector as list of length 1 works", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "return_only",
                               edge_attrs = "all", node_attrs = "all", vis_save = F), NA)
})


test_that("user supplied colors are used", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  colors0 <- c("midnightblue", "black", "red", "#b99055")

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = "partcor", vis_save = F, group_colors = colors0),
               NA)
  colors0 <- c("midnightblue", "red", "#b99055")

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = "partcor", vis_save = F, group_colors = colors0),
               NA)
})


test_that("edge widths can be scaled", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")
  # Check if cytoscape is active
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")


  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = "partcor", vis_save = F, edge_factor = 12),
               NA)
  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "cytoscape",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = "partcor", vis_save = F, edge_factor = 6),
               NA)
})


test_that("cytoscape nodespace works", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")
  # Check if cytoscape is active
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")


  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "cytoscape",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = "partcor", vis_save = F, cyto_node_space = 4),
               NA)
})


test_that("error is thrown when length of width type doesn't match", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = c("partcor", "MI"), vis_save = F, ),
               "width type must be 1 or matching with")
})


test_that("width types can be provided as vector", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                                edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                width_type = c("partcor", "partcor", "default"), vis_save = F),
               NA)
})


test_that("igraph can arrange visualizations in grid", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(
      VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor", vis_save = T, igr_grid = T), NA)
  # expect_error({
  #   for (i in seq_along(adj_mats)) {
  #     VisualiseNetwork(adj_mats[1:i], group_vec = group_vec, output_type = "igraph",
  #                      edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
  #                      width_type = "partcor", vis_save = T, igr_grid = T)}
  # }, NA)
})


test_that("igraph grid can be specified manually", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(
      VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor", vis_save = T, igr_grid = c(2,6),
                       vis_export_opts = list(width = 8200, height = 2600)), NA)
})


test_that("errors occur for incorrect grid igr_grid input", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_error(
      VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor", vis_save = T, igr_grid = c(2,6,3),
                       vis_export_opts = list(width = 7000, height = 2600)),
      "TRUE, FALSE, or a vector of two integers")

  expect_error(
    VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                     edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                     width_type = "partcor", vis_save = T, igr_grid = NULL,
                     vis_export_opts = list(width = 7000, height = 2600)),
    "TRUE, FALSE, or a vector of two integers")

  expect_error(
    VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                     edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                     width_type = "partcor", vis_save = T, igr_grid = c(2,2),
                     vis_export_opts = list(width = 7000, height = 2600)),
    "grid dimensions must exceed or equal number of networks")
})


test_that("igraph grid node label space can be adjusted with margins", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(
    VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                     edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                     width_type = "partcor", vis_save = T, igr_grid = c(2,6),
                     vis_export_opts = list(width = 6400, height = 2400),
                     igr_par_opts = list(mar=c(6,6,6,6))), NA)
})


test_that("xgmml output works for a single network", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file(c("network.xgmml"), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    VisualiseNetwork(adj_mats = Mat1, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network", netw_ext = "XGMML")

    expect_setequal(list.files(pattern = "network"),
                    c("network.xgmml"))
  })

  # VisualiseNetwork(adj_mats = Mat1, node_attrs = "all", edge_attrs = "all",
  #                  group_vec = group_vec, output_type = "network", netw_ext = "XGMML")
})


test_that("xgmml output uses custom save names", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file(c("my_xgmml.xgmml"), {
    VisualiseNetwork(adj_mats = Mat1, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network", netw_ext = "XGMML",
                     save_names = "my_xgmml")

    expect_setequal(list.files(pattern = "my_xgmml"),
                    c("my_xgmml.xgmml"))
  })
})


test_that("xgmml output saves multiple networks using save name numbering", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:4]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network",
                     netw_ext = "XGMML")

    expect_setequal(list.files(pattern = "network"),
                    c("network.xgmml", paste0("network_", 2:4, ".xgmml")))
  })
})


test_that("xgmml output writes xgmml title", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:4]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network",
                     netw_ext = "XGMML")

    title_tags <- sapply(list.files(pattern = "network"), function (x) x %>%
      readLines() %>%
        stringr::str_trim() %>%
        stringr::str_subset("^<dc:title>") %>%
        stringr::str_sub(start = 11, end = -12)
      ) %>%
      unname()

    expect_setequal(title_tags,
                    c("network", paste0("network_", 2:4)))
  })
})


test_that("xgmml output can use vector of xgmml titles", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:4]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network",
                     netw_ext = "XGMML", netw_xgmml_title = c("one", "two",
                                                              "three", "four"))

    title_tags <- sapply(list.files(pattern = "network"), function (x) x %>%
                           readLines() %>%
                           stringr::str_trim() %>%
                           stringr::str_subset("^<dc:title>") %>%
                           stringr::str_sub(start = 11, end = -12)
    ) %>%
      unname()

    expect_setequal(title_tags,
                    c("one", "two", "three", "four"))
  })
})


test_that("xgmml output throws error for incorrect length xgmml titles", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:4]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    expect_error(VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all",
                                  edge_attrs = "all",
                     group_vec = group_vec, output_type = "network",
                     netw_ext = "XGMML", netw_xgmml_title = c("one", "two",
                                                              "three")),
                 "Length of xgmml titles must be 1 or matching with")

  })
})


test_that("xgmml output has a matching number of nodes and edges compared to original data", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file("network.xgmml", {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    output <- VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all",
                               edge_attrs = "all",
                               group_vec = group_vec, output_type = "return",
                               netw_ext = "XGMML",
                               netw_xgmml_title = c("one", "two", "three", "four"))
    nr_edges <- output$edges[[1]] %>% nrow()
    nr_nodes <- output$nodes[[1]] %>% nrow()

    VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network",
                     netw_ext = "XGMML")

    xml_nodes <- readLines("network.xgmml") %>%
      stringr::str_trim() %>%
      stringr::str_subset("^<node label") %>%
      length()
    xml_edges <- readLines("network.xgmml") %>%
      stringr::str_trim() %>%
      stringr::str_subset("^<edge label") %>%
      length()


    expect_equal(nr_edges, xml_edges)
    expect_equal(nr_nodes, xml_nodes)
  })
})


test_that("xgmml output contains the same attributes as the input data", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))


  withr::with_file("network.xgmml", {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    output <- VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all",
                               edge_attrs = "all",
                               group_vec = group_vec, output_type = "return",
                               netw_ext = "XGMML",
                               netw_xgmml_title = c("one", "two", "three", "four"))
    edge_attrs <- output$edges[[1]] %>% names()
    edge_attrs <- edge_attrs[!(edge_attrs %in% c("source", "target"))]
    node_attrs <- output$nodes[[1]] %>% names()
    node_attrs[(node_attrs %in% c("node"))] <- "name"

    VisualiseNetwork(adj_mats = adj_mats, node_attrs = "all", edge_attrs = "all",
                     group_vec = group_vec, output_type = "network",
                     netw_ext = "XGMML")

    xgmml_l <- readLines("network.xgmml")
    start_node1 <- which(stringr::str_trim(xgmml_l) %>%
                           stringr::str_starts("<node"))[1]
    end_node1 <- which(stringr::str_trim(xgmml_l) %>%
                           stringr::str_starts("</node>"))[1]
    attrs_node1 <- xgmml_l[(start_node1+1):(end_node1-1)] %>%
      stringr::str_extract_all('name=.*\" value') %>%
      unlist() %>%
      stringr::str_sub(start = 7, end = -8)

    start_edge1 <- which(stringr::str_trim(xgmml_l) %>%
                           stringr::str_starts("<edge"))[1]
    end_edge1 <- which(stringr::str_trim(xgmml_l) %>%
                           stringr::str_starts("</edge>"))[1]
    attrs_edge1 <- xgmml_l[(start_edge1+1):(end_edge1-1)] %>%
      stringr::str_extract_all('name=.*\" value') %>%
      unlist() %>%
      stringr::str_sub(start = 7, end = -8)

    expect_equal(edge_attrs, attrs_edge1)
    expect_equal(node_attrs, attrs_node1)
  })
})


# Run below test to see example -------------------------------------------
test_that("igraph grid visualization allows adding titles to plots", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  expect_error(
    VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                     edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                     width_type = "partcor", vis_save = T, igr_grid = c(2,6),
                     vis_export_opts = list(width = 6400, height = 2600),
                     igr_par_opts = list(mar=c(2,4,5,4)),
                     igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
  # expect_error(
  #   VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
  #                    edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
  #                    width_type = "partcor", vis_save = T, igr_grid = c(2,6),
  #                    vis_export_opts = list(width = 18, height = 7.5),
  #                    igr_par_opts = list(mar=c(2,4,5,4)), vis_export_type = "svg",
  #                    igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
})


test_that("igraph grid titles need correct length else error", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_error(
    VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                     edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                     width_type = "partcor", vis_save = T, igr_grid = c(2,6),
                     vis_export_opts = list(width = 6400, height = 2600),
                     igr_par_opts = list(mar=c(2,4,5,4)),
                     igr_grid_names = paste("patient", LETTERS[1:length(adj_mats)-1])),
    "Grid names must be TRUE, FALSE, or of length matching")
})


test_that("igraph grid titles cause warning when requested but adj list unnamed", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_warning(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F, igr_grid = c(1,3),
                         igr_par_opts = list(mar=c(2,4,5,4)),
                         igr_grid_names = T),
        "`igr_grid_names` is TRUE but no names were found")
})


test_that("igraph grid titles can be drawn from names of adj_mats list", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    names(adj_mats) <- paste("person", LETTERS[1:3])
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F, igr_grid = c(1,3),
                         igr_par_opts = list(mar=c(2,4,5,4)),
                         igr_grid_names = T),
        NA)
})


test_that("validation of list arguments works", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    names(adj_mats) <- paste("person", LETTERS[1:3])
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F, igr_grid = c(1,3),
                         igr_par_opts = list(mar=c(2,4,5,4)), igr_plot_opts = c("a"),
                         igr_grid_names = T),
        "must be a list")
})


# test_that("large network can be visualized with igraph", {
#     test_call <- deparse(sys.calls()[[1]][1])
#     skip_if_not(test_call == "test_that()",
#                 message = "igraph visualizations need to be checked manually")
#
#     large_adj <- readRDS(test_path("fixtures", "large_network.RDS"))
#     # group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#     expect_error(
#         VisualiseNetwork(large_adj, output_type = "igraph",
#                          edge_attrs = "all", node_attrs = "size", arrange_co = TRUE,
#                          width_type = "partcor", vis_save = T, vis_export_type = "svg",
#                          vis_export_opts = list(width = 30, height = 30),
#                          igr_par_opts = list(mar=c(5,5,5,5))),
#         NA)
# })
#
#
# test_that("large network can be visualized with cytoscape", {
#     test_call <- deparse(sys.calls()[[1]][1])
#     skip_if_not(test_call == "test_that()",
#                 message = "igraph visualizations need to be checked manually")
#
#     # Check if cytoscape is active
#     cytosc <- RCy3::cytoscapePing() %>% capture_condition()
#     skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
#                 message = "this test runs only when cytoscape is active")
#
#     large_adj <- readRDS(test_path("fixtures", "large_network.RDS"))
#     # group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#     expect_error(
#         VisualiseNetwork(large_adj, output_type = "cytoscape",
#                          edge_attrs = "all", node_attrs = "size", arrange_co = TRUE,
#                          width_type = "partcor", vis_save = T),
#         NA)
# })

test_that("visualization with only positive values is informative", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    adj_mats <- lapply(adj_mats, abs)

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = T, igr_grid = c(2,6),
                         vis_export_opts = list(width = 6400, height = 2600),
                         igr_par_opts = list(mar=c(2,4,5,4)),
                         igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
})


test_that("colorblind accessible colors can be used", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    netw <- VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "return_only",
                             edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                             width_type = "partcor", vis_save = F, igr_grid = c(1,2),
                             igr_par_opts = list(mar=c(2,4,5,4)), colorblind = T)

    colours <- netw$nodes[[1]]$color %>% unique()

    expect_true(all(colours %in% palette.colors(palette = "Okabe-Ito")))
})


test_that("warning occurs is colorblind colors are overwritten by manually selected colors", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_warning(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "return_only",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F, igr_grid = c(1,2),
                         igr_par_opts = list(mar=c(2,4,5,4)), colorblind = T,
                         group_colors = c("red", "green", "blue", "yellow")),
        "instead of colorblind accessible colors")
})


test_that("custom edge color function can be used for pos. + neg. data", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:6]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F, igr_grid = c(2,3),
                         igr_par_opts = list(mar=c(2,4,5,4)),
                         # reversing the below color would make more sense
                         edge_color_func = pals::brewer.piyg),
        NA)
})


test_that("custom edge color function can be used for positive only data", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:6]
    adj_mats <- lapply(adj_mats, abs)
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    my_cols <- function (n) rev(pals::magma(n))
    my_cols <- function (n) rev(pals::kovesi.linear_grey_10_95_c0(n))

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F, igr_grid = c(2,3),
                         igr_par_opts = list(mar=c(2,4,5,4)),
                         edge_color_func = my_cols),
        NA)
})


test_that("igraph visualizes directed networks", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
    adj_mats <- lapply(adj_mats, lower_tri_remix)
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = T, igr_grid = c(2,6),
                         vis_export_opts = list(width = 6400, height = 2600),
                         igr_par_opts = list(mar=c(2,4,5,4)),
                         directed = T,
                         igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
})


test_that("cytoscape visualizes directed networks", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]
    adj_mats <- lapply(adj_mats, lower_tri_remix)

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")
    # Check if cytoscape is active
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")


    expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "cytoscape",
                                  edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                  directed = T,
                                  width_type = "partcor", vis_save = F),
                 NA)
})


test_that("igraph visualizes despite missing values in adj. matrix", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    adj_mats[24,25] <- NA
    adj_mats[25,24] <- NA

    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F), NA)
})


test_that("igraph shows self loops for directed and undirected networks", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         self_loops = TRUE,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F), NA)
    expect_error(
        VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "igraph",
                         self_loops = TRUE, directed = TRUE,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", vis_save = F), NA)
})


test_that("cytoscape visualizes self loops for directed and undirected networks", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")
    # Check if cytoscape is active
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "cytoscape",
                                  edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                  directed = F, self_loops = T,
                                  width_type = "partcor", vis_save = F),
                 NA)
    expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, output_type = "cytoscape",
                                  edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                  directed = T, self_loops = T,
                                  width_type = "partcor", vis_save = F),
                 NA)
})
