test_that("when cytoscape is not available RCy3 will give error", {
  # Presence of cytoscape test
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if(cytosc$message == "You are connected to Cytoscape!\n",
          message = "this test runs only when cytoscape is inactive")

  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  # Create network
  net <- adjToNetwork(Mat1, group_vec = group_vec,
                      edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                      width_type = "partcor")

  # Visualize the network
  expect_message(anvis(net, output_type = "cyto"),
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

  # Create network
  net <- adjToNetwork(Mat1, group_vec = group_vec,
                      edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                      width_type = "partcor")

  # Visualize the network
  expect_error(anvis(net, output_type = "cyto"), NA)
})


test_that("Cytoscape visualizations are made for each network in the list", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  # Presence of cytoscape test
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()

  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  nets <- adjToNetwork(adj_mats, group_vec = group_vec,
                      edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                      width_type = "partcor")

  withr::with_file(c("network.png", paste0("network_", 2:3, ".png")), {
    anvis(nets, output_type = "cyto", vis_save = T,
          cyto_save_session = F, cyto_close_session = T)

    expect_setequal(list.files(pattern = "network"),
                    c("network.png", paste0("network_", 2:3, ".png")))
  })

})


test_that("Igraph visualizations are made for each network in the list", {
  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:12]

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  expect_error(anvis(nets, output_type = "igraph", igr_grid = T), NA)
})


test_that("edge widths can be scaled", {
  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")
  # Check if cytoscape is active
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  expect_error(anvis(nets, output_type = "igraph", vis_save = F, vis_edge_factor = 20),
               NA)
  expect_error(anvis(nets, output_type = "cytoscape", vis_save = F, vis_edge_factor = 20),
               NA)
})


test_that("cytoscape nodespace works", {
  # Check if cytoscape is active
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  expect_error(anvis(nets, output_type = "cytoscape",
                     vis_save = F, cyto_node_space = 4),
               NA)
})


test_that("igraph can arrange visualizations in grid", {
  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  expect_error(
      anvis(nets, output_type = "igraph", vis_save = T, igr_grid = T),
      NA)
})


test_that("igraph grid can be specified manually", {
  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  expect_error(
      anvis(nets, output_type = "igraph",
                       vis_save = T, igr_grid = c(2,6),
                       vis_export_opts = list(width = 8200, height = 2600)), NA)
})


test_that("errors occur for incorrect grid igr_grid input", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor")

    expect_error(
        anvis(nets, output_type = "igraph", vis_save = T, igr_grid = c(2,6,3),
              vis_export_opts = list(width = 7000, height = 2600)),
        "TRUE, FALSE, or a vector of two integers")

    expect_error(
        anvis(nets, output_type = "igraph", vis_save = T, igr_grid = NULL,
              vis_export_opts = list(width = 7000, height = 2600)),
        "TRUE, FALSE, or a vector of two integers")

    expect_error(
        anvis(nets, output_type = "igraph",
              vis_save = T, igr_grid = c(2,2),
              vis_export_opts = list(width = 7000, height = 2600)),
        "grid dimensions must exceed or equal number of networks")
})


test_that("igraph grid node label space can be adjusted with margins", {
  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  expect_error(
      anvis(nets, output_type = "igraph", vis_save = T, igr_grid = c(2,6),
            vis_export_opts = list(width = 6400, height = 2400),
            igr_par_opts = list(mar=c(6,6,6,6))), NA)
})


test_that("xgmml output works for a single network", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Some grouping based on column names
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = Mat1, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  withr::with_file(c("network.xgmml"), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    anvis(nets, output_type = "network", netw_ext = "XGMML")

    expect_setequal(list.files(pattern = "network"),
                    c("network.xgmml"))
  })
})


test_that("xgmml output uses custom save names", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Some grouping based on column names
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = Mat1, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor")

  withr::with_file(c("my_xgmml.xgmml"), {
    anvis(nets, output_type = "network", netw_ext = "XGMML",
                     save_names = "my_xgmml")

    expect_setequal(list.files(pattern = "my_xgmml"),
                    c("my_xgmml.xgmml"))
  })
})


test_that("xgmml output saves multiple networks using save name numbering", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:4]
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                       width_type = "partcor")

  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    anvis(nets, output_type = "network", netw_ext = "XGMML")

    expect_setequal(list.files(pattern = "network"),
                    c("network.xgmml", paste0("network_", 2:4, ".xgmml")))
  })
})


test_that("xgmml output writes xgmml title", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:4]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor")

  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    anvis(nets, output_type = "network", netw_ext = "XGMML")

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

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all",
                       arrange_co = TRUE, width_type = "partcor")

  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    anvis(nets, output_type = "network",
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

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all",
                       arrange_co = TRUE, width_type = "partcor")

  withr::with_file(c("network.xgmml", paste0("network_", 2:4, ".xgmml")), {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    expect_error(anvis(nets, output_type = "network",
                     netw_ext = "XGMML", netw_xgmml_title = c("one", "two",
                                                              "three")),
                 "Length of xgmml titles must be 1 or matching with")
  })
})


test_that("xgmml output has a matching number of nodes and edges compared to original data", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  output <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all",
                       arrange_co = TRUE, width_type = "partcor") %>%
      dfs_from_graphNEL()

  withr::with_file("network.xgmml", {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    nr_edges <- output$edges %>% nrow()
    nr_nodes <- output$vertices %>% nrow()

    anvis(output, output_type = "network", netw_ext = "XGMML")

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

  output <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all",
                       arrange_co = TRUE, width_type = "partcor") %>%
      dfs_from_graphNEL()

  withr::with_file("network.xgmml", {
    disrupt_files <- list.files(pattern = "network.*xgmml")
    rmed <- file.remove(disrupt_files)

    edge_attrs <- output$edges %>% names()
    edge_attrs <- edge_attrs[!(edge_attrs %in% c("source", "target"))]
    node_attrs <- output$vertices %>% names()
    node_attrs[(node_attrs %in% c("node"))] <- "name"

    anvis(nets, output_type = "network",
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
  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "igraph visualizations need to be checked manually")

  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all",
                       arrange_co = TRUE, width_type = "partcor")

  expect_error(
      anvis(nets, output_type = "igraph", vis_save = T, igr_grid = c(2,6),
            vis_export_opts = list(width = 6400, height = 2600),
            igr_par_opts = list(mar=c(2,4,5,4)),
            igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
  # expect_error(
  #     anvis(nets, output_type = "igraph", vis_save = T, igr_grid = c(2,6),
  #           vis_export_opts = list(width = 18, height = 7.5),
  #           igr_par_opts = list(mar=c(2,4,5,4)), vis_export_type = "svg",
  #           igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
})


test_that("igraph grid titles need correct length else error", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                       edge_attrs = "all", node_attrs = "all",
                       arrange_co = TRUE, width_type = "partcor")

  expect_error(
      anvis(nets, output_type = "igraph", vis_save = T, igr_grid = c(2,6),
            vis_export_opts = list(width = 6400, height = 2600),
            igr_par_opts = list(mar=c(2,4,5,4)),
            igr_grid_names = paste("patient", LETTERS[1:length(adj_mats)-1])),
      "Grid names must be TRUE, FALSE, or of length matching")
})


test_that("igraph grid titles cause warning when requested but adj list unnamed", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor")

    expect_warning(
        anvis(nets, output_type = "igraph", vis_save = F, igr_grid = c(1,3),
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

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor")

    expect_error(
        anvis(nets, output_type = "igraph", vis_save = F, igr_grid = c(1,3),
              igr_par_opts = list(mar=c(2,4,5,4)),
              igr_grid_names = T),
        NA)
})


test_that("validation of list arguments works", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    names(adj_mats) <- paste("person", LETTERS[1:3])
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor")

    expect_error(
        anvis(nets, output_type = "igraph",
                         vis_save = F, igr_grid = c(1,3),
                         igr_par_opts = list(mar=c(2,4,5,4)), igr_plot_opts = c("a"),
                         igr_grid_names = T),
        "must be a list")
})


test_that("visualization with only positive values is informative", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    adj_mats <- lapply(adj_mats, abs)

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor")

    expect_error(
        anvis(nets, output_type = "igraph",
              vis_save = T, igr_grid = c(2,6),
              vis_export_opts = list(width = 6400, height = 2600),
              igr_par_opts = list(mar=c(2,4,5,4)),
              igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
})


test_that("igraph visualizes directed networks", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    adj_mats <- lapply(adj_mats, lower_tri_remix)
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = T)

    expect_error(
        anvis(nets, output_type = "igraph",
              vis_save = F, igr_grid = c(1,3),
              vis_export_opts = list(width = 6400, height = 2600),
              igr_par_opts = list(mar=c(2,4,5,4)),
              igr_grid_names = paste("patient", LETTERS[seq_along(adj_mats)])), NA)
})


test_that("cytoscape visualizes directed networks", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")
    # Check if cytoscape is active
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]
    adj_mats <- lapply(adj_mats, lower_tri_remix)

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = T)

    expect_error(anvis(nets, output_type = "cytoscape",
                       vis_save = F),
                 NA)
})


test_that("igraph visualizes despite missing values in adj. matrix", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    adj_mats[24,25] <- NA
    adj_mats[25,24] <- NA

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor")

    expect_error(anvis(nets, output_type = "igraph", vis_save = F), NA)
})


test_that("igraph shows self loops for directed and undirected networks", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", self_loops = TRUE)

    expect_error(anvis(nets, output_type = "igraph", vis_save = F), NA)

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = T,
                         self_loops = TRUE)

    expect_error(anvis(nets, output_type = "igraph", vis_save = F), NA)
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

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = F,
                         self_loops = TRUE)

    expect_error(anvis(nets, output_type = "cytoscape", vis_save = F), NA)

    nets <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = T,
                         self_loops = TRUE)

    expect_error(anvis(nets, output_type = "cytoscape", vis_save = F),NA)
})


test_that("anvis makes cytoscape visualization for 3 network types", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "cytoscape visualizations need to be checked manually")

    # Check if cytoscape is active
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nel <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = T,
                         self_loops = TRUE)
    igr <- igraph::graph_from_graphnel(nel)
    df <- nel %>% dfs_from_graphNEL()

    expect_error(anvis(nel, output_type = "cytoscape", vis_save = F), NA)
    expect_error(anvis(igr, output_type = "cytoscape", vis_save = F), NA)
    # Only the below one will be undirected, to make it directed anvis needs an
    #     additional argument
    expect_error(anvis(df, output_type = "cytoscape", vis_save = F), NA)
})


test_that("anvis makes igraph visualization for 3 network types", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    # Check if cytoscape is active
    cytosc <- RCy3::cytoscapePing() %>% capture_condition()
    skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
                message = "this test runs only when cytoscape is active")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nel <- adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all",
                         arrange_co = TRUE, width_type = "partcor", directed = T,
                         self_loops = TRUE)
    igr <- igraph::graph_from_graphnel(nel)
    df <- nel %>% dfs_from_graphNEL()

    expect_error(anvis(nel, output_type = "igraph", vis_save = F), NA)
    expect_error(anvis(igr, output_type = "igraph", vis_save = F), NA)
    # Only the below one will be undirected, to make it directed anvis needs an
    #     additional argument
    expect_error(anvis(df, output_type = "igraph", vis_save = F), NA)
})


# test_that("informative errors are raised when plotting window is too small", {
#     # To reproduce the error that is caught the plotting window of Rstudio
#     #     must be very narrow. We've detected two possible errors that occur for
#     #     figure margins that are to large for the plotting window. When the
#     #     Rstudio graphical device is active (`names(dev.cur()[1]) == "RStudioGD"`)
#     #     the raised error will be "invalid graphics state". When instead the
#     #     null device is active `names(dev.cur()[1]) == "null device"` the error
#     #     will be "figure margins too large".
#
#     test_call <- deparse(sys.calls()[[1]][1])
#     skip_if_not(test_call == "test_that()",
#                 message = "igraph visualizations need to be checked manually")
#
#     group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#     nets <- adjToNetwork(adj_mats = sepsis, group_vec = group_vec,
#                          edge_attrs = "all", node_attrs = "all",
#                          arrange_co = TRUE, width_type = "partcor")
#
#     expect_message(
#         anvis(nets, output_type = "igraph", vis_save = F, igr_grid = c(2,6),
#               igr_grid_names = T), "Resizing plot window") %>%
#         expect_error()
# })


# test_that("100 nodes is a reasonable limit for easy to interpret visualizations made with igraph", {
#     # Choose
#     n <- 100
#     # making a network from multiple times the existing network, n is nearest multiple
#     reps <- ceiling(n / 36)
#     n <- reps * 36
#
#     adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]
#     group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#     xl_names <- adj_mats %>% names() %>% rep(times = reps) %>%
#         paste0("_", rep(1:reps, each = ncol(adj_mats)))
#
#     xl_vals <- adj_mats[lower.tri(adj_mats)]
#
#     xl_mat <- matrix(sample(xl_vals, size = n*n, replace = T), nrow = n, ncol = n)
#
#     diag(xl_mat) <- 1
#
#     dimnames(xl_mat) <- list(xl_names, xl_names)
#
#     xl_groups <- rep(group_vec, times = reps)
#
#     nel <- adjToNetwork(adj_mats = xl_mat, group_vec = xl_groups,
#                         edge_attrs = "all", node_attrs = "all",
#                         arrange_co = TRUE, width_type = "partcor", directed = F,
#                         self_loops = F)
#
#     # check nr of non 0 edges
#     n_edges <- sum(abs(xl_mat[xl_mat %>% upper.tri()]) > 0)
#     expect_error(anvis(nel, output_type = "igraph", vis_save = T,
#                        save_names = paste0("network_nodes_", n, "_edges_", n_edges)),
#                  NA)
# })
#
#
# test_that("250 nodes is a reasonable limit for easy to interpret visualizations made with cytoscape", {
#     # Choose
#     n <- 250
#     # making a network from multiple times the existing network, n is nearest multiple
#     reps <- ceiling(n / 36)
#     n <- reps * 36
#
#     adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]
#     group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#     xl_names <- adj_mats %>% names() %>% rep(times = reps) %>%
#         paste0("_", rep(1:reps, each = ncol(adj_mats)))
#
#     xl_vals <- adj_mats[lower.tri(adj_mats)]
#
#     xl_mat <- matrix(sample(xl_vals, size = n*n, replace = T), nrow = n, ncol = n)
#
#     diag(xl_mat) <- 1
#
#     dimnames(xl_mat) <- list(xl_names, xl_names)
#
#     xl_groups <- rep(group_vec, times = reps)
#
#     nel <- adjToNetwork(adj_mats = xl_mat, group_vec = xl_groups,
#                         edge_attrs = "all", node_attrs = "all",
#                         arrange_co = TRUE, width_type = "partcor", directed = F,
#                         self_loops = F)
#
#     # check nr of non 0 edges
#     n_edges <- sum(abs(xl_mat[xl_mat %>% upper.tri()]) > 0)
#     expect_error(anvis(nel, output_type = "cytoscape", vis_save = T,
#                        save_names = paste0("network_nodes_", n, "_edges_", n_edges)),
#                  NA)
# })
