test_that("grouping throws error when group vector doesn't have correct length", {
  df <- tibble::as_tibble_col(c("abc", "bbc", "cbc"), column_name = "node")
  groups <- c("A", "B")

  expect_error(group_nodes(node_table = df, group_vec = groups),
               "matching `node_table` and `group_vec`")
})


test_that("grouping add group column", {
  df <- tibble::as_tibble_col(c("abc", "bbc", "cbc"), column_name = "node")
  groups <- c("A", "B", "C")

  expect_named(group_nodes(node_table = df, group_vec = groups),
               c("node", "group"))
})


test_that("grouping returns sorted columns", {
  df <- tibble::as_tibble_col(c("abc", "cbc", "bbc"), column_name = "node")
  groups <- c("A", "C", "B")

  expect_equal(group_nodes(node_table = df, group_vec = groups)$node, sort(df$node))
  expect_equal(group_nodes(node_table = df, group_vec = groups)$group, sort(groups))
})


test_that("edge width works without error for all types", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)

  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "cor"), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "partcor"), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "MI") %>%
                 suppressWarnings(), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "ranked"), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "percentile"), NA)
})


test_that("all edge width types return a column called width", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)

  add_edge_widths <- . %>%
    edge_weight_to_widths(width_type = ., edge_table = edge_table)

  expect_true("width" %in% colnames("cor" %>% add_edge_widths))
  expect_true("width" %in% colnames("partcor" %>% add_edge_widths))
  expect_true("width" %in% colnames("MI" %>% add_edge_widths) %>%
                suppressWarnings())
  expect_true("width" %in% colnames("ranked" %>% add_edge_widths))
  expect_true("width" %in% colnames("percentile" %>% add_edge_widths))
})


test_that("Warning is shown when range doesn't match partcor or cor", {
  # Create artificial edgelist with column 'weight'
  df <- tibble::tibble("weight" = seq(0,3.6, length.out = 40))

  expect_warning(edge_weight_to_widths(df, "partcor"), "Unexpected results may be returned")
  expect_warning(edge_weight_to_widths(df, "cor"), "Unexpected results may be returned")

  df <- tibble::tibble("weight" = seq(-5, 0, length.out = 40))

  expect_warning(edge_weight_to_widths(df, "partcor"), "Unexpected results may be returned")
  expect_warning(edge_weight_to_widths(df, "cor"), "Unexpected results may be returned")
})


test_that("Warning is shown when range doesn't match partcor or cor", {
  # Create artificial edgelist with column 'weight'
  df <- tibble::tibble("weight" = seq(-5,3000, length.out = 40))

  expect_warning(edge_weight_to_widths(df, "MI"), "'MI' width type")
})


test_that("order of edge width is the same as edge weight", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)
  # Check order of edges
  ordered <- dplyr::arrange(edge_table, desc(abs(weight)))

  test_prep <- . %>%
    edge_weight_to_widths(width_type = ., edge_table = edge_table) %>%
    dplyr::arrange(dplyr::desc(abs(width))) %>%
    dplyr::select(-width)

  expect_equal("cor" %>% test_prep, ordered)
  expect_equal("partcor" %>% test_prep, ordered)
  expect_equal(suppressWarnings("MI" %>% test_prep), ordered)
  expect_equal("ranked" %>% test_prep, ordered)
  # Percentiles have the same width so the ordering trick of this test does not
  #   work for the "percentile" option.
  # expect_equal("percentile" %>% test_prep, ordered)
})


test_that("edge width are in the expected range of 0 to 1", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)

  get_edge_widths <- . %>%
    edge_weight_to_widths(width_type = ., edge_table = edge_table) %>%
    dplyr::pull(width)

  expect_true(range("cor" %>% get_edge_widths)[1] >= 0)
  expect_true(range("cor" %>% get_edge_widths)[2] <= 1)
  expect_true(range("partcor" %>% get_edge_widths)[1] >= 0)
  expect_true(range("partcor" %>% get_edge_widths)[2] <= 1)
  expect_true(suppressWarnings(range("MI" %>% get_edge_widths)[1]) >= 0)
  expect_true(suppressWarnings(range("MI" %>% get_edge_widths)[2]) <= 1)
  expect_true(range("ranked" %>% get_edge_widths)[1] >= 0)
  expect_true(range("ranked" %>% get_edge_widths)[2] <= 1)
  expect_true(range("percentile" %>% get_edge_widths)[1] >= 0)
  expect_true(range("percentile" %>% get_edge_widths)[2] <= 1)
})


test_that("order of edges gets maintained after adding widths column", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)

  test_prep <- . %>%
    edge_weight_to_widths(width_type = ., edge_table = edge_table) %>%
    dplyr::arrange(dplyr::desc(abs(width))) %>%
    dplyr::select(-width)
  expect_equal(edge_weight_to_widths(edge_table, "cor") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
  expect_equal(edge_weight_to_widths(edge_table, "partcor") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
  expect_equal(suppressWarnings(edge_weight_to_widths(edge_table, "MI")) %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
  expect_equal(edge_weight_to_widths(edge_table, "ranked") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
  expect_equal(edge_weight_to_widths(edge_table, "percentile") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
})


test_that("connectivity based on edge weights matches one determined by rowSums", {
  # Load adjacency matrix
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

  network <- adjToNetwork(Mat1, directed = T, self_loops = T, node_attrs = "none")
  network <- dfs_from_graphNEL(network)
  edge_table <- network[["edges"]]
  node_table <- network[["vertices"]]
  node_table <- node_size_connectivity(node_table = node_table,
                                       edge_table = edge_table,
                                       size_type = "scaled_only")

  # Calculate by alternative means
  conn2 <- rowSums(abs(Mat1), na.rm = TRUE)
  conn2 <- (conn2 / max(conn2)) %>% sigmoid_xB(3) %>% unname()

  expect_equal(node_table$size, conn2)
})


test_that("connectivity can be determined when a node has no edges", {
  # Load adjacency matrix
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

  network <- adjToNetwork(Mat1, directed = F, self_loops = F, node_attrs = "none")
  network <- dfs_from_graphNEL(network)
  edge_table <- network[["edges"]]
  edge_table <- edge_table %>% dplyr::rowwise() %>%
      dplyr::filter(source != "C5C5a" && target != "C5C5a")
  node_table <- network[["vertices"]]
  node_table <- node_size_connectivity(node_table = node_table,
                                       edge_table = edge_table,
                                       size_type = "scaled_only")

  # Check if the removed edge has size 0
  rm_nodesize <- node_table %>% dplyr::filter(node == "C5C5a") %>% dplyr::pull(size)

  expect_equal(rm_nodesize, 0)
})


test_that("avg conn. shows warning when not all node tables have size column", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) {
                       adjToNetwork(adj_mats[[x]],
                                             node_attrs = "all",
                                             edge_attrs = "all",
                                             group_vec = group_vec[[
                                               if (length(group_vec) == length(adj_mats)) x else 1]],
                                             width_type = "partcor") %>%
                             dfs_from_graphNEL()})

  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$vertices)
  Network = list(adjacencies = adj_mats, nodes = nodes)

  expect_error(sort_avg_connectivity(Network$nodes), NA)

  Network$nodes[[12]] <- Network$nodes[[12]] %>% dplyr::select(-size)
  expect_warning(sort_avg_connectivity(Network$nodes),
                 "Not all networks have node size attribute")

  for (i in 1:11) {
    Network$nodes[[i]] <- Network$nodes[[i]] %>% dplyr::select(-size)
  }
  expect_error(sort_avg_connectivity(Network$nodes),
               "Must provide data frames with 'size' connectivity column")

})


test_that("avg conn. gives the same result for reordered node tables", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) {
                       adjToNetwork(adj_mats[[x]],
                                             node_attrs = "all",
                                             edge_attrs = "all",
                                             group_vec = group_vec[[
                                               if (length(group_vec) == length(adj_mats)) x else 1]],
                                             width_type = "partcor") %>%
                             dfs_from_graphNEL()})

  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$vertices)
  nodes_reorder <- lapply(seq_along(adj_mats),
                          function(x) networks[[x]]$vertices %>%
                              {.[sample(1:nrow(.)),]})

  expect_equal(sort_avg_connectivity(nodes), sort_avg_connectivity(nodes_reorder))
})


test_that("avg. conn. is calculated when some some node tables miss size attribute", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) {
                       adjToNetwork(adj_mats[[x]],
                                             node_attrs = "all",
                                             edge_attrs = "all",
                                             group_vec = group_vec[[
                                               if (length(group_vec) == length(adj_mats)) x else 1]],
                                             width_type = "partcor") %>%
                             dfs_from_graphNEL()})
  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$vertices)
  nodes_minus <- nodes
  nodes_minus[[12]]$size <- NULL

  sorted_nodes <- sort_avg_connectivity(nodes_minus) %>% suppressWarnings()
  sec_node <- sapply(1:12, function (x) sorted_nodes[[x]][2,1])

  # Fasligand has the second highest average for its group, check this to see
  #   if without the connectivity of the last node table the nodes are still sorted
  expect_true(all(stringr::str_detect(sec_node, "FasLigand")))
})


test_that("avg. conn. give error for missing node column", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) {
                       adjToNetwork(adj_mats[[x]],
                                             node_attrs = "all",
                                             edge_attrs = "all",
                                             group_vec = group_vec[[
                                               if (length(group_vec) == length(adj_mats)) x else 1]],
                                             width_type = "partcor") %>%
                             dfs_from_graphNEL()})
  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$vertices)
  nodes_minus <- nodes
  nodes_minus[[12]]$node <- NULL

  expect_error(sort_avg_connectivity(nodes_minus),
               "Must provide node names")
})


test_that("avg. conn. give error for unequal node sets", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) {
                       adjToNetwork(adj_mats[[x]],
                                             node_attrs = "all",
                                             edge_attrs = "all",
                                             group_vec = group_vec[[
                                               if (length(group_vec) == length(adj_mats)) x else 1]],
                                             width_type = "partcor") %>%
                             dfs_from_graphNEL()})
  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$vertices)
  nodes_minus <- nodes
  nodes_minus[[12]]$node <- stringr::str_replace_all(nodes_minus[[12]]$node, "MMP8", "MMP80000")
  nodes_minus[[3]]$node <- stringr::str_replace_all(nodes_minus[[3]]$node, "MMP8", "BAD_NODE")

  expect_error(sort_avg_connectivity(nodes_minus),
               "have the same nodes in node tables")
})


test_that("edge colors change when a function to get colors is provided", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Create edge table
    edge_table <- adj_matrix_to_edgelist(Mat1)

    colors_before <- weights_to_color(edge_table = edge_table)$color

    # set up color function
    diverge_col_f <- function (n) pals::brewer.rdbu(n = n)

    colors_after <- weights_to_color(edge_table = edge_table, diverge_col_f)$color

    expect_true(all(colors_before != colors_after))
})


test_that("error occurs when edge_color_func is not a function", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Create edge table
    edge_table <- adj_matrix_to_edgelist(Mat1)

    # set up color function
    col_func <- "my_func"

    expect_error(weights_to_color(edge_table = edge_table, col_func),
                 "Must provide a function to change edge colors")
})


test_that("errors of the edge_color_func are caught and raised with extra message", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Create edge table
    edge_table <- adj_matrix_to_edgelist(Mat1)

    # set up color function
    col_func <- function (n) {
        if (n > 5){
            stop("I can only give you 5 colors")
        }
    }

    # weights_to_color(edge_table = edge_table, col_func)
    weights_to_color(edge_table = edge_table, col_func) %>%
        expect_message("While getting 100 colors")  %>%
        expect_message("Make sure that") %>%
        expect_error("I can only give you 5 colors")
})


test_that("warnings of the edge_color_func are caught and shown with extra message", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Create edge table
    edge_table <- adj_matrix_to_edgelist(Mat1)

    # set up color function
    col_func <- function (n) {
        if (n > 1){
            warning("I only give one unique color")
        }

        return(rep("red", 100))
    }

    # weights_to_color(edge_table = edge_table, col_func)
    weights_to_color(edge_table = edge_table, col_func) %>%
        expect_message("the following warning was raised") %>%
        expect_warning("I only give one unique")
})


test_that("error when edge_color_func doesn't return 100 values", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Create edge table
    edge_table <- adj_matrix_to_edgelist(Mat1)

    # set up color function
    col_func <- function (n) {
        return(rep("red", n-5))
    }

    # weights_to_color(edge_table = edge_table, col_func)
    expect_error(weights_to_color(edge_table = edge_table, col_func),
                 "must return 100 colors")
})


test_that("error when edge_color_func doesn't return 100 values", {
    # Load adjacency matrix
    Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
    # Create edge table
    edge_table <- adj_matrix_to_edgelist(Mat1)

    # set up color function
    col_func <- function (n) {
        return(rep("fakellow", 100))
    }

    # weights_to_color(edge_table = edge_table, col_func)
    expect_error(weights_to_color(edge_table = edge_table, col_func),
                 "valid color names") %>%
        expect_message("returns valid colors")
})


