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


test_that("connectivity throws error when adj_matrix and node_table have different nodes", {
  # Load adjacency matrix
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  node_table <- network_list[["node_table"]]

  # Remove elements
  Mat1 <- Mat1[,-2]
  node_table <- node_table %>% dplyr::filter(node != "ESelectin")

  expect_error(node_size_connectivity(node_table = node_table, Mat1, size_type = NULL),
               "Must provide node_table and adj_matrix with the same nodes")
})


test_that("connectivity rowSums matches one determined based on edge weights", {
  # Load adjacency matrix
  Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  network_list <- adj_matrix_to_network(Mat1,
                                        group_vec = group_vec,
                                        width_type = "partcor")
  edge_table <- network_list[["edge_table"]]
  node_table <- network_list[["node_table"]]
  node_table <- node_size_connectivity(node_table = node_table, Mat1, size_type = "scaled_only")

  # Calculate by alternative means
  conns2 <- sapply(node_table$node,
                   function (x) sum(abs(c(edge_table[edge_table$source == x,]$weight,
                                           edge_table[edge_table$target == x,]$weight))))
  conns2 <- (conns2 / max(conns2)) %>% sigmoid_xB(3) %>% unname()

  expect_equal(node_table$size, conns2)
})


test_that("avg conn. shows warning when not all node tables have size column", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
  group_vec <- list(group_vec)

  networks <- VisualiseNetwork(adj_mats, group_vec = group_vec, vis_type = "xgmml")
  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) {
                       adj_matrix_to_network(adj_mats[[x]],
                                             node_attrs = "all",
                                             edge_attrs = "all",
                                             group_vec = group_vec[[
                                               if (length(group_vec) == length(adj_mats)) x else 1]],
                                             width_type = "partcor")})
  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$node_table)
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

