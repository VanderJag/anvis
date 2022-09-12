test_that("creating edgelist works for matrix", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Check if no error occurs
  expect_error(adj_matrix_to_edgelist(Mat1), NA)
})


test_that("creating edgelist works for data frame", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  df <- as.data.frame(Mat1)

  # Check if no error occurs
  expect_error(adj_matrix_to_edgelist(df), NA)
})


test_that("creating edgelist from data frame or matrix gives same result", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  df <- as.data.frame(Mat1)

  # Check if no error occurs
  expect_equal(adj_matrix_to_edgelist(Mat1), adj_matrix_to_edgelist(df))
})


test_that("creating edgelist throws error when adj. matrix is not symmetrical", {
  # Adjacency matrix
  mat1 <- matrix(rep(1:3, each = 3), ncol =3, nrow = 3)
  row.names(mat1) <- colnames(mat1) <- c("A", "B", "C")

  # Check if error occurs
  expect_warning(adj_matrix_to_edgelist(mat1), "unequal upper and lower")
})


test_that("creating edgelist does not fail when adj. matrix is symmetrical", {
  # Adjacency matrix
  mat1 <- matrix(c(1, 2, 3,
                   2, 2, 3,
                   3, 3, 3), ncol =3, nrow = 3)
  row.names(mat1) <- colnames(mat1) <- c("A", "B", "C")


  # Check if no error occurs
  expect_error(adj_matrix_to_edgelist(mat1), NA)
})


test_that("group vector must be provided to add node groups", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  expect_error(adjToNetwork(adj_mats = Mat1, node_attrs = "group",
                                     edge_attrs = "none", group_vec = NULL),
               "Must provide grouping vector")
})


test_that("group column is added to node table when 'group' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1, node_attrs = "group",
                        edge_attrs = "none", group_vec = group_vec) %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node", "group"), ignore.order = TRUE)
})


test_that("group color column error when there is no group", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  expect_error(adjToNetwork(adj_mats = Mat1, node_attrs = "color",
                        edge_attrs = "none")[['node_table']],
               "Must provide node table with group info")
})


test_that("group color column is added to node table when 'group' and 'color' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1, node_attrs = c("group", "color"),
                        edge_attrs = "none", group_vec = group_vec) %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node", "group", "color"), ignore.order = TRUE)
})


test_that("group column is added to node table when 'group' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1, node_attrs = "group",
                        edge_attrs = "none", group_vec = group_vec) %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node", "group"), ignore.order = TRUE)
})


test_that("size column is added to node table when 'size' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1, node_attrs = "size",
                        edge_attrs = "none", group_vec = group_vec) %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node", "size"), ignore.order = TRUE)
})


test_that("size, group, color is added to node table when 'all' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1, node_attrs = "all",
                                     edge_attrs = "none", group_vec = group_vec) %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node", "group", "color", "size"), ignore.order = TRUE)
})


test_that("no additional column is added to node table when 'none' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1, node_attrs = "none",
                                     edge_attrs = "none", group_vec = group_vec) %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node"), ignore.order = TRUE)
})


test_that("no additional column is added to node table when node_attrs is blank", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1,
                                     edge_attrs = "none") %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node"), ignore.order = TRUE)
})


test_that("width column is added to edge table when 'width' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1,
                                     edge_attrs = "width") %>%
                   {dfs_from_graphNEL(.)[['edges']]},
               c("source", "target", "weight", "width"), ignore.order = TRUE)
})


test_that("color column is added to edge table when 'color' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1,
                                     edge_attrs = "color") %>%
                   {dfs_from_graphNEL(.)[['edges']]},
               c("source", "target", "weight", "color"), ignore.order = TRUE)
})


test_that("color and width column is added to edge table when 'all' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1,
                                     edge_attrs = "all") %>%
                   {dfs_from_graphNEL(.)[['edges']]},
               c("source", "target", "weight", "width", "color"), ignore.order = TRUE)
})


test_that("no additional column is added to edge table when 'none' is selected", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1,
                                     edge_attrs = "none") %>%
                   {dfs_from_graphNEL(.)[['edges']]},
               c("source", "target", "weight"), ignore.order = TRUE)
})


test_that("no additional column is added to edge table when edge_attrs is left blank", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1) %>%
                   {dfs_from_graphNEL(.)[['edges']]},
               c("source", "target", "weight"), ignore.order = TRUE)
})


test_that("creating adj matrix from edgelist creates original matrix",{
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    names_order <- colnames(Mat1)
    diag(Mat1) <- 0
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    network_list <- adjToNetwork(Mat1,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = "partcor")  %>%
        dfs_from_graphNEL()
    edge_table <- network_list[["edges"]]
    node_table <- network_list[["vertices"]]

    new_adj <- edgelist_to_adj(edge_table) %>% as.matrix()
    new_adj <- new_adj[names_order,names_order]

    expect_equal(new_adj, Mat1)
})


test_that("error is shown by edge to adj when to weight column is not found", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
    network_list <- adjToNetwork(Mat1, width_type = "partcor",
                                          edge_attrs = "width") %>%
        dfs_from_graphNEL()
    edge_table <- network_list[["edges"]]
    edge_table2 <- edge_table %>% dplyr::select(-weight)

    expect_error(edgelist_to_adj(edge_table2, weight_col = "weight"),
                 "Weight column name must be NULL or present in names")
    expect_error(edgelist_to_adj(edge_table2, weight_col = "width"),
                 NA)
    expect_error(edgelist_to_adj(edge_table2, weight_col = NULL),
                 NA)
})

# the two plots created by this function should be identical
test_that("other functions can make use of the adj. matrix created from edgelist", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    # data to make matrix
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
    names_order <- colnames(Mat1)
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
    network <- adjToNetwork(Mat1,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = "partcor")


    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)

    edge_table <- dfs_from_graphNEL(network)[["edges"]]
    # use matrix for visualization
    new_adj <- edgelist_to_adj(edge_table)
    new_adj <- new_adj[names_order,names_order]
    network <- adjToNetwork(new_adj,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = "partcor")


    expect_error(visIgraph(network, radial_labs = T, export_type = "print"),
                 NA)
})


test_that("creating an directed edge list gives same as igraph implementation", {
    dir_mat <- readRDS(testthat::test_path("fixtures", "directed_adj_matrix.rds"))

    diag(dir_mat) <- 0

    g  <- igraph::graph_from_adjacency_matrix(dir_mat, weighted=TRUE, mode = "directed")
    ctrl_edges <- igraph::as_data_frame(g, "edges")
    ctrl_edges <- ctrl_edges %>% tibble::as_tibble() %>% dplyr::rename("source" = 1,
                                                                       "target" = 2)

    test_edges <- adj_matrix_to_edgelist(dir_mat, directed = TRUE)

    expect_equal(test_edges, ctrl_edges)
})


test_that("missing values are replaced by 0", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

    Mat1[24,25] <- NA
    Mat1[25,24] <- NA

    expect_message(adj_matrix_to_edgelist(Mat1), "replaced with 0")
})


test_that("adding or removing self interaction works", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

    edges <- adj_matrix_to_edgelist(Mat1)
    expect_true(!any(edges$source == edges$target))

    n <- nrow(Mat1)

    edges <- adj_matrix_to_edgelist(Mat1, self_loops = TRUE)
    expect_true(sum(edges$source == edges$target) == n)
})


test_that("self interaction works for main network creation function", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

    network_list <- adjToNetwork(Mat1, width_type = "partcor") %>%
        dfs_from_graphNEL()
    edges <- network_list[["edges"]]

    expect_true(!any(edges$source == edges$target))

    n <- nrow(Mat1)

    network_list <- adjToNetwork(Mat1, self_loops = TRUE,
                                 directed = TRUE, width_type = "partcor")  %>%
        dfs_from_graphNEL()
    edges <- network_list[["edges"]]

    expect_true(sum(edges$source == edges$target) == n)
})


test_that("no additional attributes are added when both edge and node attrs are none", {
    Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))

    nel <- adjToNetwork(Mat1,
                 directed = FALSE,
                 self_loops = FALSE,
                 node_attrs = "none",
                 edge_attrs = "none")

    dfs <- dfs_from_graphNEL(nel)

    df_names <- c(names(dfs$vertices), names(dfs$edges))

    expect_setequal(df_names, c("node", "source", "target", "weight"))
})


test_that("3 networks can be created at once", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats = adj_mats,
                 node_attrs = "none",
                 edge_attrs = "none")

    expect_equal(sapply(nets, function(x) class(x)), rep("graphNEL", 3))
})


test_that("vectorized version of create network creates list of networks with no additional attributes", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    node_attrs <- adjToNetwork(adj_mats = adj_mats,
                 node_attrs = "none",
                 edge_attrs = "none", width_type = "partcor") %>%
        lapply(function(x) dfs_from_graphNEL(x) %>%
                   {names(.$vertices)})
    edge_attrs <- adjToNetwork(adj_mats = adj_mats,
                 node_attrs = "none",
                 edge_attrs = "none", width_type = "partcor") %>%
        lapply(function(x) dfs_from_graphNEL(x) %>%
                   {names(.$edges)})

    expect_equal(node_attrs, list("node", "node", "node"))
    expect_equal(edge_attrs, list(c("source", "target", "weight"),
                                  c("source", "target", "weight"),
                                  c("source", "target", "weight")))
})


test_that("vectorized version of create network creates list of networks with all attributes", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    node_attrs <- adjToNetwork(adj_mats = adj_mats,
                               node_attrs = "all", group_vec = group_vec,
                               edge_attrs = "all", width_type = "partcor") %>%
        lapply(function(x) dfs_from_graphNEL(x) %>%
                   {names(.$vertices)})
    edge_attrs <- adjToNetwork(adj_mats = adj_mats,
                               node_attrs = "all", group_vec = group_vec,
                               edge_attrs = "all", width_type = "partcor") %>%
        lapply(function(x) dfs_from_graphNEL(x) %>%
                   {names(.$edges)})

    expect_equal(node_attrs, list(c("node", "group", "color", "size"),
                                  c("node", "group", "color", "size"),
                                  c("node", "group", "color", "size")))
    expect_equal(edge_attrs, list(c("source", "target", "weight", "width", "color"),
                                  c("source", "target", "weight", "width", "color"),
                                  c("source", "target", "weight", "width", "color")))
})


# TODO still need to adapt tests
test_that("group vec list and adj mat list are checked for equal size", {
    mat0 <- matrix(rep(1, 4), ncol = 2)
    mat_list <- list(mat1 = mat0, mat2 = mat0, mat3 = mat0)
    groupings <- c("a", "b")
    group_vec <- list(group1 = groupings, group2 = groupings, group3 = groupings)

    group_vec <- group_vec[-2]
    expect_error(VisualiseNetwork(adj_mats = mat_list, group_vec = group_vec),
                 "must be of equal length")
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
