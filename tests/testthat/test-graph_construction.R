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


test_that("all additional column are added to node table when node_attrs is blank", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1,
                                     edge_attrs = "none") %>%
                   {dfs_from_graphNEL(.)[['vertices']]},
               c("node", "group", "color", "size"), ignore.order = TRUE)
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


test_that("all additional column are added to edge table when edge_attrs is left blank", {
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  expect_named(adjToNetwork(adj_mats = Mat1) %>%
                   {dfs_from_graphNEL(.)[['edges']]},
               c('color', 'source', 'target', 'weight', 'width'), ignore.order = TRUE)
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


test_that("group vec list and adj mat list are checked for equal size", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    group_vec <- list(group1 = group_vec, group2 = group_vec, group3 = group_vec)

    group_vec <- group_vec[-2]
    expect_error(adjToNetwork(adj_mats = adj_mats, group_vec = group_vec,
                              node_attrs = "group"),
                 "must be of equal length")
})


test_that("grouping vector of length 1 or same as data works", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    networks <- adjToNetwork(adj_mats, group_vec = group_vec,
                                 edge_attrs = "all", node_attrs = "all") %>%
        dfs_from_graphNEL()

    all_equal <- all(sapply(seq_along(networks), function (x) {
        all(networks[[1]]$vertices$group == networks[[x]]$vertices$group)
    }))

    expect_true(all_equal)

    grouping_list <- replicate(length(adj_mats), sample(group_vec), simplify = FALSE)

    networks <- adjToNetwork(adj_mats, group_vec = grouping_list,
                                 edge_attrs = "all", node_attrs = "all")  %>%
        dfs_from_graphNEL()

    expect_equal(lapply(seq_along(networks),
                        function (x) {
                            # resort to original order
                            tmp_idx <- networks[[x]]$vertices$node %>%
                                {match(colnames(adj_mats[[x]]), .)}
                            # Get original order group names
                            networks[[x]]$vertices[tmp_idx,]$group}),
                 grouping_list)
})


test_that("grouping vector as list of length 1 works", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]

    group_vec0 <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
    group_vec <- list(group_vec0)

    networks0 <- adjToNetwork(adj_mats, group_vec = group_vec0,
                             edge_attrs = "all", node_attrs = "all")  %>%
        dfs_from_graphNEL()
    networks <- adjToNetwork(adj_mats, group_vec = group_vec,
                             edge_attrs = "all", node_attrs = "all")  %>%
        dfs_from_graphNEL()

    expect_equal(networks$vertices$group, networks0$vertices$group)
})


test_that("user supplied colors are used", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    colors0 <- c("midnightblue", "black", "red", "#b99055") %>%
        col2hex()

    res_nets <- adjToNetwork(adj_mats, group_vec = group_vec,
                              edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                              width_type = "partcor", group_colors = colors0)
    cols_out <- res_nets %>% dfs_from_graphNEL() %>%
        lapply(function (x) {unique(x$vertices$color)}) %>%
        unlist() %>% unique()

    expect_true(all(c(all(cols_out %in% colors0), all(colors0 %in% cols_out))))

    colors0 <- c("midnightblue", "red", "#b99055") %>%
        col2hex()

    res_nets <- adjToNetwork(adj_mats, group_vec = group_vec,
                             edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                             width_type = "partcor", group_colors = colors0)
    cols_out <- res_nets %>% dfs_from_graphNEL() %>%
        lapply(function (x) {unique(x$vertices$color)}) %>%
        unlist() %>% unique()

    expect_true(all(c(all(cols_out %in% colors0), all(colors0 %in% cols_out))))
})


test_that("error is thrown when length of width type doesn't match", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(adjToNetwork(adj_mats, group_vec = group_vec,
                                  edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                                  width_type = c("partcor", "MI")),
                 "width type must be 1 or matching with")
})


test_that("width types can be provided as vector", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]] %>%
        {replicate(3, ., simplify = F)}

    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    nets <- adjToNetwork(adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = c("partcor", "partcor", "default")) %>%
        dfs_from_graphNEL()

    # GEt the widths for all edges
    widths <- lapply(nets, function(x) {
        x$edges$width
    })

    # Are the widths of the networks the same, only the third should be FALSE
    widths_equal <- lapply(widths, function(wid) {
        all(widths[[1]] == wid)
    })

    expect_equal(unlist(widths_equal), c(T, T, F))
})


test_that("colorblind accessible colors can be used", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    netw <- adjToNetwork(adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", colorblind = T) %>%
        dfs_from_graphNEL()

    colours <- netw$vertices$color %>% unique()

    expect_true(all(colours %in% palette.colors(palette = "Okabe-Ito")))
})


test_that("warning occurs is colorblind colors are overwritten by manually selected colors", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:2]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_warning(
        adjToNetwork(adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", colorblind = T,
                         group_colors = c("red", "green", "blue", "yellow")),
        "instead of colorblind accessible colors")
})


test_that("custom edge color function can be used for pos. + neg. data", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    net <- adjToNetwork(adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor",
                         # reversing the below color would make more sense
                         edge_color_func = pals::brewer.piyg) %>%
        dfs_from_graphNEL()

    edge_colors <- net$edges$color

    expect_true(all(edge_colors %in% pals::brewer.piyg(100)))
})


test_that("custom edge color function can be used for positive only data", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    adj_mats <- lapply(adj_mats, abs)
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    my_cols <- function (n) rev(pals::kovesi.linear_grey_10_95_c0(n))

    net <- adjToNetwork(adj_mats, group_vec = group_vec,
                         edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                         width_type = "partcor", edge_color_func = my_cols) %>%
        dfs_from_graphNEL()

    edge_colors <- net$edges$color

    expect_true(all(edge_colors %in% pals::kovesi.linear_grey_10_95_c0(100)))
})

test_that("For list input each element is checked seperately", {
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    class(adj_mats[[3]]) <- "graphNEL"
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(adjToNetwork(adj_mats, group_vec = group_vec,
                 edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                 width_type = "partcor"), "3 of your list is of class: graphNEL")
})


test_that("sorting by connectivity works for a single network", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(adjToNetwork(adj_mats[1], group_vec = group_vec,
                 edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                 width_type = "partcor") %>% visIgraph(export_type = "print"),
                 NA)
})


test_that("sorting by connectivity works for a multiple networks", {
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    expect_error(adjToNetwork(adj_mats[1:3], group_vec = group_vec,
                 edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                 width_type = "partcor") %>% anvis(igr_grid = c(1,3)),
                 NA)
})


test_that("addVisAttr produces same result as adjToNet",{
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1:3]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    net <- adjToNetwork(adj_mats, edge_attrs = "none", node_attrs = "none")

    net_ref <- adjToNetwork(adj_mats, group_vec = group_vec,
                        edge_attrs = "all", node_attrs = "all", arrange_co = TRUE,
                        width_type = "partcor")

    net_test <- addVisAttrs(net, group_vec = group_vec,
                            edge_attrs = "all", node_attrs = "all",
                            arrange_co = TRUE, width_type = "partcor")

    expect_equal(net_test, net_ref)
})


test_that("addVisAttr works for all network input types",{
    test_call <- deparse(sys.calls()[[1]][1])
    skip_if_not(test_call == "test_that()",
                message = "igraph visualizations need to be checked manually")

    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1] %>%
        {replicate(n = 3, expr = .)}
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    net <- adjToNetwork(adj_mats, edge_attrs = "none", node_attrs = "none")
    net[[2]] <- net[[2]] %>% igraph::graph_from_graphnel()
    net[[3]] <- net[[3]] %>% dfs_from_graphNEL()

    net_test <- addVisAttrs(net, group_vec = group_vec,
                            edge_attrs = "all", node_attrs = "all",
                            arrange_co = TRUE, width_type = "partcor")
    # Should produce 3 equal looking plots
    expect_error(anvis(net_test, igr_grid = c(1,3)),
                 NA)
})


test_that("addVisAttr maintains graph information for igraph objects",{
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    net <- adjToNetwork(adj_mats, edge_attrs = "none", node_attrs = "none")
    net <- net %>% igraph::graph_from_graphnel()

    # Set graph attribute
    igraph::graph_attr(net, name = "layout") <- "circular"

    net_test <- addVisAttrs(net, edge_attrs = "none", node_attrs = "none")

    expect_equal(igraph::graph_attr(net), igraph::graph_attr(net_test))
})


test_that("addVisAttr maintains graph information for graphNEL objects",{
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    net <- adjToNetwork(adj_mats, edge_attrs = "none", node_attrs = "none")

    # Set graph attribute
    net@graphData$layout <- "circular"

    net_test <- addVisAttrs(net, edge_attrs = "none", node_attrs = "none")

    expect_equal(net, net_test)
})


test_that("self loops are not doubled for undirected network",{
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    # Get network with double self loops
    net0 <- adjToNetwork(adj_mats, edge_attrs = "all", node_attrs = "all",
                        directed = F, self_loops = T, group_vec = group_vec,
                        width_type = "partcor") %>%
        dfs_from_graphNEL()

    net <- net0

    # visCytoscape(net0, close_session = F, save_session = F, export_image = T)

    # Remove any self loop duplicate manually
    net$edges <- dplyr::distinct(net$edges)

    igraph::graph_from_data_frame(d = net$edges, directed = F, vertices = net$vertices) %>%
        igraph::as_data_frame("both")

    expect_equal(net0, net)
})


test_that("adjToNetwork creates graphNEL, igraph, and list output",{
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[1]
    group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

    # Get network with double self loops
    nel <- adjToNetwork(adj_mats, edge_attrs = "all", node_attrs = "all",
                        directed = F, self_loops = T, group_vec = group_vec,
                        width_type = "partcor", output_as = "graphNEL")
    igr <- adjToNetwork(adj_mats, edge_attrs = "all", node_attrs = "all",
                        directed = F, self_loops = T, group_vec = group_vec,
                        width_type = "partcor", output_as = "igraph")
    df_list <- adjToNetwork(adj_mats, edge_attrs = "all", node_attrs = "all",
                        directed = F, self_loops = T, group_vec = group_vec,
                        width_type = "partcor", output_as = "list")

    expect_true(is(nel, "graphNEL"))
    expect_equal(class(igr), "igraph")
    expect_equal(class(df_list), "list")
})


test_that("edge_attr and node_attr do not cause error when default is 'all'",{
    adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))[[1]]

    # Get network with double self loops
    net_0 <- adjToNetwork(adj_mats, edge_attrs = "none", node_attrs = "none",
                          output_as = "igraph")
    net1 <- adjToNetwork(adj_mats, width_type = "partcor", output_as = 'igraph')
    net2 <- addVisAttrs(net_0)

    edge_ats_1 <- igraph::list.edge.attributes(net1)
    node_ats_1 <- igraph::list.vertex.attributes(net1)
    edge_ats_2 <- igraph::list.edge.attributes(net2)
    node_ats_2 <- igraph::list.vertex.attributes(net2)

    expect_equal(edge_ats_1, c("weight", "width", "color"))
    expect_equal(node_ats_1, c("name", "group", "color", "size"))
    expect_equal(edge_ats_2, c("weight", "width", "color"))
    expect_equal(node_ats_2, c("name", "group", "color", "size"))
})
