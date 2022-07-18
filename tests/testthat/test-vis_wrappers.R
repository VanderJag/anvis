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
  expect_message(VisualiseNetwork(Mat1, group_vec = group_vec, vis_type = "cyto"),
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
  expect_error(VisualiseNetwork(Mat1, group_vec = group_vec, vis_type = "cyto"), NA)
})


test_that("group vec list and adj mat list are checked for equal size", {
  mat0 <- matrix(rep(1, 4), ncol = 2)
  mat_list <- list(mat1 = mat0, mat2 = mat0, mat3 = mat0)
  groupings <- c("a", "b")
  group_vec <- list(group1 = groupings, group2 = groupings, group3 = groupings)

  group_vec <- group_vec[-2]
  expect_error(VisualiseNetwork(mat_list, group_vec),
               "must be equal length")
})

test_that("Igraph visualizations are made for each network in the list", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  # Presence of cytoscape test, not currently working because
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()
  skip_if(cytosc$message == "You are connected to Cytoscape!\n",
          message = "this test runs only when cytoscape is inactive")

})

# test_that("Cytoscape visualizations are made for each network in the list", {
#   adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))
#
#   group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))
#
#   # Presence of cytoscape test, not currently working because
#   cytosc <- RCy3::cytoscapePing() %>% capture_condition()
#   skip_if(cytosc$message == "You are connected to Cytoscape!\n",
#           message = "this test runs only when cytoscape is inactive")
#
# })

test_that("Igraph visualizations are made for each network in the list", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  test_call <- deparse(sys.calls()[[1]][1])
  skip_if_not(test_call == "test_that()",
              message = "cytoscape visualizations need to be checked manually")

  expect_error(VisualiseNetwork(adj_mats, group_vec = group_vec, vis_type = "igraph"),
               NA)
})


# test the grouping vector of length 1 and some as data
test_that("grouping vector of lenght 1 or same as data works", {
  adj_mats <- readRDS(test_path("fixtures", "adj_matrix_list.rds"))

  group_vec <- readRDS(test_path("fixtures", "group_vec_adj_matrix.rds"))

  networks <- VisualiseNetwork(adj_mats, group_vec = group_vec, vis_type = "xgmml")

  all_equal <- all(vapply(seq_along(networks$nodes),
         function (x) all(networks$nodes[[1]]$group == networks$nodes[[x]]$group),
         FUN.VALUE = T))

  expect_true(all_equal)

  grouping_list <- replicate(length(adj_mats), sample(group_vec), simplify = FALSE)

  networks <- VisualiseNetwork(adj_mats, group_vec = grouping_list, vis_type = "xgmml")

  expect_equal(lapply(seq_along(networks$nodes),
                      function (x) {
                        # resort to original order
                        tmp_idx <- networks$nodes[[x]]$node %>%
                          {match(colnames(adj_mats[[x]]), .)}
                        # Get original order group names
                        networks$nodes[[x]][tmp_idx,]$group}),
                      grouping_list)

})

# try grouping vec as list of size 1
