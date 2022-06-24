test_that("when cytoscape is not available RCy3 will give error", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- rep("A", times = nrow(Mat1))
  group_vec[colnames(Mat1) %>% stringr::str_detect("IL")] <- "B"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CCL")] <- "C"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CXCL")] <- "D"

  # Presence of cytoscape test
  cytosc <- evaluate_promise(RCy3::cytoscapePing())

  skip_if(cytosc$messages == "You are connected to Cytoscape!\n")

  # Visualize the network
  expect_message(VisualiseNetwork(Mat1, group_vec = group_vec, type = 1),
                 "Please check that Cytoscape is running") %>%
  expect_error("object 'res' not found") %>%
  expect_error("object 'res' not found")
})

test_that("when cytoscape is available wrapper runs without error", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- rep("A", times = nrow(Mat1))
  group_vec[colnames(Mat1) %>% stringr::str_detect("IL")] <- "B"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CCL")] <- "C"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CXCL")] <- "D"

  # Presence of cytoscape test
  cytosc <- evaluate_promise(RCy3::cytoscapePing())

  skip_if_not(cytosc$messages == "You are connected to Cytoscape!\n")

  # Visualize the network
  expect_error(VisualiseNetwork(Mat1, group_vec = group_vec, type = 1), NA)
})
