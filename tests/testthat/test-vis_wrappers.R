test_that("when cytoscape is not availble RCy3 will give error", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- rep("A", times = nrow(Mat1))
  group_vec[colnames(Mat1) %>% stringr::str_detect("IL")] <- "B"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CCL")] <- "C"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CXCL")] <- "D"

  # Visualize the network
  expect_error(VisualiseNetwork(Mat1, group_vec = group_vec, type = 1), "cyError")
})



