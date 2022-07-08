test_that("when cytoscape is not available RCy3 will give error", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))

  # Some grouping based on column names
  group_vec <- rep("A", times = nrow(Mat1))
  group_vec[colnames(Mat1) %>% stringr::str_detect("IL")] <- "B"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CCL")] <- "C"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CXCL")] <- "D"

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
  group_vec <- rep("A", times = nrow(Mat1))
  group_vec[colnames(Mat1) %>% stringr::str_detect("IL")] <- "B"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CCL")] <- "C"
  group_vec[colnames(Mat1) %>% stringr::str_detect("CXCL")] <- "D"

  # Presence of cytoscape test
  cytosc <- RCy3::cytoscapePing() %>% capture_condition()

  skip_if_not(cytosc$message == "You are connected to Cytoscape!\n",
              message = "this test runs only when cytoscape is active")

  # Visualize the network
  expect_error(VisualiseNetwork(Mat1, group_vec = group_vec, vis_type = "cyto"), NA)
})
