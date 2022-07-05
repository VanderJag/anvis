test_that("edge width works without error for all types", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)

  expect_error(edge_weight_to_widths(edge_table = edge_table, 1), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, 2), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, 3), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, 4), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, 5), NA)
})

