test_that("edge width works without error for all types", {
  # Load adjacency matrix
  Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
  # Create edge table
  edge_table <- adj_matrix_to_edgelist(Mat1)

  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "cor"), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "partcor"), NA)
  expect_error(edge_weight_to_widths(edge_table = edge_table, width_type = "MI"), NA)
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
  expect_true("width" %in% colnames("MI" %>% add_edge_widths))
  expect_true("width" %in% colnames("ranked" %>% add_edge_widths))
  expect_true("width" %in% colnames("percentile" %>% add_edge_widths))
})


test_that("Warning is shown when range doesn't match partcor or cor", {
  # Create artificial edgelist with column 'weight'
  df <- tibble::tibble("weight" = seq(0,3.6, length.out = 40))

  expect_warning(edge_weight_to_widths(df, "partcor"), "ideal range")
  expect_warning(edge_weight_to_widths(df, "cor"), "ideal range")

  df <- tibble::tibble("weight" = seq(-5, 0, length.out = 40))

  expect_warning(edge_weight_to_widths(df, "partcor"), "ideal range")
  expect_warning(edge_weight_to_widths(df, "cor"), "ideal range")
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
  expect_equal("MI" %>% test_prep, ordered)
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
  expect_true(range("MI" %>% get_edge_widths)[1] >= 0)
  expect_true(range("MI" %>% get_edge_widths)[2] <= 1)
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
  expect_equal(edge_weight_to_widths(edge_table, "MI") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
  expect_equal(edge_weight_to_widths(edge_table, "ranked") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
  expect_equal(edge_weight_to_widths(edge_table, "percentile") %>% {paste(.$source, .$target)},
               edge_table %>% {paste(.$source, .$target)})
})

