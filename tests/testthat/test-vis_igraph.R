test_that("igraph visualization throws error when no network input is provided", {
  expect_error(vis_igraph(edge_table = NULL,
                          node_table = NULL,
                          igraph_obj = NULL), "provide either edge and node table or igraph object")
})

test_that("igraph visualization throws error when only edge_table is provided", {
  expect_error(vis_igraph(edge_table = T,
                          node_table = NULL,
                          igraph_obj = NULL), "node table parameter is NULL")
})

test_that("igraph visualization throws error when only node_table is provided", {
  expect_error(vis_igraph(edge_table = NULL,
                          node_table = T,
                          igraph_obj = NULL), "edge table parameter is NULL")
})

test_that("igraph_obj input must be of class igraph", {
  graph_obj <- matrix(1:9, 3, 3)
  class(graph_obj) <- "other_class"

  expect_error(vis_igraph(edge_table = NULL,
                          node_table = NULL,
                          igraph_obj = graph_obj), "must be of class igraph")
})


