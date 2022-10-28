test_that("networks are converted into the expected format from graphNEL", {
  control <- adjToNetwork(sepsis[1:3],
               node_attrs = "none",
               edge_attrs = "none",
               output_as = "list")

  # convert tibbles to data frames for comparison
  control <- lapply(control, function (net) {
      net$edges <- as.data.frame(net$edges)
      return(net)
  })

  test_obj <- adjToNetwork(sepsis[1:3],
                           node_attrs = "none",
                           edge_attrs = "none",
                           output_as = "graphNEL")

  test_res <- dfsFromNet(test_obj)

  expect_equal(test_res, control)
})


test_that("networks are converted into the expected format from igraph", {
    control <- adjToNetwork(sepsis[1:3],
                            node_attrs = "none",
                            edge_attrs = "none",
                            output_as = "list")

    # convert tibbles to data frames for comparison
    control <- lapply(control, function (net) {
        net$edges <- as.data.frame(net$edges)
        return(net)
    })

    test_obj <- adjToNetwork(sepsis[1:3],
                             node_attrs = "none",
                             edge_attrs = "none",
                             output_as = "igraph")

    test_res <- dfsFromNet(test_obj)

    expect_equal(test_res, control)
})


test_that("single network is not converted into multi level list", {
    test_obj <- adjToNetwork(sepsis[[1]],
                            node_attrs = "none",
                            edge_attrs = "none",
                            output_as = "graphNEL")

    test_res <- dfsFromNet(test_obj)

    expect_true(is_network_list(test_res))
})
