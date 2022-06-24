# library(igraph)
# library(testthat)
#
# Mat1 <- readRDS(test_path("fixtures", "trail_adjacency_matrix.rds"))
#
# # Some grouping based on column names
# group_vec <- rep("A", times = nrow(Mat1))
# group_vec[colnames(Mat1) |> stringr::str_detect("IL")] <- "B"
# group_vec[colnames(Mat1) |> stringr::str_detect("CCL")] <- "C"
# group_vec[colnames(Mat1) |> stringr::str_detect("CXCL")] <- "D"
#
#
#
#
# vis_in_igraph <- function(edge_table, node_table) {
#
# }
#
# my_graph <- graph_from_adjacency_matrix(Mat1,
#                                         mode = "undirected",
#                                         weighted = TRUE,
#                                         diag = FALSE)
# group_layout <- layout_in_circle(my_graph, order = order(group_vec))
# V(my_graph)$color <-
# plot(my_graph, layout = group_layout)
#
#
# lookup_table <- data.frame()
#
# dat <- dat%>%left_join(lookup_table)
#
# colnames(Mat1)[order(group_vec)]

