# library(igraph)
#
# Mat1 <- readRDS("tests/trail_adjacency_matrix.rds")
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
#   my_graph <- graph_from_adjacency_matrix(Mat1,
#                                           mode = "undirected",
#                                           weighted = TRUE,
#                                           diag = FALSE)
#   plot(my_graph, layout=layout_in_circle)
# }
