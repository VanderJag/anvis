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
#
#   g2 <- graph.data.frame(topology, vertices=answers, directed=FALSE)
#   g <- simplify(g2)
# }
#
#
# network_list <- adj_matrix_to_network(Mat1,
#                                       group_vec = group_vec,
#                                       width_type = 1)
# edge_table <- network_list[["edge_table"]]
# node_table <- network_list[["node_table"]]
#
# # my_graph <- graph_from_adjacency_matrix(Mat1,
# #                                         mode = "undirected",
# #                                         weighted = TRUE,
# #                                         diag = FALSE)
# my_graph <- graph_from_data_frame(edge_table, vertices=node_table, directed=FALSE)
#
# group_layout <- layout_in_circle(my_graph, order = order(V(my_graph)$Groups))
# plot(my_graph,
#      layout = group_layout,
#      vertex.color=V(my_graph)$color,
#      edge.width=E(my_graph)$width,
#      edge.color=E(my_graph)$Stroke,
#      vertex.label.dist=1)
#
# ## The igraph docs say that vertex.label.degree controls the position
# ## of the labels with respect to the vertices. It's interpreted as a
# ## radian, like this:
# ##
# ## Value is : Label appears ... the node
# ## -pi/2: above
# ## 0: to the right of
# ## pi/2: below
# ## pi: to the left of
# ##
# ## We can generalize this. vertex.label.degree can take a vector as
# ## well as a scalar for its argument. So we write a function to
# ## calculate the right position for a label based on its vertex's location
# ## on the circle.
#
# ## Get the labels aligned consistently around the edge of the circle
# ## for any n of nodes.
# ## This code borrows bits of ggplot2's polar_coord function
# ## start = offset from 12 o'clock in radians
# ## direction = 1 for clockwise; -1 for anti-clockwise.
# radian.rescale <- function(x, start=0, direction=1) {
#   # From https://gist.github.com/kjhealy/834774/a4e677401fd6e4c319135dabeaf9894393f9392c
#   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
# }
#
# lab.locs <- radian.rescale(x=1:vcount(my_graph), direction=-1, start=0)
#
# # Because the labels have no justification
# V(my_graph)$name <- V(my_graph)$name %>%
#   stringr::str_pad(width = max(nchar(.)), side = "left")
# # Because the labels have no justification
# # V(my_graph)$name <- "A"
#
#
# plot(my_graph,
#      layout = group_layout,
#      edge.width=E(my_graph)$width,
#      edge.color=E(my_graph)$Stroke,
#      vertex.size=8,
#      vertex.color=V(my_graph)$color,
#      vertex.frame.color = V(my_graph)$color,
#      vertex.label.degree=lab.locs,
#      vertex.label.dist=rep(c(0,3), each=18),
#      vertex.label.cex=0.9,
#      vertex.label.family="Helvetica",
#      vertex.label.color = "black")
#
#
#
# V(my_graph)$name
#
# radian.rescale <- function(x, start=0, direction=1) {
#   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
# }
#
# ### Example
# ## Generate some fake data
# n <- 15
# g <- erdos.renyi.game(n, 0.5)
# ## Obviously labeling in this way this only makes sense for graphs
# ## laid out as a circle to begin with
# la <- layout.circle(g)
#
# lab.locs <- radian.rescale(x=1:n, direction=-1, start=0)
# plot(g, layout=la, vertex.size=2, vertex.label.dist=1,
#      vertex.label.degree=lab.locs)
#
