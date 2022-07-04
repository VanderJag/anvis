#' #' # library(igraph)
#' #' # library(testthat)
#' #'
#' #'
#' #' # Load data ---------------------------------------------------------------
#' #'
#' Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
#'
#' # Some grouping based on column names
#' group_vec <- rep("A", times = nrow(Mat1))
#' group_vec[colnames(Mat1) |> stringr::str_detect("IL")] <- "B"
#' group_vec[colnames(Mat1) |> stringr::str_detect("CCL")] <- "C"
#' group_vec[colnames(Mat1) |> stringr::str_detect("CXCL")] <- "D"
#'
#'
#' # Convert to edge and node table ------------------------------------------
#'
#' network_list <- adj_matrix_to_network(Mat1,
#'                                       group_vec = group_vec,
#'                                       width_type = 1)
#' edge_table <- network_list[["edge_table"]]
#' node_table <- network_list[["node_table"]]
#'
#'
#' # Visualize in igraph -----------------------------------------------------
#'
#' my_graph <- igraph::graph_from_data_frame(edge_table, vertices=node_table, directed=FALSE)
#'
#' group_layout <- igraph::layout_in_circle(my_graph, order = order(igraph::V(my_graph)$group))
#'
#'
#'
#'
#'
#'
#' #' Visualize with igraph
#' #'
#' #' More extensive description of what the function does
#' #'
#' #' When all three network input parameters are provided, `edge_table` and
#' #' `node_table` will be used instead of `igraph_obj`.
#' #' Used defaults in the for vertex and label styling, but these can be overwritten
#' #'   by providing arguments for igraph plot style, i.e. arguments starting
#' #'   with the "vertex." or "label.". See igraph plotting docs.
#' #'
#' #' @param rot_labs A logical to indicate whether vertex labels should be
#' #'   rotated around the circular arrangement of vertices.
#' #' @param rot_lab_opts A names list, in which the names are valid arguments for
#' #'   `text()`.
#' #' @return The section on the returned values
#' vis_igraph <- function(edge_table = NULL, node_table = NULL,
#'                        igraph_obj = NULL,
#'                        rot_labs = T,
#'                        rot_labs_opts = list(),
#'                        ...) {
#'
#'   # Validate network parameters ---------------------------------------------
#'
#'   # error for when all network input is missing
#'   if (is.null(edge_table) & is.null(node_table) & is.null(igraph_obj)) {
#'     stop("Must provide either edge and node table or igraph object",
#'          "\nℹ edge_table, node_table and igraph_obj parameters are NULL", call.=FALSE)
#'   }
#'
#'   # Error when network table input is incomplete
#'   if (!is.null(edge_table) & is.null(node_table) |
#'       is.null(edge_table) & !is.null(node_table)) {
#'     stop("Must provide both edge and node table",
#'          "\nℹ ", ifelse(is.null(edge_table),
#'                         "edge table parameter is NULL",
#'                         "node table parameter is NULL"),
#'          call.=FALSE)
#'   }
#'
#'   # Prepare igraph graph for visualization
#'   if (!is.null(edge_table) & !is.null(node_table)) {
#'     graph <- igraph::graph_from_data_frame(edge_table,
#'                                            vertices = node_table,
#'                                            directed = FALSE)
#'   } else {
#'     if (!igraph::is_igraph(igraph_obj)) {
#'       stop("`igraph_obj` parameter must be of class igraph:",
#'       "\nℹ Your input is class: ", class(igraph_obj),
#'       ".\n✖ igraph::is_igraph(igraph_obj) must return TRUE.", call.=FALSE)
#'     }
#'     graph <- igraph_obj
#'   }
#'
#'   # Capture input -----------------------------------------------------------
#'
#'   plot_params <- list(...)
#'
#'   # If parameter is found in user input use that one, otherwise use the default
#'   get_param <- function(list_item, name_in_df = NULL) {
#'     # Use user input if present
#'     if (list_item %in% names(plot_params)) {
#'       # Record value from user input
#'       value <- plot_params[[list_item]]
#'       # Since we use the value now it does not need to be passed to more functions
#'       #   as additional argument
#'       plot_params[[list_item]] <- NULL
#'       # Return value
#'       return(value)
#'
#'     # If there is no user input for a variable, use data from graph
#'     } else {
#'       # Take the desired data from the graph
#'       if (stringr::str_starts(list_item, "vertex")) {
#'         igraph::vertex.attributes(graph)[[name_in_df]]
#'       } else if (stringr::str_starts(list_item, "edge")) {
#'         igraph::edge.attributes(graph)[[name_in_df]]
#'       } else if (list_item == "layout") {
#'         igraph::layout_in_circle(graph, order = order(igraph::V(graph)$group))
#'       }
#'     }
#'   }
#'
#'     # TODO what if the graph does not have the correct attributes for default visualization
#'   node_arrangement <- get_param("layout")
#'   edge_width <- get_param("edge.width", "width")
#'   edge_color <- get_param("edge.color", "color")
#'   vertex_color <- get_param("vertex.color", "color")
#'   vertex_label0 <- get_param("vertex.label", "name")
#'
#'   # TODO if rot_labs then vertex_label should be ""
#'   if (rot_labs) vertex_label <- "" else vertex_label <- vertex_label0
#'
#'
#'   # Visualize in igraph -----------------------------------------------------
#'
#'   do.call(igraph::plot.igraph,
#'           c(list(x = graph,
#'             layout = node_arrangement,
#'             edge.width = edge_width,
#'             edge.color = edge_color,
#'             vertex.color = vertex_color,
#'             vertex.label = vertex_label),
#'             plot_params
#'           )
#'   )
#'
#'
#'   if (rot_labs) {
#'     ## Apply labels manually
#'     #Specify x and y coordinates of labels, adjust outward as desired
#'     x = node_arrangement[,1] * 1.1
#'     y = node_arrangement[,2] * 1.1
#'
#'     # formula from:
#'     # https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f
#'     # create vector of angles for text based on number of nodes
#'     # (flipping the orientation of the words half way around so none appear upside down)
#'     angle = ifelse(atan(-(node_arrangement[,1]/node_arrangement[,2]))*(180/pi) < 0,
#'                    90 + atan(-(node_arrangement[,1]/node_arrangement[,2]))*(180/pi),
#'                    270 + atan(-node_arrangement[,1]/node_arrangement[,2])*(180/pi))
#'
#'
#'     # Create a text function that vectorizes srt argument
#'     text <- function(...) Map(graphics::text, ...)
#'
#'     do.call(text,
#'             c(list(x=x,
#'                    y=y,
#'                    labels=vertex_label0,
#'                    adj=ifelse(x<=0, 1, 0),
#'                    col="black",
#'                    srt=angle,
#'                    xpd=T),
#'               rot_labs_opts)
#'     )
#'   }
#'
#'
#'   invisible(NULL)
#' }
#'
#' vis_igraph(edge_table, node_table, rot_labs = T, rot_labs_opts = list(cex = 1.2), vertex.color = c(rep("black", 18), rep("yellow", 18)))
#' # TODO implement check to see whether rot_labs_opts is the correct list input
