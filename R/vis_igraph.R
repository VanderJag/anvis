#' # library(igraph)
#' # library(testthat)
#'
#'
#' # Load data ---------------------------------------------------------------
#'
# Mat1 <- readRDS(testthat::test_path("fixtures", "trail_adjacency_matrix.rds"))
#
# # Some grouping based on column names
# group_vec <- rep("A", times = nrow(Mat1))
# group_vec[colnames(Mat1) |> stringr::str_detect("IL")] <- "B"
# group_vec[colnames(Mat1) |> stringr::str_detect("CCL")] <- "C"
# group_vec[colnames(Mat1) |> stringr::str_detect("CXCL")] <- "D"
#
#
# # Convert to edge and node table ------------------------------------------
#
# network_list <- adj_matrix_to_network(Mat1,
#                                       group_vec = group_vec,
#                                       width_type = 1)
# edge_table <- network_list[["edge_table"]]
# node_table <- network_list[["node_table"]]
#
#
# # Visualize in igraph -----------------------------------------------------
#
# my_graph <- igraph::graph_from_data_frame(edge_table, vertices=node_table, directed=FALSE)
#'
#' group_layout <- igraph::layout_in_circle(my_graph, order = order(igraph::V(my_graph)$group))
#'
#' # Basic graph
#' igraph::plot.igraph(my_graph,
#'      layout = group_layout)
#'
#' # Graph with widths and colors adjusted
#' igraph::plot.igraph(my_graph,
#'      layout = group_layout,
#'      vertex.color=igraph::V(my_graph)$color,
#'      edge.width=igraph::E(my_graph)$width,
#'      edge.color=igraph::E(my_graph)$color)
#'
#'
#'
#' ## The igraph docs say that vertex.label.degree controls the position
#' ## of the labels with respect to the vertices. It's interpreted as a
#' ## radian, like this:
#' ##
#' ## Value is : Label appears ... the node
#' ## -pi/2: above
#' ## 0: to the right of
#' ## pi/2: below
#' ## pi: to the left of
#' ##
#' ## We can generalize this. vertex.label.degree can take a vector as
#' ## well as a scalar for its argument. So we write a function to
#' ## calculate the right position for a label based on its vertex's location
#' ## on the circle.
#'
#' ## Get the labels aligned consistently around the edge of the circle
#' ## for any n of nodes.
#' ## This code borrows bits of ggplot2's polar_coord function
#' ## start = offset from 12 o'clock in radians
#' ## direction = 1 for clockwise; -1 for anti-clockwise.
#' radian.rescale <- function(x, start=0, direction=1) {
#'   # From https://gist.github.com/kjhealy/834774/a4e677401fd6e4c319135dabeaf9894393f9392c
#'   c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#'   c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
#' }
#'
#' lab.locs <- radian.rescale(x=1:igraph::vcount(my_graph), direction=-1, start=0)
#'
#' plot(my_graph,
#'      layout = group_layout,
#'      edge.width=E(my_graph)$width,
#'      edge.color=E(my_graph)$color,
#'      vertex.size=10,
#'      vertex.color=V(my_graph)$color,
#'      vertex.frame.color = V(my_graph)$color,
#'      vertex.label.degree=lab.locs,
#'      vertex.label.dist=1,
#'      vertex.label.cex=0.9,
#'      vertex.label.family="Helvetica",
#'      vertex.label.color = "black")
#'
#' # Change vertex label arrangement
#' vertex.attributes(my_graph)
#' tmp_circle <- tibble::tibble("x" = group_layout[,1],
#'                              "y" = group_layout[,2],
#'                              "name" = igraph::V(my_graph)$name,
#'                              "nr" = 1:length(igraph::V(my_graph)))
#'
#' # TODO extend documentation
#' #' Get vector for igraphs's `vertex.lable.degree` plotting parameter
#' #'
#' #' Move the vertex labels so they have less overlap with vertices and other
#' #' labels when a circular layout is used for plotting.
#' #' igraph plotting e.g. with `igraph::plot.igraph(...)` has a parameter
#' #' `vertex.lable.degree` that determines on which side relative to the vertex
#' #' the label will be placed. This function returns a vector of numbers relating
#' #' to label sides, such that the labels on the right will be place to the right
#' #' of the vertex, the label on the top of the circle will be place above the vertex,
#' #' etc.
#' #'
#' #' @param xy_mat Matrix or data frame, like returned from `igraph::layout_in_circle()`.
#' #'   The number of rows should match the number of labels that need to be placed.
#' #'   The first column should be x-coordinates, the second y-coordinates. Both
#' #'   x and y values are expected to range from -1 to 1.
#' #' @return The section on the returned values ...
#' #'
#' #' @examples
#' #'
#' match_labels_ring <- function(xy_mat) {
#'   if (!("x" %in% colnames(xy_mat) & "y" %in% colnames(xy_mat))) {
#'     # igraph function may return matrix with coordinates without names
#'     colnames(xy_mat) <- c("x", "y")
#'   }
#'
#'   xy_mat <- xy_mat %>%
#'     tibble::as_tibble() %>%
#'     dplyr::mutate(label_loc = dplyr::case_when(
#'       # right side
#'       y > -0.8 & y < 0.8 & x > 0 ~ 0,
#'       # left side
#'       y > -0.8 & y < 0.8 & x < 0 ~ pi,
#'       # top
#'       y >= 0.8 ~ -pi/2,
#'       # bottom
#'       y <= -0.8 ~ pi/2,
#'       # anything else (there shouldn't be anything)
#'       TRUE ~ 0
#'     ))
#'
#'   return(xy_mat %>% dplyr::pull(label_loc))
#' }
#'
#'
#' plot(my_graph,
#'      layout = group_layout,
#'      edge.width = E(my_graph)$width,
#'      edge.color = E(my_graph)$color,
#'      vertex.size = 10,
#'      vertex.color = V(my_graph)$color,
#'      vertex.frame.color = V(my_graph)$color,
#'      vertex.label.degree = match_labels_ring(group_layout),
#'      vertex.label.dist = 1,
#'      vertex.label.cex = 0.9,
#'      vertex.label.family = "Helvetica",
#'      vertex.label.color = "black")
#'
#' # TODO extend documentation
#' #' Label vertex distance calculation for circular network layout
#' #'
#' #' Calculate distances that try to prevent overlap of vertex labels and vertices
#' #' or other labels.
#' #'
#' #' @param labels Character vector of the names of the vertices
#' #' @return The section on the returned values ...
#' #'
#' #' @examples
#' #'
#' #' @inheritParams match_labels_ring
#' distance_ring_labels <- function(labels, xy_mat) {
#'   # Check which sides of the circle the labels are on
#'   side <- match_labels_ring(xy_mat = xy_mat)
#'
#'   # t1 <- sapply(as.vector(node_labels), FUN = strwidth, units='in')
#'   # t2 <- (t1 - min(t1))/ (max(t1) - min(t1))*3+0.9
#'
#'   labs_df <- tibble::tibble("label" = labels,
#'                             "side" = side,
#'                             "x" = xy_mat[,1],
#'                             "y" = xy_mat[,2]) %>%
#'     dplyr::mutate(distance = dplyr::case_when(
#'       # right side
#'       side == 0 ~ strwidth(label, family = "sans", units = "in") %>%
#'         {(. - min(.))/ (max(.) - min(.))*3+0.9} * 1.4 + 1,
#'       # left side
#'       side == pi ~ strwidth(label, family = "sans", units = "in") %>%
#'         {(. - min(.))/ (max(.) - min(.))*3+0.9} * 1.4 + 1,
#'       # anything else
#'       TRUE ~ 0
#'     ))
#'
#'   count_up_down <- function(numbers) {
#'     l <- length(numbers)
#'     if (l %% 2 == 0) {
#'       v <- c(1:(l/2), (l/2):1)
#'     } else if (l %% 2 == 1) {
#'       v <- c(1:ceiling(l/2), floor(l/2):1)
#'     }
#'
#'     v
#'   }
#'
#'   labs_df_top <- labs_df %>%
#'     dplyr::filter(side == -pi/2) %>%
#'     dplyr::arrange(x) %>%
#'     dplyr::mutate(distance = (count_up_down(label)**2.15 / nrow(.) + 1))
#'
#'   labs_df_bottom <- labs_df %>%
#'     dplyr::filter(side == pi/2) %>%
#'     dplyr::arrange(x) %>%
#'     dplyr::mutate(distance = count_up_down(label)**2.15 / nrow(.) + 1)
#'   tb_labs <- dplyr::bind_rows(labs_df_top, labs_df_bottom) %>%
#'     tibble::column_to_rownames("label")
#'
#'   for (label_i in rownames(tb_labs)) {
#'     labs_df <- dplyr::mutate(labs_df, distance =
#'                         ifelse(label == label_i,
#'                                tb_labs[label_i,]$distance,
#'                                distance))
#'   }
#'
#'   return(labs_df %>% dplyr::pull(distance))
#' }
#'
#'
#' plot(my_graph,
#'      layout = group_layout,
#'      edge.width = igraph::E(my_graph)$width  * 1.25,
#'      edge.color = igraph::E(my_graph)$color,
#'      vertex.size = 12,
#'      vertex.color = igraph::V(my_graph)$color,
#'      vertex.frame.color = igraph::V(my_graph)$color,
#'      vertex.label.degree = match_labels_ring(group_layout),
#'      vertex.label.dist = distance_ring_labels(igraph::V(my_graph)$name, group_layout),
#'      vertex.label.cex = 0.9,
#'      vertex.label.family = "sans",
#'      vertex.label.color = "black"
#'      )
#'
#'
#' plot(my_graph,
#'      layout = group_layout,
#'      edge.width = igraph::E(my_graph)$width  * 1.25,
#'      edge.color = igraph::E(my_graph)$color,
#'      vertex.size = 12,
#'      vertex.color = igraph::V(my_graph)$color,
#'      vertex.frame.color = igraph::V(my_graph)$color,
#'      vertex.label.cex = 0.9,
#'      vertex.label.family = "sans",
#'      vertex.label.color = "black",
#'      vertex.label = ""
#'      )
#'
#'
#' ## Apply labels manually
#' #Specify x and y coordinates of labels, adjust outward as desired
#' x = group_layout[,1] * 1.1
#' y = group_layout[,2] * 1.1
#'
#' # formula from:
#' # https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f
#' # create vector of angles for text based on number of nodes
#' # (flipping the orientation of the words half way around so none appear upside down)
#' angle = ifelse(atan(-(group_layout[,1]/group_layout[,2]))*(180/pi) < 0,
#'                90 + atan(-(group_layout[,1]/group_layout[,2]))*(180/pi),
#'                270 + atan(-group_layout[,1]/group_layout[,2])*(180/pi))
#'
#' #Apply the text labels with a loop with angle as srt
#' for (i in 1:length(x)) {
#'   text(x=x[i],
#'        y=y[i],
#'        labels=igraph::V(my_graph)$name[i],
#'        adj=ifelse(x[i]<=0, 1, 0),
#'        pos=NULL,
#'        cex=.8,
#'        col="black",
#'        srt=angle[i],
#'        xpd=T)
#' }
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
#' #' @param yes_no A logical scalar. Should missing values be ...
#' #' @return The section on the returned values
vis_igraph <- function(edge_table = NULL, node_table = NULL,
                       igraph_obj = NULL,
                       rot_labs = T,
                       ...) {

  # Validate network parameters ---------------------------------------------

  # error for when all network input is missing
  if (is.null(edge_table) & is.null(node_table) & is.null(igraph_obj)) {
    stop("Must provide either edge and node table or igraph object",
         "\nℹ edge_table, node_table and igraph_obj parameters are NULL", call.=FALSE)
  }

  # Error when network table input is incomplete
  if (!is.null(edge_table) & is.null(node_table) |
      is.null(edge_table) & !is.null(node_table)) {
    stop("Must provide both edge and node table",
         "\nℹ ", ifelse(is.null(edge_table),
                        "edge table parameter is NULL",
                        "node table parameter is NULL"),
         call.=FALSE)
  }

  # Prepare igraph graph for visualization
  if (!is.null(edge_table) & !is.null(node_table)) {
    graph <- igraph::graph_from_data_frame(edge_table,
                                           vertices = node_table,
                                           directed = FALSE)
  } else {
    if (!igraph::is_igraph(igraph_obj)) {
      stop("`igraph_obj` parameter must be of class igraph:",
      "\nℹ Your input is class: ", class(igraph_obj),
      ".\n✖ igraph::is_igraph(igraph_obj) must return TRUE.", call.=FALSE)
    }
    graph <- igraph_obj
  }

  # Capture input -----------------------------------------------------------

  plot_params <- list(...)

  # If parameter is found in user input use that one, otherwise use the default
  get_param <- function(list_item, name_in_df = NULL) {
    # Use user input if present
    if (list_item %in% names(plot_params)) {
      # Record value from user input
      value <- plot_params[[list_item]]
      # Since we use the value now it does not need to be passed to more functions
      #   as additional argument
      plot_params[[list_item]] <- NULL
      # Return value
      return(value)

    # If there is no user input for a variable, use data from graph
    } else {
      # Take the desired data from the graph
      if (stringr::str_starts(list_item, "vertex")) {
        igraph::vertex.attributes(graph)[[name_in_df]]
      } else if (stringr::str_starts(list_item, "edge")) {
        igraph::edge.attributes(graph)[[name_in_df]]
      } else if (list_item == "layout") {
        igraph::layout_in_circle(graph, order = order(igraph::V(graph)$group))
      }
    }
  }

    # TODO what if the graph does not have the correct attributes for default visualization
  node_arrangement <- get_param("layout")
  edge_width <- get_param("edge.width", "width")
  edge_color <- get_param("edge.color", "color")
  vertex_color <- get_param("vertex.color", "color")
  vertex_label0 <- get_param("vertex.label", "name")

  # TODO if rot_labs then vertex_label should be ""
  if (rot_labs) vertex_label <- "" else vertex_label <- vertex_label0


  # Visualize in igraph -----------------------------------------------------

  do.call(igraph::plot.igraph,
          c(list(x = graph,
            layout = node_arrangement,
            edge.width = edge_width,
            edge.color = edge_color,
            vertex.color = vertex_color,
            vertex.label = vertex_label),
            plot_params
          )
  )


  if (rot_labs) {
    ## Apply labels manually
    #Specify x and y coordinates of labels, adjust outward as desired
    x = node_arrangement[,1] * 1.1
    y = node_arrangement[,2] * 1.1

    # formula from:
    # https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f
    # create vector of angles for text based on number of nodes
    # (flipping the orientation of the words half way around so none appear upside down)
    angle = ifelse(atan(-(node_arrangement[,1]/node_arrangement[,2]))*(180/pi) < 0,
                   90 + atan(-(node_arrangement[,1]/node_arrangement[,2]))*(180/pi),
                   270 + atan(-node_arrangement[,1]/node_arrangement[,2])*(180/pi))

    #Apply the text labels with a loop with angle as srt
    for (i in 1:length(x)) {
      do.call(text,
              c(list(x=x[i],
                y=y[i],
                labels=vertex_label0[i],
                adj=ifelse(x[i]<=0, 1, 0),
                col="black",
                srt=angle[i],
                xpd=T),
                plot_params
              ))
    }
  }
  # TODO deal with parameters that can't be used in the text() functions

  invisible(NULL)
}

vis_igraph(edge_table, node_table, rot_labs = T, cex = 0.8, vertex.color = "black", param_t = "t")
