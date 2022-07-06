#' Visualize with igraph
#'
#' More extensive description of what the function does
#'
#' When all three network input parameters are provided, `edge_table` and
#' `node_table` will be used instead of `igraph_obj`.
#' Used defaults in the for vertex and label styling, but these can be overwritten
#'   by providing arguments for igraph plot style, i.e. arguments starting
#'   with the "vertex." or "label.". See igraph plotting docs.
#'
#' @param radial_labs A logical to indicate whether vertex labels should be
#'   positioned radially around the circular arrangement of vertices.
#' @param radial_lab_opts A names list, in which the names are valid arguments for
#'   `text()`.
#' @param ... Additional options to be used with `igraph::plot.igraph` for
#'   visualizing your network. Any options provided here will overwrite the
#'   defaults. If `radial_labs` is `FALSE`, this argument can also be used to
#'   customize the vertex labels placed by `plot.igraph`.
#' @return The section on the returned values
vis_igraph <- function(edge_table = NULL, node_table = NULL,
                       igraph_obj = NULL,
                       radial_labs = T,
                       radial_labs_opts = list(),
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

  # Store additional user arguments as list
  plot_params <- list(...)

  # Get the values from additional user input or otherwise use the defaults
  node_arrangement <- get_param(graph, plot_params, "layout")
  edge_width <- get_param(graph, plot_params, "edge.width", "width")
  edge_color <- get_param(graph, plot_params, "edge.color", "color")
  vertex_color <- get_param(graph, plot_params, "vertex.color", "color")
  vertex_label0 <- get_param(graph, plot_params, "vertex.label", "name")

  # If vertex labels are to be placed radially, there should be none placed
  #   by plot.igraph.
  if (radial_labs) vertex_label <- "" else vertex_label <- vertex_label0


  # Visualize in igraph -----------------------------------------------------

  # Visualize the basic graph
  do.call(igraph::plot.igraph,
          c(list(x = graph,
            layout = node_arrangement,
            edge.width = edge_width * 4,
            edge.color = edge_color,
            vertex.color = vertex_color,
            vertex.label = vertex_label),
            plot_params
          )
  )

  # If vertex labels are to be added radially that will be done below
  if (radial_labs) {
    x = node_arrangement[,1]
    y = node_arrangement[,2]

    # create vector of angles for text based on number of nodes
    # (flipping the orientation of the words half way around so none appear upside down)
    angle = ifelse(atan(-(x/y))*(180/pi) < 0,
                   90 + atan(-(x/y))*(180/pi),
                   270 + atan(-x/y)*(180/pi))
    # formula from: https://gist.github.com/ajhmohr/5337a5c99b504e4a243fad96203fa74f

    # Create a text function that vectorizes srt argument
    text <- function(...) Map(graphics::text, ...)

    # TODO implement check to see whether radial_labs_opts is the correct list input
    # TODO this will throw an error if the user supplies one of the arguments that
    #   are already supplied
    do.call(text,
            c(list(x = x * 1.1,
                   y = y * 1.1,
                   labels = vertex_label0,
                   adj = ifelse(x<=0, 1, 0),
                   srt = angle,
                   xpd = T),
              radial_labs_opts))
  }

  invisible(NULL)
}

#' Use user parameters over default
#'
#' This is a helper function for `vis_igraph`. I makes sure that when there are
#' plotting arguments that have been provided by the user, these will be used
#' instead of the default found in the graph.
#'
#' @param graph Igraph object.
#' @param plot_params Names list with the graphical parameters supplied by the
#'   user.
#' @param plot_arg Character string. Will be used to modify visualization with
#'   `igraph::plot.igraph`.
#' @param name_in_df Character string. Some attributes are called different in the
#'   context of plotting vs in the object they are stored in (e.g. vertex.label
#'   vs name, respectively)
#' @return A vector of values that will be used for one of the arguments of
#'   `igraph::plot.igraph`. When the attribute is not in
get_param <- function(graph, plot_params, plot_arg, name_in_df = NULL) {
  # Prepare a vector that contains the attributes present in the data
  in_edge_attr <- function(attr) attr %in% igraph::edge_attr_names(graph)
  in_vertex_attr <- function(attr) attr %in% igraph::vertex_attr_names(graph)

  # Use user input if present
  if (plot_arg %in% names(plot_params)) {
    # Return value from user input
    plot_params[[plot_arg]]

    # If there is no user input for a variable, use data from graph
  } else {
    # Take the desired data from the graph
    if (plot_arg == "layout") {
      igraph::layout_in_circle(graph, order = order(igraph::V(graph)$group))

    } else if (stringr::str_starts(plot_arg, "vertex") & in_vertex_attr(name_in_df)) {
      igraph::vertex.attributes(graph)[[name_in_df]]

    } else if (stringr::str_starts(plot_arg, "edge") & in_edge_attr(name_in_df)) {
      igraph::edge.attributes(graph)[[name_in_df]]

    } else {
      NULL
    }
  }
}
