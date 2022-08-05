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
#' @param rad_lab_opts A names list, in which the names are valid arguments for
#'   `text()`.
#' @param ... Additional options to be used with `igraph::plot.igraph` for
#'   visualizing your network. Any options provided here will overwrite the
#'   defaults. If `radial_labs` is `FALSE`, this argument can also be used to
#'   customize the vertex labels placed by `plot.igraph`.
#' @return The section on the returned values
vis_igraph <- function(edge_table = NULL, node_table = NULL,
                       igraph_obj = NULL,
                       radial_labs = T,
                       rad_lab_opts = list(),
                       scale_width = 3.25,
                       save_name = "network",
                       export_type = c("png", "print", "pdf", "svg", "jpeg", "tiff",
                                     "bmp", "ps"),
                       export_opts = list(),
                       par_opts = list(),
                       ...) {
  # TODO add to documentation how par is used with the graphical devices and reset afterwards

  export_type <- match.arg(export_type)

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
  edge_width <- plot_params[["edge.width"]] %||%
      igraph::edge.attributes(graph)[["width"]]
  edge_color <- plot_params[["edge.color"]] %||%
      igraph::edge.attributes(graph)[["color"]]
  vertex_color <- plot_params[["vertex.color"]] %||%
      igraph::vertex.attributes(graph)[["color"]]
  vertex_size <- plot_params[["vertex.size"]] %||%
      igraph::vertex.attributes(graph)[["size"]]
  vertex_label0 <- plot_params[["vertex.label"]] %||%
      igraph::vertex.attributes(graph)[["name"]]
  # If group info is present order based on group, otherwise keep node order
  if ("group" %in% igraph::vertex_attr_names(graph)) {
      layout_ord <- order(igraph::V(graph)$group)
  } else {
      layout_ord <- igraph::V(graph)
  }

  node_arrangement <- plot_params[["layout"]] %||%
      igraph::layout_in_circle(graph, order = layout_ord)

  # If vertex labels are to be placed radially, there should be none placed
  #   by plot.igraph.
  if (radial_labs) vertex_label <- "" else vertex_label <- vertex_label0


  # Visualize in igraph -----------------------------------------------------

  # Select a graphics device to save output
  start_saving(export_type, export_opts, save_name)
  if (export_type != "print") on.exit(if (dev.cur() > 1) dev.off())

  # Change graphical parameters here, so they will affect the newly active device
  if (length(par_opts) != 0) old_par <- do.call(par, par_opts)

  # Visualize the basic graph
  do.call(igraph::plot.igraph,
          c(list(x = graph,
            layout = node_arrangement,
            edge.width = edge_width * scale_width,
            edge.color = edge_color,
            vertex.size = vertex_size,
            vertex.color = vertex_color,
            vertex.label = vertex_label),
            plot_params
          )
  )

  # If vertex labels are to be added radially that will be done below
  if (radial_labs) {
    x = node_arrangement[,1]
    y = node_arrangement[,2]

    # Check if user wants to overwrite one of the default arguments
    txt_x <- if ("x" %in% names(rad_lab_opts)) rad_lab_opts[["x"]] else x * 1.1
    txt_y <- if ("y" %in% names(rad_lab_opts)) rad_lab_opts[["y"]] else y * 1.1
    txt_labels <- if ("labels" %in% names(rad_lab_opts)) rad_lab_opts[["labels"]] else vertex_label0
    txt_adj <- if ("adj" %in% names(rad_lab_opts)) rad_lab_opts[["adj"]] else ifelse(x<=0, 1, 0)
    txt_cex <- if ("cex" %in% names(rad_lab_opts)) rad_lab_opts[["cex"]] else 0.85
    # Remove the used arguments from list as to pass them twice, prevents error
    rad_lab_opts[c("x","y","labels","adj","cex")] <- NULL

    angle = radial_angle(x, y)

    # Create a text function that vectorizes srt argument
    text <- function(...) Map(graphics::text, ...)

    do.call(text,
            c(list(x = txt_x,
                   y = txt_y,
                   labels = txt_labels,
                   adj = txt_adj,
                   cex = txt_cex,
                   srt = angle,
                   xpd = T),
              rad_lab_opts))
  }

  # reset graphical parameters to original option
  if (length(par_opts) != 0) par(old_par)

  # Finish saving
  if (export_type != "print") {
    dev.off()
  }

  invisible(NULL)
}


start_saving <- function(export_type, export_opts, save_name) {
  if (export_type != "print") {
    save_funcs <- list("png" = png, "pdf" = pdf, "svg" = svg, "jpeg" = jpeg,
                       "tiff" = tiff, "bmp" = bmp, "ps" = postscript)
    save_dev <- save_funcs[[export_type]]

    # The default ('png') creates very low resolution images, fix this
    if (export_type == "png") {
      export_opts[["res"]] <- export_opts[["res"]] %||% 300
      export_opts[["width"]] <- export_opts[["width"]] %||% 2400
      export_opts[["height"]] <- export_opts[["height"]] %||% 2400
    }

    # Set the save name
    save_name <- file_sequence(save_name, paste0(".", export_type))
    save_name <- paste0(save_name, ".", export_type)
    if (export_type %in% c("bmp", "jpeg", "tiff", "png", "svg")) {
      export_opts[["filename"]] <- save_name
    } else if (export_type %in% c("pdf", "ps")) {
      export_opts[["file"]] <- save_name
    }

    # Start graphics device
    do.call(save_dev,
            export_opts)
  }
  invisible(NULL)
}
