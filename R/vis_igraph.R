#' Visualize with igraph
#'
#' This function takes a network object and visualizes it visualization.
#' Depending on the additional attributes that are present in the
#' input network, nodes and edges will be visually styled.
#'
#' This function uses defaults for visual styling, based network attributes with
#' fixed names such as 'color'. The used defaults can be overwritten by supplying
#' additional arguments to this function that are specified in
#' [igraph::igraph.plotting].
#'
#' @param network A network object of one of the valid classes:
#'     "igraph", "graphNEL", and "list" of two data frames with the names "edges"
#'     and "vertices". If the edges in the network have the additional attributes
#'     'width' or 'color' those attributes will be used to style the edges in
#'     the visualization. Optional node attribute 'group' will be used to arrange
#'     vertices so those with the same group label will be placed next to each
#'     other, 'color' and 'size' will be used to adjust the corresponding
#'     features of the vertices.
#' @param directed A logical (automatically determined for network objects of
#'     class 'igraph' and 'graphNEL', otherwise default `FALSE`).
#'     When this argument is `TRUE` edges will be
#'     drawn with a curvature and they will be shown as arrows.
#' @param radial_labs A logical (default `TRUE`) to indicate whether vertex labels
#'     should be positioned radially around the circular arrangement of vertices.
#'     If `FALSE`, they will be placed on top of the vertices (as is the default
#'     for igraph). When this argument is `FALSE`, use the options described
#'     in [igraph::igraph.plotting] as additional arguments for this function
#'     to adjust label styling.
#' @param rad_lab_opts A named list, in which the names are valid arguments for
#'     [text]. These styling options apply to vertex labels when `radial_lab`
#'     is `TRUE.`
#' @param scale_width Numeric, a number that will be multiplied with the edge
#'     widths, scaling the edge widths linearly.
#' @param save_name A character string that will be used as the base file name
#'     when saving the visualization. Don't include the file extension, as this
#'     will be added automatically based on the `export_type`. Numbers will be
#'     appended if a file with the same name and extension already exists in the
#'     directory used for saving.
#' @param export_type Character string, one of the following graphical devices:
#'     'png' (default), 'print' (instead of saving, show the plot in your R
#'     session), 'pdf', 'svg', 'jpeg', 'tiff', 'bmp', 'ps'.
#' @param export_opts A list with named elements. The list items will be used
#'     as arguments for the graphical device selected with `export_type`. Check
#'     which options are available for your graphical device with e.g. [png].
#'     any file name in this list will be overwritten by `save_name`.
#' @param par_opts A list with named elements. The list items will be used
#'     as arguments for [par]. Changes to the graphical parameters will be
#'     applied to your selected graphical device before making the visualization.
#'     After the visualization is completed, the graphical parameters will be
#'     reset to their original value.
#' @param ... Additional options to be used with [igraph::plot.igraph] for
#'     visualizing your network. Any options provided here will overwrite the
#'     defaults. If `radial_labs` is `FALSE`, this argument can also be used to
#'     customize the vertex labels placed by `plot.igraph`.
#'
#' @return Returns `NULL` invisibly. This functions creates visualizations and
#'     will show them directly in your R session or save them to the selected
#'     graphical device.
#'
#' @export
visIgraph <- function(network,
                      directed = if (is(network, "graphNEL")) {
                          graph::edgemode(network) == "directed"
                      } else if (is(network, "igraph")) {
                          igraph::is_directed(network)
                      } else {
                          FALSE
                      },
                      radial_labs = TRUE,
                      rad_lab_opts = list(),
                      scale_width = 3.25,
                      save_name = "network",
                      export_type = c("print", "png", "pdf", "svg", "jpeg",
                                      "tiff", "bmp", "ps"),
                      export_opts = list(),
                      par_opts = list(),
                      ...) {
    # input validation
    named_list_check(rad_lab_opts)
    named_list_check(export_opts)
    named_list_check(par_opts)

    export_type <- match.arg(export_type)

    if (length(save_name) != 1) {
        stop("Must provide a single save name:",
             "\nℹ For your input length(save_name): ", length(save_name),
             "\n✖ Length of save name object must be 1.", call.=FALSE)
    } else if (!inherits(save_name, "character")) {
        stop("Save name must be of class character: ",
             "\nℹ For your input class(save_name): ", class(save_name),
             call.=FALSE)
    } else if (nchar(save_name) <= 0) {
        stop("Save name must be 1 or more characters.", call.=FALSE)
    }

    if (length(scale_width) != 1) {
        stop("Must provide a single number for scale_width:",
             "\nℹ For your input length(scale_width): ", length(scale_width),
             call. = FALSE)
    } else if (!inherits(scale_width, "numeric")) {
        stop("Edge width scaling factor must be of class numeric: ",
             "\nℹ For your input class(scale_width): ", class(scale_width),
             call.=FALSE)
    }


    # Check type of network ---------------------------------------------------

    if (is(network, "graphNEL")) {
        graph <- igraph::graph_from_graphnel(network)

    } else if (is(network, "igraph")) {
        graph <- network

    } else if (is_network_list(network)) {
        graph <- igraph::graph_from_data_frame(network$edges,
                                               vertices = network$vertices,
                                               directed = directed)
    } else {
        stop("Input network must be graphNEL, igraph, or list containing data ",
             "frames named 'vertices' and 'edges'. \nℹ Class of your network: ",
             class(network), call.=FALSE)
    }


    # Capture input -----------------------------------------------------------

    # Store additional user arguments as list
    plot_params <- list(...)

    # Get the values from additional user input or otherwise use the defaults
    edge_width <- plot_params[["edge.width"]] %||%
        igraph::edge.attributes(graph)[["width"]]
    edge_color <- plot_params[["edge.color"]] %||%
        igraph::edge.attributes(graph)[["color"]]
    edge_arrowsize <- plot_params[["edge.arrow.size"]] %||% 0.3
    edge_curve <- plot_params[["edge.curved"]] %||% 0
    if (directed)   edge_curve <- plot_params[["edge.curved"]] %||% 0.05
    vertex_color <- plot_params[["vertex.color"]] %||%
        igraph::vertex.attributes(graph)[["color"]]
    vertex_size <- plot_params[["vertex.size"]] %||%
        igraph::vertex.attributes(graph)[["size"]]
    vertex_brd_col<- plot_params[["vertex.frame.color"]] %||% "white"
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
    # If an error occurs stop the opened graphical device
    if (export_type != "print") {
        dev_name <- if (export_type == "ps") "postscript" else export_type
        on.exit(if (names(dev.cur()) == dev_name) dev.off())
    }

    # Change graphical parameters here,
    #     so they will affect the newly active device
    if (length(par_opts) != 0) old_par <- do.call(par, par_opts)

    # Visualize the basic graph
    do.call(igraph::plot.igraph,
            c(list(x = graph,
                   layout = node_arrangement,
                   edge.width = edge_width * scale_width,
                   edge.color = edge_color,
                   edge.arrow.size = edge_arrowsize,
                   edge.curved = edge_curve,
                   vertex.size = vertex_size,
                   vertex.color = vertex_color,
                   vertex.label = vertex_label,
                   vertex.frame.color = vertex_brd_col),
              plot_params
            )
    )

    # If vertex labels are to be added radially that will be done below
    if (radial_labs) {
        x = node_arrangement[,1]
        y = node_arrangement[,2]

        # Check if user wants to overwrite one of the default arguments
        txt_x <- if ("x" %in% names(rad_lab_opts))
            rad_lab_opts[["x"]] else x * 1.1
        txt_y <- if ("y" %in% names(rad_lab_opts))
            rad_lab_opts[["y"]] else y * 1.1
        txt_labels <- if ("labels" %in% names(rad_lab_opts))
            rad_lab_opts[["labels"]] else vertex_label0
        txt_adj <- if ("adj" %in% names(rad_lab_opts))
            rad_lab_opts[["adj"]] else ifelse(x<=0, 1, 0)
        txt_cex <- if ("cex" %in% names(rad_lab_opts))
            rad_lab_opts[["cex"]] else 0.85
        # Remove the used arguments from list to prevent errors
        #   by making sure the plot function only receives them once
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

# Internal function that starts graphical devices for saving.
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
