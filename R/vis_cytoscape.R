#' Visualize in Cytoscape
#'
#' Uses RCy3 to communicate to your running cytoscape software. Visualizes your
#' network with a circular layout. Depending on the attributes of your
#' network, additional visual properties will be added to
#' the visualization. Image and session object can be saved.
#'
#' @param network A network object of one of the valid classes:
#'     "igraph", "graphNEL", and "list" of two data frames with the names "edges"
#'     and "vertices". If the nodes have an additional attribute 'color' or
#'     'size', the respective the respective element will be adjusted in the
#'     visualization. If edges have the attributes 'width' or 'weight', this will
#'     be used to determine edge widths in the visualization (only when 'width'
#'     is not present weight will be used). Edge attribute 'color' determines
#'     the color of edges.
#' @param directed A logical (automatically determined for network
#'     objects of class 'igraph' and 'graphNEL', otherwise default `FALSE`).
#'     When this argument is `TRUE` edges will be drawn with arrows.
#' @param radial_labs A logical (default `TRUE`) to indicate whether node labels
#'     should be positioned radially around the circular arrangement of nodes.
#' @param export_image Logical (default `TRUE`), should the visualization be saved?
#' @param save_session Logical (default `TRUE`), specifying whether the cytoscape
#'     session should be saved (as .cys file).
#' @param close_session Logical (default `TRUE`), should the cytoscape session be
#'     closed after optional saving?
#' @param save_name Character string (default 'network') that will be used to
#'     name image and session save files. File extensions should not be included
#'     for this argument, as they will be added automatically. If the name
#'     chosen for `save_name` already exists in the current working directory
#'     numbers will be appended to it.
#' @param export_type Character string, one of 'PNG' (default), 'JPEG', 'PDF',
#'     'SVG', or 'PS'. Determines which file format will be used to save the
#'     network visualization.
#' @param export_opts List with named values that will be used to customize
#'     image export. Any argument accepted by [RCy3::exportImage] is valid.
#'     The options 'filename' and 'type' of this list will be overwritten
#'     by `save_name` and `export_type`, respectively.
#' @param scale_width Numeric, a number that will be multiplied with the edge
#'     widths, scaling the edge widths linearly.
#' @param node_space Numeric (default 1.2). Adjusts spacing between nodes.
#'     When this argument is 1, the borders of the largest nodes
#'     can be touching. When e.g. a value of 2 is chosen for this argument, the
#'     space around a node will be twice the (maximum) node size.
#' @param cyto3.8_check Logical (default `TRUE`). Should execution stop if
#'     Cytoscape version 3.8.x is detected? `FALSE` to skip this test.
#'     Cytoscape version 3.8.x has problems interacting with `RCy3`, first
#'     visualization may not show.
#'
#' @export
visCytoscape <- function(network,
                         directed = if (is(network, "graphNEL")) {
                             graph::edgemode(network) == "directed"
                         } else if (is(network, "igraph")) {
                             igraph::is_directed(network)
                         } else {
                             FALSE
                         },
                         radial_labs = TRUE,
                         export_image = TRUE,
                         save_session = TRUE,
                         close_session = export_image,
                         save_name = "network",
                         export_type = c("PNG", "JPEG", "PDF", "SVG", "PS"),
                         export_opts = list(),
                         scale_width = 1,
                         node_space = 1.2,
                         cyto3.8_check = T) {

    export_type <- match.arg(export_type)

    # Convert any of the possible network types into data frames
    if (is(network, "graphNEL")) {
        network <- dfs_from_graphNEL(gr_nel = network)

    } else if (is(network, "igraph")) {
        network <- dfs_from_igraph(igraph_obj = network)

    } else if (is_network_list(network)) {
        # Network is already correct format
        network <- network
    } else {
        stop("Input network must be graphNEL, igraph, or list containing data ",
             "frames named 'vertices' and 'edges'. \nℹ Class of your network: ",
             class(network), call.=FALSE)
    }

    edge_table <- network$edges
    node_table <- network$vertices

    # Check that the input contains the required information on the nodes and edges
    if (!"node" %in% colnames(node_table)) {
        stop("`node_table` must contain column named 'node':",
             "\nℹ Column names of your `node_table`: ",
             paste0(colnames(node_table), collapse = ", "), ".",
             call.=FALSE)
    }

    if (!all(c("source", "target") %in% colnames(edge_table))) {
        stop("`edge_table` must contain columns 'source' and 'target':",
             "\nℹ Column names of your `edge_table`: ",
             paste0(colnames(edge_table), collapse = ", "), ".",
             call.=FALSE)

    }

    # Calculate positions for the nodes
    def_node_size <- 25
    if ("size" %in% colnames(node_table)) def_node_size <- max(node_table$size)

    node_table <- add_node_pos(node_table = node_table,
                               nodesize = def_node_size,
                               space_fct = node_space)
    # Convert name of node name column to id, so cytoscape recognizes it
    node_table <- node_table %>% dplyr::rename("id" = "node")

    # Cytoscape needs additional columns that indicate how nodes relate
    edge_table$interaction <- "interacts"
    edge_table$sharedname <- paste(edge_table$source,
                                   "(interacts)",
                                   edge_table$target)

    # Rescaling of node widths
    if ("width" %in% colnames(edge_table)) {
        edge_table$width <- edge_table$width * scale_width
    }

    # Prepare names for saving the networks and for naming in cytoscape
    # store original name for series
    save_name0 <- save_name
    # Check suffix numbers to avoid duplicate names,
    #    always checks for both image and cys file
    save_name <- cyto_file_seq(save_name,
                               ext1 = paste0(".",
                                             (export_opts[["type"]] %||% "PNG")),
                               ext2 = ".cys")

    # Set names for labeling network aspects in cytoscape
    Network_name = save_name
    Network_Collection = save_name0

    # Prepare defaults
    defaults <- list(NODE_SHAPE = "Ellipse",
                     NODE_SIZE = def_node_size,
                     EDGE_TRANSPARENCY = 255,
                     NODE_LABEL_POSITION = "W,E,c,0.00,0.00",
                     NODE_BORDER_PAINT = "#FFFFFF")


    # Check if a valid version of cytoscape is used
    if (cyto3.8_check) {
        cytosc_v <- RCy3::cytoscapeVersionInfo()[["cytoscapeVersion"]] %>%
            numeric_version()

        if (!(cytosc_v < "3.8" | cytosc_v >= "3.9")) {
            stop("Must use a cytoscape version different from 3.8.x:",
                 "\nℹ You're using: ", cytosc_v,
                 "\n✖ Cytoscape versions 3.8.x fail to make the first visualization.",
                 call.=FALSE)
        }
    }

    # Having the same style name for multiple networks causes issues with
    #   visualizations, avoiding this by checking which visual styles already exist
    prev_styles <- RCy3::getVisualStyleNames() %>%
        stringr::str_subset("^anvis_style")

    if (length(prev_styles) == 0) {
        style_name <- "anvis_style_1"
    } else {
        # add one to the highest number found in the styles
        style_i <- prev_styles %>%
            stringr::str_extract(pattern = "\\d+") %>%
            as.numeric() %>%
            max() + 1
        style_name <- paste0("anvis_style_", style_i)
    }

    # Create Cytoscape network
    RCy3::createNetworkFromDataFrames(nodes = node_table,
                                      edges = edge_table,
                                      title = Network_name,
                                      collection = Network_Collection,
                                      style.name  =  style_name)

    # Create network properties
    vis_props <- list()
    vis_props[["nodeLabels"]] <- RCy3::mapVisualProperty(
        "Node Label", "id", "p")
    vis_props[["nodeXlocation"]] <- RCy3::mapVisualProperty(
        "Node X Location", "x", "p")
    vis_props[["nodeYlocation"]] <- RCy3::mapVisualProperty(
        "Node Y Location", "y", "p")
    vis_props[["edgeline"]] <- RCy3::mapVisualProperty(
        "Edge Line Type", "interaction", "d",
        as.vector(unique(edge_table$interaction)),
        as.vector(c("Solid")))

    # Add radial node labels if the option is selected
    if (radial_labs) {
        vis_props[["nodeLabelPosition"]] <- RCy3::mapVisualProperty(
            "NODE LABEL POSITION", "id", "d", node_table$id,
            prep_label_pos(node_table$x, node_table$y, def_node_size))

        vis_props[["nodeLabelRotation"]] <- RCy3::mapVisualProperty(
            "NODE LABEL ROTATION", "id", "d", node_table$id,
            radial_angle(node_table$x, node_table$y))

    }

    # Optional properties

    if ("color" %in% colnames(node_table)) {
        vis_props[["nodecolor"]] <- RCy3::mapVisualProperty(
            "Node Fill Color", "color", "p")
    }

    if ("size" %in% colnames(node_table)) {
        vis_props[["nodesize"]] <- RCy3::mapVisualProperty(
            "Node Size", "size", "p")
    }

    if (any(c("width", "weight") %in% colnames(edge_table))) {
        # Weights might be outside the desired range for edge widths
        if (is.null(edge_table[["width"]])) {
            max_weight <- max(abs(edge_table$weight))
            if (max_weight > 5) {
                warning("Detected high values for the edge weights. Since these are used ",
                        "for edge width they can make visualizations unclear. The issue can be",
                        " avoided by using scaled edge widths.")
            }
        }
        vis_props[["edgewidth"]] <- RCy3::mapVisualProperty(
            "Edge Width", "shared name", "d",
            as.vector(edge_table$sharedname),
            edge_table$width %||% edge_table$weight)
    }

    if ("color" %in% colnames(edge_table)) {
        vis_props[["edgestroke"]] <- RCy3::mapVisualProperty(
            "Edge Stroke Unselected Paint", "shared name", "d",
            as.vector(edge_table$sharedname), as.vector(edge_table$color))
    }

    if (directed) {
        vis_props[["arrowshape"]] <- RCy3::mapVisualProperty(
            "EDGE TARGET ARROW SHAPE", "target", "d", edge_table$target,
            rep("ARROW", length(edge_table$target)))

        if ("color" %in% colnames(edge_table)) {
            vis_props[["arrowcolor"]] <- RCy3::mapVisualProperty(
                "Edge Target Arrow Unselected Paint",
                "shared name", "d", edge_table$sharedname,
                edge_table$color)
        }
    }

    # Set visual style
    RCy3::createVisualStyle(style.name = style_name,
                            defaults = defaults,
                            mappings = vis_props)
    RCy3::setVisualStyle(style_name)

    # Fit content into window
    RCy3::fitContent(selected.only = FALSE)

    # Saving and closing ------------------------------------------------------
    if (export_image) {
        if (!is.null(export_opts[["filename"]])) {
            warning('`export_opts[["filename"]` will be overwritten by `save_name`.')
        }
        export_opts[["filename"]] <- save_name
        export_opts[["type"]] <- export_type
        do.call(RCy3::exportImage, export_opts)
    }
    if (save_session) RCy3::saveSession(filename = save_name)
    if (close_session) RCy3::closeSession(save.before.closing = FALSE)

    invisible(NULL)
}


# position options
# default string would be like: "C,C,c,0.00,0.00"
# nodeAnchor="C", graphicAnchor="C", justification="c", xOffset=0.0, yOffset=0.0
# nodeAnchor Position on node to place the graphic: NW,N,NE,E,SE,S,SW,W
#    or C for center (default)
# graphicAnchor Position on graphic to place on node: NW,N,NE,E,SE,S,SW,W
#    or C for center (default)
# justification Positioning of content within graphic: l,r,c (default)
# xOffset Additional offset in the x direction
# yOffset Additional offset in the y direction
prep_label_pos <- function(x, y, nodesize) {
    # Check fraction of noderadius related to circle radius
    node_space <- (nodesize/2) / max(x)

    # place labels left or right of a node depeding on their postion in the circle
    sides <- ifelse(x < 0, "C,E,c,", "C,W,c,")
    # Create a bit of space to account for varying nodesizes, avoid 0 value
    nudge <- paste0(node_space * x + 0.001, ",", node_space * y + 0.001)
    # create properly formatted string
    paste0(sides, nudge)
}
