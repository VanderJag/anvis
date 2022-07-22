#' Visualize in Cytoscape
#'
#' Uses RCy3 to communicate to your running cytoscape software. Visualizes your
#' network with a circular layout. Depending on the attributes of your
#' `edge_table` and `node_table`, additional visual properties will be added to
#' the visualization. Image and session object can be saved.
#'
#' @param node_table Data frame with required column 'node'. Optional are columns
#'   'color' and 'size': when present they will be used to determine the respective
#'   visual properties of the visualization. Other columns with node attributes
#'   will not be used in the visualization, but they will be added to the node
#'   table in cytoscape.
#' @param edge_table Data frame with required columns 'source' and 'target'.
#'   Optional are columns: 'width' or 'weight' to determine edge widths in the
#'   visualization (only when 'width' is not present weight will be used), and
#'   'color' to determine the color of the edges.
#' @param export_image Logical (default TRUE), should the visualization be saved?
#' @param save_session Logical (default TRUE), specifying whether the cytoscape
#'   session should be saved.
#' @param close_session Logical (default TRUE), should the cytoscape session be
#'   closed after optional saving?
#' @param save_name Character string that will be used to name image and session
#'   save files, excluding file extensions, as these will be added automatically.
#'   If this argument is `NULL`, `image_opt[["filename"]]` will be used for naming.
#'   If both are `NULL`, "network" will be used as default name. If the name
#'   chosen for `save_name` already exists in the current working directory
#'   numbers will be appended to it.
#' @param export_opts List with named values that will be used to customize
#'   image export. Any argument accepted by [RCy3::exportImage] is valid.
#' @param cyto3.8_check Logical (default TRUE). Should execution stop if
#'   Cytoscape version 3.8.x is detected? `FALSE` to skip this test.
#'   Cytoscape version 3.8.x has problems interacting with `RCy3`, first
#'   visualization may not show.
vis_in_cytoscape <- function(node_table, edge_table,
                             radial_labs = TRUE,
                             export_image = TRUE,
                             save_session = TRUE,
                             close_session = TRUE,
                             save_name = "network",
                             export_type = c("PNG", "JPEG", "PDF", "SVG", "PS"),
                             export_opts = list(),
                             cyto3.8_check = T) {

  export_type <- match.arg(export_type)

  # Check that the input contains the required information on the nodes and edges
  if (!"node" %in% colnames(node_table)) {
    stop("`node_table` must contain column named 'node':",
    "\nℹ Column names of your `node_table`: ", paste0(colnames(node_table),
                                                      collapse = ", "), ".",
    call.=FALSE)
  }

  if (!all(c("source", "target") %in% colnames(edge_table))) {
    stop("`edge_table` must contain columns 'source' and 'target':",
         "\nℹ Column names of your `edge_table`: ", paste0(colnames(edge_table),
                                                           collapse = ", "), ".",
         call.=FALSE)

  }

  # Calculate positions for the nodes, also cytoscape need node column called id
  def_node_size <- 254
  if ("size" %in% colnames(node_table)) def_node_size <- max(node_table$size)

  node_table <- add_node_pos(node_table = node_table,
                             nodesize = def_node_size)
  node_table <- node_table %>% dplyr::rename("id" = "node")

  # Cytoscape needs additional columns that indicate how nodes relate
  edge_table$interaction <- "interacts"
  edge_table$sharedname <- paste(edge_table$source,
                                 "(interacts)",
                                 edge_table$target)

  # Prepare names for saving the networks and for naming in cytoscape
  # store original name for series
  save_name0 <- save_name
  # Check suffix numbers to avoid duplicate names, always checks for both image and cys file
  save_name <- file_pair_seq(save_name,
                             ext1 = paste0(".", (export_opts[["type"]] %||% "PNG")),
                             ext2 = ".cys")

  # Set names for labeling network aspects in cytoscape
  Network_name = save_name
  Network_Collection = save_name0
  style_name = "netvis_style"

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

  # Create Cytoscape network
  RCy3::createNetworkFromDataFrames(nodes = node_table,
                                    edges = edge_table,
                                    title = Network_name,
                                    collection = Network_Collection,
                                    style.name  =  style_name)

  # Create network properties
  vis_props <- list()
  vis_props[["nodeLabels"]] <- RCy3::mapVisualProperty("Node Label", "id", "p")
  vis_props[["nodeXlocation"]] <- RCy3::mapVisualProperty("Node X Location", "x", "p")
  vis_props[["nodeYlocation"]] <- RCy3::mapVisualProperty("Node Y Location", "y", "p")
  vis_props[["edgeline"]] <- RCy3::mapVisualProperty("Edge Line Type", "interaction", "d",
                                      as.vector(unique(edge_table$interaction)),
                                      as.vector(c("Solid")))
  # Add radial node labels if the option is selected
  if (radial_labs) {
    vis_props[["nodeLabelPosition"]] <-
      RCy3::mapVisualProperty("NODE LABEL POSITION", "id", "d", node_table$id,
                              prep_label_pos(node_table$x, node_table$y, def_node_size))

    vis_props[["nodeLabelRotation"]] <-
      RCy3::mapVisualProperty("NODE LABEL ROTATION", "id", "d", node_table$id,
                              radial_angle(node_table$x, node_table$y))

  }

  # Optional properties
  if ("color" %in% colnames(node_table)) {
    vis_props[["nodecolor"]] <- RCy3::mapVisualProperty("Node Fill Color", "color", "p")
  }
  if ("size" %in% colnames(node_table)) {
    vis_props[["nodesize"]] <- RCy3::mapVisualProperty("Node Size", "size", "p")
  }
  if (any(c("width", "weight") %in% colnames(edge_table))) {
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
  # Set visual style
  RCy3::createVisualStyle(style_name, defaults, vis_props)
  RCy3::setVisualStyle(style_name)
  # Fit content into window
  RCy3::fitContent(selected.only = FALSE)


  # Saving and closing ------------------------------------------------------
  if (export_image) {
    export_opts[["filename"]] <- save_name
    export_opts[["type"]] <- export_type
    do.call(RCy3::exportImage, export_opts)
  }
  if (save_session) RCy3::saveSession(filename = save_name)
  if (close_session) RCy3::closeSession(save.before.closing = FALSE)
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
