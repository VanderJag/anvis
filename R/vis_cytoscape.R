#' Visualize in Cytoscape
#'
#' Uses RCy3 to communicate with cytoscape. Visualizes the
#' network with the default circular layout. Additional visual properties are appended to the network depending on the attributes specified in 
#' edge_table` and `node_table`.
#' Both the visualised network and the cytoscape session can be saved.
#'
#' @param node_table Data frame with required column 'node'. Optional are columns
#'   'color' and 'size': when present they are used to determine the respective
#'   visual properties of the nodes. And additional columns present in the table 
#'   are only appended in the cytoscape node table for conveniance. 
#' @param edge_table Data frame with required columns 'source' and 'target'.
#'   Optional are columns: 'width' or 'weight' to determine edge widths in the
#'   visualization ('weight' is used in absence of column 'width'), and
#'   'color' to determine the color of the edges.
#' @param export_image Logical (default TRUE), Visualization is saved when TRUE.
#' @param save_session Logical (default TRUE), Cytoscape session is saved when TRUE.
#' @param close_session Logical (default TRUE), Cytoscape session is closed when TRUE.
#'   Usefull when visualising diferent networks in different sessions.
#' @param save_name Character string containing the desired name (excluding file extensions) for image and session
#'   save files. An appropriate tailing integer is appended to avoid the overwriting of an existing file with the same name. This argument supersedes the "filename" argument of image_opts.
#'   If both 'save-name' and 'filename' aguments are `NULL`, "network" will be used as default name. 
#' @param image_opts List with named values that are used to specify properties of the
#'   exported image. All arguments accepted by [RCy3::exportImage] are valid.
#' @param cyto3.8_check Logical (default TRUE). The function is stopped if cytoscape version 3.8 is detected when `TRUE`(default) and user is asked to uprade their cytoscape software. 
#'   Cytoscape 3.8 has issues interacting with RCy3, however the user can use the `FALSE` boolean to continue using this function with Cytoscape 3.8.

#Sanjee_Cooments:
#Add: Requires Cyotoscape open/running in the background.
#In parameters node_table and edge_table, can the optional column name be "color" or "colour"



vis_in_cytoscape <- function(node_table, edge_table,
                             export_image = TRUE,
                             save_session = TRUE,
                             close_session = TRUE,
                             save_name = NULL,
                             image_opts = list(filename = "network", type = "PNG"),
                             cyto3.8_check = T) {

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
  node_table <- add_node_pos(node_table)
  node_table <- node_table %>% dplyr::rename("id" = "node")

  # Cytoscape needs additional columns that indicate how nodes relate
  edge_table$interaction <- "interacts"
  edge_table$sharedname <- paste(edge_table$source,
                                 "(interacts)",
                                 edge_table$target)

  # Prepare names for saving the networks and for naming in cytoscape
  # Get default
  save_name <- save_name %||% image_opts[["filename"]] %||% "network"
  # store original name for series
  save_name0 <- save_name
  # Check suffix numbers to avoid duplicate names, always checks for both image and cys file
  save_name <- file_pair_seq(save_name,
                             ext1 = paste0(".", (image_opts[["type"]] %||% "PNG")),
                             ext2 = ".cys")

  # Set names for labeling network aspects in cytoscape
  Network_name = save_name
  Network_Collection = save_name0
  style_name = "netvis_style"

  # Prepare defaults
  defaults <- list(NODE_SHAPE = "Ellipse",
                   NODE_SIZE = 25.0,
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
  vis_props[["nodeXlocation"]] <- RCy3::mapVisualProperty("Node X Location", "X", "p")
  vis_props[["nodeYlocation"]] <- RCy3::mapVisualProperty("Node Y Location", "Y", "p")
  vis_props[["edgeline"]] <- RCy3::mapVisualProperty("Edge Line Type", "interaction", "d",
                                      as.vector(unique(edge_table$interaction)),
                                      as.vector(c("Solid")))
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
    image_opts[["filename"]] <- save_name
    do.call(RCy3::exportImage, image_opts)
  }
  if (save_session) RCy3::saveSession(filename = save_name)
  if (close_session) RCy3::closeSession(save.before.closing = FALSE)
}
