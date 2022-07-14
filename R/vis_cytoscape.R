# optional columns for nodes: (group), color, size
# optional columns for edge table: width (otherwise defaults to weight), color
# save name is used both for session and for image, so don't add extension, if
#   the name already exists a number will be add, when both session and image
#   are save the numbers are chosen to match
vis_in_cytoscape <- function(edge_table, node_table,
                             save_session = TRUE,
                             close_session = TRUE,
                             save_name = NULL,
                             export_image = TRUE,
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
