# Required packages:
#   library(dplyr)
#   library(RCy3)

# Inputs
# Keep cytoscape open in the background
#   A is an adjacency matrix with the 1st column containing the column names and 1st row containing the row names and
#       the matrix containing numerical values corresponding to the variable relationships in the columns and rows
#   A can also be a list of matrices.
#   Group is boolean asking if certain variables are to be clustered together as neighbours in the figure.
#   Incase certain variables are to be clustered together in groups in the visualisation,
#   G is a vector containing the label names of the "Groups to be clustered together" Nodes with the same labels will be visualised adjacent to each other.
#
#Example:
#	source("VisualiseNetwork.R")
#	library(dplyr)
#	library(RCy3)
#
#	setwd("full path to working directory")
#	Mat1 <- read.table("Adjacency_Matrix1.txt", header = TRUE, row.names = 1)
#       Mat2 <- read.table("Adjacency_Matrix2.txt", header = TRUE, row.names = 1)
#       mat3 <- read.table("Adjacency_Matrix3.txt", header = TRUE, row.names = 1)
#
#	A <- list(Mat1, Mat2, Mat3)
#	G <- as.vector(c(label1, label2, label3...., labeln))
#
#	Visual <- VisualiseNetwork(A, Group = TRUE, G, type = 3)
#
##############################################################################################################################################################



# optional columns for nodes: group,
vis_in_cytoscape <- function(edge_table, node_table, netw_nr = 1,
                             save_session = TRUE,
                             close_session = TRUE,
                             image_opts = list(filename = "network", type = "PNG",
                                               units = "inches", resolution = 600,
                                               height = 5, width = 5),
                             cyto3.8_check = T) {

  # TODO add option to give save names
  # TODO remove manual specification of network number, let the system automatically
  #   detect which new number to add when there is no saving name specified

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

  # Calculate positions for the nodes
  node_table <- add_node_pos(node_table)

  # Cytoscape needs additional columns that indicate how nodes relate
  edge_table$interaction <- "interacts"
  edge_table$sharedname <- paste(edge_table$source,
                                 "(interacts)",
                                 edge_table$target)

  # Set names for labeling network aspects in cytoscape
  Network_name = sprintf("Visual_Network_%i", netw_nr)
  Network_Collection = sprintf("Visual_Networks_%i", netw_nr)
  style_name = "default_style"

  # Prepare data to visualize for Cytoscape
  # Minimal required data
  nodes <- data.frame(id = as.vector(node_table$node))
  edges <- data.frame(source = as.vector(edge_table$source),
                      target = as.vector(edge_table$target),
                      interaction = as.vector(edge_table$interaction),
                      weight = as.vector(edge_table$weight))
  defaults <- list(NODE_SHAPE = "Ellipse",
                   NODE_SIZE = 25.0,
                   EDGE_TRANSPARENCY = 255,
                   NODE_LABEL_POSITION = "W,E,c,0.00,0.00",
                   NODE_BORDER_PAINT = "#FFFFFF")
  # Optional additional data
  if ("group" %in% colnames(node_table)) {
    nodes[["group"]] <- node_table$group
    n_groups <- length(unique(node_table$group))
  }

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
  RCy3::createNetworkFromDataFrames(nodes = nodes,
                                    edges = edges,
                                    title = Network_name,
                                    collection = Network_Collection,
                                    style.name  =  style_name)
  # Create network properties
  nodeLabels <- RCy3::mapVisualProperty("Node Label", "id", "p")
  nodecolour <- RCy3::mapVisualProperty("Node Fill Color", "group", "d",
                                        unique(node_table$group),
                                        n_distinct_cols(n_groups))
  nodeXlocation <- RCy3::mapVisualProperty("Node X Location", "id", "d",
                                           as.vector(node_table$node),
                                           as.vector(node_table$X))
  nodeYlocation <- RCy3::mapVisualProperty("Node Y Location", "id", "d",
                                           as.vector(node_table$node),
                                           as.vector(node_table$Y))
  nodesize <- RCy3::mapVisualProperty("Node Size", "shared name", "d",
                                      as.vector(node_table$node),
                                      as.vector(node_table$size))
  edgeline <- RCy3::mapVisualProperty("Edge Line Type", "interaction", "d",
                                      as.vector(unique(edge_table$interaction)),
                                      as.vector(c("Solid")))
  edgewidth <- RCy3::mapVisualProperty("Edge Width", "shared name", "d",
                                       as.vector(edge_table$sharedname),
                                       as.vector(edge_table$width))
  edgestroke <- RCy3::mapVisualProperty("Edge Stroke Unselected Paint", "shared name", "d",
                                        as.vector(edge_table$sharedname), as.vector(edge_table$color))
  # Set visual style
  RCy3::createVisualStyle(style_name,
                          defaults,
                          list(nodeLabels, nodecolour, nodesize,
                               nodeXlocation, nodeYlocation,
                               edgeline, edgewidth, edgestroke))
  RCy3::setVisualStyle(style_name)

  # Fit content into window
  # TODO check if doing this three times is required
  RCy3::fitContent(selected.only = FALSE)
  RCy3::fitContent(selected.only = FALSE)
  RCy3::fitContent(selected.only = FALSE)

  # Network_out = sprintf("Network_Image_%i", netw_nr)
  # full.path = paste(getwd(), Network_out, sep = "/")
  #
  # img_path <- if

  do.call(RCy3::exportImage, image_opts)


  Network_save = sprintf("Cytoscape_Network_%i", netw_nr)
  full.path.cps = paste(getwd(), Network_save, sep = "/")
  RCy3::closeSession(save.before.closing = save_session, filename = full.path.cps)
}
