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
# For visualising the Edge Weights,
#   Choose "type = 1" for grading the edges based on partial correlation values
#   Choose "type = 2" for grading the edges based on Pearson or Spearman correlation values
#   Choose "type = 3" for grading the edges on a ranked percentile system (such that the edges are ranked and on an exponential scale
#   the gradient of the width and colour of edges are assigned. for eg. 98th percentile with the highest width, 95th percentile with the next width etc.)
#
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




vis_in_cytoscape <- function(edge_table, node_table, netw_nr = 1, save_session = TRUE) {

  # Cytoscape needs additional columns that indicate how nodes relate
  edge_table$Interaction <- "interacts"
  edge_table$sharedname <- paste(edge_table$Source,
                                 "(interacts)",
                                 edge_table$Target)

  # Set names for labeling network aspects in cytoscape
  Network_name = sprintf("Visual_Network_%i", netw_nr)
  Network_Collection = sprintf("Visual_Networks_%i", netw_nr)
  style_name = "SanjeeNetworkStyle"

  # Prepare data to visualize for Cytoscape
  nodes <- data.frame(id = as.vector(node_table$Node),
                      group = as.vector(node_table$Groups),
                      stringsAsFactors = FALSE)
  edges <- data.frame(source = as.vector(edge_table$Source),
                      target = as.vector(edge_table$Target),
                      interaction = as.vector(edge_table$Interaction),
                      weight = as.vector(edge_table$Weight),
                      stringsAsFactors = FALSE)
  defaults <- list(NODE_SHAPE = "Ellipse",
                   NODE_SIZE = 25.0,
                   EDGE_TRANSPARENCY = 255,
                   NODE_LABEL_POSITION = "W,E,c,0.00,0.00",
                   NODE_BORDER_PAINT = "#FFFFFF")
  n_groups <- length(unique(node_table$Groups))

  # Create Cytoscape network
  RCy3::createNetworkFromDataFrames(nodes,
                                    edges,
                                    title = Network_name,
                                    collection = Network_Collection,
                                    style.name  =  style_name)
  # Create network properties
  nodeLabels <- RCy3::mapVisualProperty("Node Label", "id", "p")
  nodecolour <- RCy3::mapVisualProperty("Node Fill Color", "group", "d",
                                        unique(node_table$Groups),
                                        n_distinct_cols(n_groups))
  nodeXlocation <- RCy3::mapVisualProperty("Node X Location", "id", "d",
                                           as.vector(node_table$Node),
                                           as.vector(node_table$X))
  nodeYlocation <- RCy3::mapVisualProperty("Node Y Location", "id", "d",
                                           as.vector(node_table$Node),
                                           as.vector(node_table$Y))
  edgeline <- RCy3::mapVisualProperty("Edge Line Type", "interaction", "d",
                                      as.vector(unique(edge_table$Interaction)),
                                      as.vector(c("Solid")))
  edgewidth <- RCy3::mapVisualProperty("Edge Width", "shared name", "d",
                                       as.vector(edge_table$sharedname),
                                       as.vector(edge_table$width))
  edgestroke <- RCy3::mapVisualProperty("Edge Stroke Unselected Paint", "shared name", "d",
                                        as.vector(edge_table$sharedname), as.vector(edge_table$Stroke))
  # Set visual style
  RCy3::createVisualStyle(style_name,
                          defaults,
                          list(nodeLabels, nodecolour, nodeXlocation, nodeYlocation,
                               edgeline, edgewidth, edgestroke))
  RCy3::setVisualStyle(style_name)

  # Fit content into window
  # TODO check if doing this three times is required
  RCy3::fitContent(selected.only = FALSE)
  RCy3::fitContent(selected.only = FALSE)
  RCy3::fitContent(selected.only = FALSE)

  Network_out = sprintf("Network_Image_%i", netw_nr)

  full.path = paste(getwd(), Network_out, sep = "/")
  RCy3::exportImage(full.path, "PNG", units = "pixels", width = 3600, height = 1771)


  Network_save = sprintf("Cytoscape_Network_%i", netw_nr)
  full.path.cps = paste(getwd(), Network_save, sep = "/")
  RCy3::closeSession(save.before.closing = save_session,
                     filename = full.path.cps)
}
