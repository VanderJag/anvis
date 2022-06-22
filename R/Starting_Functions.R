#Required packages:
#   library(dplyr)
#   library(RCy3)
#Keep cytoscape open in the background
#Inputs
#   A is an adjacency matrix with the 1st column containing the column names and 1st row containing the row names and
#       the matrix containing numerical values corresponding to the variable relationships in the columns and rows
#   A can also be a list of matrices.
#   Group is boolean asking if certain variables are to be clustered together as neighbours in the figure.
#   Incase certain variables are to be clustered together in groups in the visualisation,
#   G is a vector containing the label names of the "Groups to be clustered together" Nodes with the same labels will be visualised adjacent to each other.
#For visualising the Edge Weights,
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

#' Adjacency matrix to edgelist
#'
#' Converts an adjacency matrix into an weighted edgelist. Interactions of nodes
#' with themselves are removed
#'
#' @param df Data frame or matrix. Square adjacency matrix. Nodes must be specified
#'   in rownames and column names.
#' @return Returns data frame representing a network as weighted edgelist. Columns are
#'   Source, Target, and Weight.
#'
#' @examples
#' nodes <- c(paste0("IL", 1:5), paste0("CCL", 1:3), paste0("CXCL", 1:4))
#' n_nodes <- length(nodes)
#'
#' interactions <- (runif(n_nodes**2)) |>
#'   matrix(nrow = n_nodes, ncol = n_nodes)
#'
#' row.names(interactions) <- nodes
#' colnames(interactions) <- nodes
#'
#' for(i in 1:n_nodes) {
#'   interactions[i,i] <- 1
#' }
#'
#' adj_matrix_to_edgelist(interactions)
adj_matrix_to_edgelist <- function(df) {
  diag(df) = 0
  df[lower.tri(df, diag=TRUE)] <- 0

  # Prepare to convert to long format by making the rownames a column of their own
  df <- df %>%
    tibble::as_tibble(df, rownames = "Source")

  # Convert into the edgelist format
  edgelist <- tidyr::pivot_longer(df, cols = -Source, names_to = "Target", values_to = "Weight") %>%
    # Self interaction and dulicate info from lower triangle have been set to 0
    dplyr::filter(Weight != 0)

  return(edgelist)

  # Previous code
  # diag(Adj) = 0
  # Adj[lower.tri(Adj, diag=TRUE)] <- 0
  #
  # Source = NULL
  # Target = NULL
  # Weight = NULL
  # for (row in 1:nrow(Adj)) {
  #   for (col in 1:ncol(Adj)) {
  #     if (Adj[row, col] != 0) {
  #       Source <- as.vector(append(Source, rownames(Adj[row, ])))
  #       Target <- as.vector(append(Target, colnames(Adj[col])))
  #       Weight <- as.vector(append(Weight, as.numeric(Adj[row, col])))
  #     } else {}
  #   }
  # }
}


adj_matrix_to_nodetable <- function(df) {

  data.frame("Node" = colnames(df))
}


# adds a column with the grouping info to a node table, and sorts it
group_nodes <- function(node_table, group_vec = NULL) {

  # TODO Check if grouping vector has the correct length and ensure it's character type

  # If a grouping vector has been provided
  if (!is.null(group_vec)) {
    node_table$Groups <- group_vec
    node_table <- node_table[order(node_table$Groups),]
  } else {
    node_table$Groups <- "A"
  }
}

add_node_pos <- function(node_table, layout = "circle") {
  X = NULL
  Y = NULL
  R = round(nrow(node_table)/10, 0) * (100)

  if (layout == "circle") {
    # Calculate position in circle
    for (i in 0:(nrow(node_table) - 1)) {
      x = R*cos((i*2*3.14159265359)/(nrow(node_table)))
      X <- as.vector(append(X, x))
      y = R*sin((i*2*3.14159265359)/(nrow(node_table)))
      Y <- as.vector(append(Y, y))
    }
  } else {
    # TODO complete error message
    stop("Must select valid network layout. ",
         "\nℹ you selected: ", layout,
         "\n✖ parameter `layout` must be one of ...", call.=FALSE)
  }

  pos <- as.data.frame(cbind(X,Y))
  node_table <- cbind(node_table, pos)

  return(node_table)
}



# requires df with a column called Weight
edge_weight_to_widths <- function(edge_table, type) {
  frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
  n_edges = nrow(edge_table)
  M = NULL
  for (i in 1:length(frac)) {
    f = frac[i]
    mes = NULL
    mes = round((f * n_edges)/100, 0)
    M <- as.vector(append(M, mes))
  }
  diff = (sum(M)) - (nrow(edge_table))
  ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

  wids <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))
  wid = NULL
  for (j in 1:length(M)) {
    times = M[j]
    value = wids[j]
    wid <- as.vector(append(wid, c(rep(value, times))))
  }

  #1 is partcor, 2 is cor, 3 is MI, 4 is ranked, 5 is percentile.
  ifelse(type == 1, edge_table <- mutate(edge_table, width=sigmoid_xB(x=nthroot(abs(Weight), 3), B=3)),
         ifelse(type == 2, edge_table <- mutate(edge_table, width=sigmoid_xB(x=abs(Weight), B=3)),
                ifelse(type == 3, edge_table <- mutate(edge_table, width=sigmoid_xB(x=(abs(Weight)/max(abs(df))), B=3)),
                       ifelse(type == 4, edge_table <- mutate(edge_table, width = sigmoid_xB(x=(Rank(-Weight)/n_edges), B=3)),
                              if(type == 5){
                                wid <- as.data.frame(wid)
                                edge_table <- edge_table[sort(abs(edge_table$Weight), decreasing=T, index.return=T)[[2]],]
                                edge_table <- cbind(edge_table, wid)
                                colnames(edge_table)[5] <- "width"
                              }
                              else{
                                print("type not selected")
                              }))))

  tmp_x <- seq(0, 1, length.out = 1000)
  plot(tmp_x, sigmoid_xB(tmp_x, 3))
  abline(0,1)
  return(edge_table)
}

weights_to_color <- function(edge_table) {

  # If negative numbers are found in the weights use a diverging color palette,
  #   otherwise use a sequential color palette
  if (min(edge_table$Weight) < 0) {
    Stroke <- as.vector(diverging_hcl(n=nrow(edge_table), palette = "Blue-Red"))
    edge_table <- edge_table[sort(edge_table$Weight, decreasing=T, index.return=T)[[2]],]
    edge_table <- cbind(edge_table, Stroke)
  } else {
    Stroke <- as.vector(sequential_hcl(n=nrow(edge_table), palette = "Reds2"))
    edge_table <- edge_table[sort(abs(edge_table$Weight), decreasing=T, index.return=T)[[2]],]
    edge_table <- cbind(edge_table, Stroke)
  }

  return(edge_table)
}

vis_in_cytoscape <- function(edge_table, node_table, netw_nr = 1) {

  Interaction <- rep("interacts", nrow(edge_table))
  edge_table <- cbind(edge_table, Interaction)
  edge_table$sharedname <- paste(edge_table$Source, "(interacts)", edge_table$Target)

  Network_name = sprintf("Visual_Network_%i", netw_nr)
  Network_Collection = sprintf("Visual_Networks_%i", netw_nr)

  nodes = NULL
  edges = NULL

  nodes <- data.frame(id=as.vector(node_table$Node), group=as.vector(node_table$Groups), stringsAsFactors = FALSE)
  edges <- data.frame(source=as.vector(edge_table$Source), target=as.vector(edge_table$Target), interaction=as.vector(edge_table$Interaction), weight=as.vector(edge_table$Weight), stringsAsFactors = FALSE)

  createNetworkFromDataFrames(nodes, edges, title=Network_name, collection=Network_Collection, style.name = "SanjeeNetworkStyle")


  Colour_palette <- as.vector(c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC", "#003C6799", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"))

  style.name = "SanjeeNetworkStyle"
  defaults <- list(NODE_SHAPE="Ellipse", NODE_SIZE=25.0, EDGE_TRANSPARENCY=255 , NODE_LABEL_POSITION="W,E,c,0.00,0.00", NODE_BORDER_PAINT="#FFFFFF")
  nodeLabels <- mapVisualProperty("Node Label", "id", "p")
  nodecolour <- mapVisualProperty("Node Fill Color", "group", "d", as.vector(unique(node_table$Groups)), as.vector(Colour_palette[1:length(unique(node_table$Groups))]))
  nodeXlocation <- mapVisualProperty("Node X Location", "id", "d", as.vector(node_table$Node), as.vector(node_table$X))
  nodeYlocation <- mapVisualProperty("Node Y Location", "id", "d", as.vector(node_table$Node), as.vector(node_table$Y))
  edgeline <- mapVisualProperty("Edge Line Type", "interaction", "d", as.vector(unique(edge_table$Interaction)), as.vector(c("Solid")))
  edgewidth <- mapVisualProperty("Edge Width", "shared name", "d", as.vector(edge_table$sharedname), as.vector(edge_table$width))
  edgestroke <- mapVisualProperty("Edge Stroke Unselected Paint", "shared name", "d", as.vector(edge_table$sharedname), as.vector(edge_table$Stroke))

  createVisualStyle(style.name, defaults, list(nodeLabels, nodecolour, nodeXlocation, nodeYlocation, edgeline, edgewidth, edgestroke))
  setVisualStyle("SanjeeNetworkStyle")

  fitContent(selected.only = FALSE)
  fitContent(selected.only = FALSE)
  fitContent(selected.only = FALSE)

  Network_out = sprintf("Network_Image_%i", netw_nr)

  full.path = paste(getwd(), Network_out, sep="/")
  exportImage(full.path, "PNG", units="pixels", width=3600, height=1771)


  Network_save = sprintf("Cytoscape_Network_%i", netw_nr)
  full.path.cps = paste(getwd(), Network_save, sep="/")
  closeSession(save.before.closing = TRUE, filename = full.path.cps)
}

sigmoid_xB <- function(x, B){
  T = (1/(1+((x/(1-x))^-B)))
  return(T)
}

#' Create formatted circular network
#'
#' This function creates a visualization for the input network
#' information. The present implementation creates a network in which nodes are
#' arranged in a circle. To highlight relevant interactions, there are different
#' options available that determine the scaling of the weights in the network.
#'
#' @param df_adjacency A square matrix or data frame or a list of these. The data
#'   should reprensed the strength of the relation between what will be the nodes in
#'   the network. Rownames and column names are required.
#' @return The section on the returned values
#'
#' @examples
#' sum(1:10)
#'
#' @section Additional criteria for the use of this function
#' * The Cytoscape software needs to be running.
#' *
#'
#' @importFrom dplyr mutate
VisualiseNetwork <- function(df_adjacency, group_vec = NULL, type = 2) {

  # Since this function uses a for loop to iterate over the visualizations that
  #   are created, the input needs to be converted into a list.
  if(inherits(df_adjacency, "data.frame") == TRUE |
     inherits(df_adjacency, "matrix") == TRUE) {
    df_adjacency <- list(df_adjacency)
  } else if (!inherits(df_adjacency, "list")) {
    stop("Must provide data.frame, matrix, or list of these as input \n",
         "You provided: ", class(df_adjacency), call. = FALSE)
  }

  # These variables will be used to store all prepared and rescaled networks
  #   and return them in the end of this function
  AdjMatrix = NULL
  NodesNetwork = NULL
  EdgesNetwork = NULL

  # Main loop to iterate over the multiple networks that can be provided as input
  for (i_matrix in 1:length(df_adjacency)) {

    # These variable store are used to create the current network in edgelist format
    Adjacency = as.data.frame(df_adjacency[i_matrix])
    node_table = NULL
    EdgeTable = NULL

    # If the number and column of the adjacency matrix is not equal there may be
    #   missing info and an error with the data input
    if (ncol(Adjacency) != nrow(Adjacency)) {
      stop("Adjacency matrix should be a square matrix with equal number of rows ",
           "and columns", call. = FALSE)
    }

    # Incomplete group information can not be correctly assigned
    if (nrow(Adjacency) != length(group_vec)) {
      stop("The number of nodes/variables in the groups table should be the same ",
           "as in the adjacency matrix", call. = FALSE)
    }


    # Node table with group info ----------------------------------------------

    node_table <- adj_matrix_to_nodetable(Adjacency)

    # Adding grouping information ---------------------------------------------

    node_table <- group_nodes(node_table, group_vec = group_vec)

    # Cytoscape node positions ------------------------------------------------

    node_table <- add_node_pos(node_table)


    # adjecency to edgelist ---------------------------------------------------

    edge_table <- adj_matrix_to_edgelist(Adjacency)

    # Convert edge weight to edge width ---------------------------------------

    edge_table <- edge_weight_to_widths(edge_table, type = type)

    # Add colour column to edge table -----------------------------------------

    edge_table <- weights_to_color(edge_table)



    # Visualize in Cytoscape --------------------------------------------------
    vis_in_cytoscape(edge_table = edge_table,
                     node_table = node_table,
                     netw_nr = i_matrix)



    # Network files for building network using some other software
    AdjMatrix <- list(AdjMatrix, Adjacency)
    NodesNetwork <- list(NodesNetwork, node_table)
    EdgesNetwork <- list(EdgesNetwork, edge_table)


  }


  Network = list(AdjMatrix, NodesNetwork, EdgesNetwork)
  return(Network)

}

# Run example
library(dplyr)        # For data wrangling, e.g. using mutate
library(RCy3)         # For communicating with cytoscape
library(pracma)       # To use the nthroot function
library(colorspace)   # To adjust color palettes

# Load adjacency matrix
Mat1 <- readRDS("tests/trail_adjacency_matrix.rds")

# Some grouping based on column names
group_vec <- rep("A", times = nrow(Mat1))
group_vec[colnames(Mat1) |> stringr::str_detect("IL")] <- "B"
group_vec[colnames(Mat1) |> stringr::str_detect("CCL")] <- "C"
group_vec[colnames(Mat1) |> stringr::str_detect("CXCL")] <- "D"

# Visualize the network
Visual <- VisualiseNetwork(Mat1,
                           group_vec = group_vec,
                           type = 1)

