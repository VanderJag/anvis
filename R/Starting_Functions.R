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
VisualiseNetwork <- function(df_adjacency, Group = TRUE, group_vec, type = 1) {

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
  for (n_matrix in 1:length(df_adjacency)) {

    # These variable store are used to create the current network in edgelist format
    Adjacency = as.data.frame(df_adjacency[n_matrix])
    NodeTable = NULL
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

    if(Group == FALSE){
      Adj <- Adjacency
      Node <- as.vector(colnames(Adj))
      Node <- as.data.frame(Node)
      Groups <- as.vector(rep("A", nrow(Adj)))
      NodeTable <- as.data.frame(cbind(Node, Groups))
    } else {
      Groups = as.vector(group_vec)
      Adj <- Adjacency
      Node <- as.vector(colnames(Adj))
      Node <- as.data.frame(Node)
      NodeTable <- as.data.frame(cbind(Node, Groups))
    }
    #NodeTable <- left_join(Node, Node_Group, by = NULL,  copy = FALSE, suffix=c(".n",".g"))
    NodeTable <- with(NodeTable, NodeTable[order(NodeTable$Groups) , ])

    diag(Adj) = 0
    Adj[lower.tri(Adj, diag=TRUE)] <- 0

    Source = NULL
    Target = NULL
    Weight = NULL
    for (row in 1:nrow(Adj)) {
      for (col in 1:ncol(Adj)) {
        if (Adj[row, col] != 0) {
          Source <- as.vector(append(Source, rownames(Adj[row, ])))
          Target <- as.vector(append(Target, colnames(Adj[col])))
          Weight <- as.vector(append(Weight, as.numeric(Adj[row, col])))
        } else {}
      }
    }
    Interaction <- as.vector(rep("interacts", length(Weight)))
    EdgeTable <- as.data.frame(cbind(Source, Target, Weight, Interaction))
    EdgeTable$Weight <- as.numeric(as.character(EdgeTable$Weight))


    X = NULL
    Y = NULL
    R = round(nrow(Adj)/10, 0) * (100)

    for (i in 0:(nrow(Adj) - 1)) {
      print(i)
      x = R*cos((i*2*3.14159265359)/(nrow(Adj)))
      X <- as.vector(append(X, x))
      y = R*sin((i*2*3.14159265359)/(nrow(Adj)))
      Y <- as.vector(append(Y, y))
    }
    pos <- as.data.frame(cbind(X,Y))

    NodeTable <- cbind(NodeTable, pos)

    frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
    E = nrow(EdgeTable)
    M = NULL
    for (i in 1:length(frac)) {
      f = frac[i]
      mes = NULL
      mes = round((f * E)/100, 0)
      M <- as.vector(append(M, mes))
    }
    diff = (sum(M)) - (nrow(EdgeTable))
    ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

    wids <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))
    wid = NULL
    for (j in 1:length(M)) {
      times = M[j]
      value = wids[j]
      wid <- as.vector(append(wid, c(rep(value, times))))
    }
    #1 is partcor, 2 is cor, 3 is MI, 4 is ranked, 5 is percentile.
    ifelse(type == 1, EdgeTable <- mutate(EdgeTable, width=sigmoid_xB(x=nthroot(abs(Weight), 3), B=3)),
           ifelse(type == 2, EdgeTable <- mutate(EdgeTable, width=sigmoid_xB(x=abs(Weight), B=3)),
                  ifelse(type == 3, EdgeTable <- mutate(EdgeTable, width=sigmoid_xB(x=(abs(Weight)/max(abs(Adj))), B=3)),
                         ifelse(type == 4, EdgeTable <- mutate(EdgeTable, width = sigmoid_xB(x=(Rank(-Weight)/E), B=3)),
                                if(type == 5){
                                  wid <- as.data.frame(wid)
                                  EdgeTable <- EdgeTable[sort(abs(EdgeTable$Weight), decreasing=T, index.return=T)[[2]],]
                                  EdgeTable <- cbind(EdgeTable, wid)
                                  colnames(EdgeTable)[5] <- "width"
                                }
                                else{
                                  print("type not selected")
                                }))))


    ct = NULL
    if (min(as.vector(Weight)) < 0) ct = 1 else ct = 2

    cp1 <- as.vector(diverging_hcl(n=nrow(EdgeTable), palette = "Blue-Red"))
    cp2 <- as.vector(sequential_hcl(n=nrow(EdgeTable), palette = "Reds2"))

    if(ct == 1){
      EdgeTable <- EdgeTable[sort(EdgeTable$Weight, decreasing=T, index.return=T)[[2]],]
      EdgeTable <- cbind(EdgeTable, cp1)
      colnames(EdgeTable)[6] <- "Stroke"
    }else{
      EdgeTable <- EdgeTable[sort(abs(EdgeTable$Weight), decreasing=T, index.return=T)[[2]],]
      EdgeTable <- cbind(EdgeTable, cp2)
      colnames(EdgeTable)[6] <- "Stroke"
    }

    EdgeTable$sharedname <- paste(EdgeTable$Source, "(interacts)", EdgeTable$Target)

    Network_name = sprintf("Visual_Network_%i", n_matrix)
    Network_Collection = sprintf("Visual_Networks_%i", n_matrix)

    nodes = NULL
    edges = NULL

    nodes <- data.frame(id=as.vector(NodeTable$Node), group=as.vector(NodeTable$Groups), stringsAsFactors = FALSE)
    edges <- data.frame(source=as.vector(EdgeTable$Source), target=as.vector(EdgeTable$Target), interaction=as.vector(EdgeTable$Interaction), weight=as.vector(EdgeTable$Weight), stringsAsFactors = FALSE)

    createNetworkFromDataFrames(nodes, edges, title=Network_name, collection=Network_Collection, style.name = "SanjeeNetworkStyle")


    Colour_palette <- as.vector(c("#0073C2", "#EFC000", "#868686", "#CD534C", "#7AA6DC", "#003C6799", "#8F7700", "#3B3B3B", "#A73030", "#4A6990"))

    style.name = "SanjeeNetworkStyle"
    defaults <- list(NODE_SHAPE="Ellipse", NODE_SIZE=25.0, EDGE_TRANSPARENCY=255 , NODE_LABEL_POSITION="W,E,c,0.00,0.00", NODE_BORDER_PAINT="#FFFFFF")
    nodeLabels <- mapVisualProperty("Node Label", "id", "p")
    nodecolour <- mapVisualProperty("Node Fill Color", "group", "d", as.vector(unique(NodeTable$Groups)), as.vector(Colour_palette[1:length(unique(NodeTable$Groups))]))
    nodeXlocation <- mapVisualProperty("Node X Location", "id", "d", as.vector(NodeTable$Node), as.vector(NodeTable$X))
    nodeYlocation <- mapVisualProperty("Node Y Location", "id", "d", as.vector(NodeTable$Node), as.vector(NodeTable$Y))
    edgeline <- mapVisualProperty("Edge Line Type", "interaction", "d", as.vector(unique(EdgeTable$Interaction)), as.vector(c("Solid")))
    edgewidth <- mapVisualProperty("Edge Width", "shared name", "d", as.vector(EdgeTable$sharedname), as.vector(EdgeTable$width))
    edgestroke <- mapVisualProperty("Edge Stroke Unselected Paint", "shared name", "d", as.vector(EdgeTable$sharedname), as.vector(EdgeTable$Stroke))

    createVisualStyle(style.name, defaults, list(nodeLabels, nodecolour, nodeXlocation, nodeYlocation, edgeline, edgewidth, edgestroke))
    setVisualStyle("SanjeeNetworkStyle")

    fitContent(selected.only = FALSE)
    fitContent(selected.only = FALSE)
    fitContent(selected.only = FALSE)

    Network_out = sprintf("Network_Image_%i", n_matrix)

    full.path = paste(getwd(), Network_out, sep="/")
    exportImage(full.path, "PNG", units="pixels", width=3600, height=1771)

    #Network files for building network using some other software
    AdjMatrix <- list(AdjMatrix, Adjacency)
    NodesNetwork <- list(NodesNetwork, NodeTable)
    EdgesNetwork <- list(EdgesNetwork, EdgeTable)

    Network_save = sprintf("Cytoscape_Network_%i", n_matrix)
    full.path.cps = paste(getwd(), Network_save, sep="/")
    closeSession(save.before.closing = TRUE, filename = full.path.cps)

  }


  Network = list(AdjMatrix, NodesNetwork, EdgesNetwork)
  return(Network)

}

sigmoid_xB <- function(x, B){
  T = (1/(1+((x/(1-x))^-B)))
  return(T)
}

# Run example
library(dplyr)        # For data wrangling, e.g. using mutate
library(RCy3)         # For communicating with cytoscape
library(pracma)       # To use the nthroot function
library(colorspace)   # To adjust color palettes

Mat1 <- readRDS("tests/trail_adjacency_matrix.rds")

# A <- list(Mat1, Mat2, Mat3)
# G <- as.vector(c(label1, label2, label3...., labeln))

group_vec <- rep("A", times = nrow(Mat1))

group_vec[names(Mat1) |> stringr::str_detect("IL")] <- "B"
group_vec[names(Mat1) |> stringr::str_detect("CCL")] <- "C"
group_vec[names(Mat1) |> stringr::str_detect("CXCL")] <- "D"

Visual <- VisualiseNetwork(Mat1[1:3, 1:2],
                           Group = TRUE,
                           group_vec = group_vec,
                           type = 1)

