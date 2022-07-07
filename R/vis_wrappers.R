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
VisualiseNetwork <- function(df_adjacency, group_vec = NULL, width_type = NULL,
                             do_save = T, save_names = NULL) {
  # TODO allow user to manually specify group colors

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

    # Create a network from adjacency info
    network_list <- adj_matrix_to_network(Adjacency,
                                          group_vec = group_vec,
                                          width_type = width_type)
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

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
