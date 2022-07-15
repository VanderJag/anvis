#' Create formatted circular network
#'
#' This function creates a visualization for the input network
#' information. The present implementation creates a network in which nodes are
#' arranged in a circle. To highlight relevant interactions, there are different
#' options available that determine the scaling of the weights in the network.
#'
#' @param adj_mats A square adjacency matrix or data frame or a list of these. The data
#'   should reprensed the strength of the relation between what will be the nodes in
#'   the network. Rownames and column names are required.
#' @param group_vec A vector of character strings that assigns group labels to
#'   the nodes. The order of this vector should match the order of column and
#'   rownames of the input
#'   adjacency matrices. If `adj_mats` is a list, a single group vector can be
#'   used if it matches all adjacency matrices. Alternatively, provide a list of
#'   group vectors with one vector for each adj. matrix in the list.
#' @return The section on the returned values
#'
#' @section Additional criteria for the use of this function:
#' * The Cytoscape software needs to be running.
#' * ...
VisualiseNetwork <- function(adj_mats,
                             group_vec = NULL,
                             vis_type = c("igraph", "cytoscape"),
                             width_type = NULL,
                             do_save = T, save_names = NULL) {
  # TODO allow user to manually specify group colors
  # TODO add option to scale igraph widths linearly
  # TODO add additional arguments of the adj_matrix_to_network

  # Check which visualization should be used, allow abbreviations
  vis_type <- match.arg(vis_type)


  # Since this function uses a for loop to iterate over the visualizations that
  #   are created, the input needs to be converted into a list.
  if(inherits(adj_mats, "data.frame") == TRUE |
     inherits(adj_mats, "matrix") == TRUE) {
    adj_mats <- list(adj_mats)
  } else if (!inherits(adj_mats, "list")) {
    stop("Must provide data.frame, matrix, or list of these as input \n",
         "You provided: ", class(adj_mats), call. = FALSE)
  }

  # TODO make sure group vector is of length 1 or same as the list of adj_mats
  #   then adjust below so it uses the correct group vec, maybe if len(vec) is 1 turn into list and then i can do group_vec[[x]] %||% group_vec[[1]]
  # Convert all adjacency matrices into edge and node tables
  networks <- lapply(seq_along(adj_mats),
                     function(x, ...) adj_matrix_to_network(adj_mats[[x]], ...),
                                                               node_attrs = "all",
                                                               edge_attrs = "all",
                                                               group_vec = group_vec,
                                                               width_type = width_type)
  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$node_table)
  edges <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$node_table)


  # These variables will be used to store all prepared and rescaled networks
  #   and return them in the end of this function
  AdjMatrix = NULL
  NodesNetwork = NULL
  EdgesNetwork = NULL

  # Main loop to iterate over the multiple networks that can be provided as input
  for (i_matrix in 1:length(adj_mats)) {

    # These variable store are used to create the current network in edgelist format
    Adjacency = as.data.frame(adj_mats[i_matrix])

    # Create a network from adjacency info
    network_list <- adj_matrix_to_network(Adjacency,
                                          node_attrs = "all",
                                          edge_attrs = "all",
                                          group_vec = group_vec,
                                          width_type = width_type)
    edge_table <- network_list[["edge_table"]]
    node_table <- network_list[["node_table"]]

    # Choose visualization
    if (vis_type == "igraph") {
      vis_igraph(edge_table = edge_table,
                 node_table = node_table)
    } else if (vis_type == "cytoscape") {
      vis_in_cytoscape(edge_table = edge_table,
                       node_table = node_table,
                       netw_nr = i_matrix)
    }


    # Network files for building network using some other software
    AdjMatrix <- list(AdjMatrix, Adjacency)
    NodesNetwork <- list(NodesNetwork, node_table)
    EdgesNetwork <- list(EdgesNetwork, edge_table)
  }

  Network = list(AdjMatrix, NodesNetwork, EdgesNetwork)
  return(Network)
}
