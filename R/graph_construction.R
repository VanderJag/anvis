#' Adjacency matrix to edgelist
#'
#' Converts an adjacency matrix into an weighted edgelist. Interactions of nodes
#' with themselves are removed. Assumes the input in undirectional
#'
#' @param df Data frame or matrix. Square adjacency matrix. Nodes must be specified
#'   in rownames and column names.
#' @return Returns data frame representing a network as weighted edge list. Columns are
#'   Source, Target, and Weight.
#'
#' @examples
#' nodes <- c(paste0("IL", 1:5), paste0("CCL", 1:3), paste0("CXCL", 1:4))
#' n_nodes <- length(nodes)
#'
#' interactions <- (runif(n_nodes**2)) |>
#'   matrix(nrow = n_nodes, ncol = n_nodes)
#'
#' interactions[upper.tri(interactions, diag=FALSE)] <-
#'     t(interactions)[upper.tri(t(interactions), diag=FALSE)]
#'
#' row.names(interactions) <- nodes
#' colnames(interactions) <- nodes
#'
#' for(i in 1:n_nodes) {
#'   interactions[i,i] <- 1
#' }
#'
#' adj_matrix_to_edgelist(interactions)
#'
adj_matrix_to_edgelist <- function(df) {

  # If the upper and lower diagonal are not the same we would be discarding information
  is_equal <- isTRUE(all.equal(df[upper.tri(df, diag=FALSE)],
                               t(df)[upper.tri(t(df), diag=FALSE)]))
  if(!is_equal) {
    stop("Must provide adjacency matrix with identical upper and lower triangle.",
         "\nℹ Check your adj. matrix with `all.equal(adj[upper.tri(adj, diag=FALSE)], ",
         "t(adj)[upper.tri(t(adj), diag=FALSE)])`",
         "\n✖ Lower triangle can't be discarded since it doesn't match upper triangle.",
         call.=FALSE)
  }

  # It is assumed that there is no interest in self intaction, and that the
  #   upper and lower triangles of the adjacency matrix are identical.
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
}


adj_matrix_to_nodetable <- function(df, group_vec = NULL) {

  data.frame("Node" = colnames(df))
}


adj_matrix_to_network <- function(adj_matrix, group_vec, width_type) {
  # If the number and column of the adjacency matrix is not equal there may be
  #   missing info and an error with the data input
  if (ncol(adj_matrix) != nrow(adj_matrix)) {
    stop("Adjacency matrix should be a square matrix with equal number of rows ",
         "and columns", call. = FALSE)
  }

  # Incomplete group information can not be correctly assigned
  if (nrow(adj_matrix) != length(group_vec)) {
    stop("The number of nodes/variables in the groups table should be the same ",
         "as in the adjacency matrix", call. = FALSE)
  }


  # Node table with group info ----------------------------------------------

  node_table <- adj_matrix_to_nodetable(adj_matrix)

  # Adding grouping information ---------------------------------------------

  node_table <- group_nodes(node_table, group_vec = group_vec)

  # Cytoscape node positions ------------------------------------------------

  node_table <- add_node_pos(node_table)


  # adjecency to edgelist ---------------------------------------------------

  edge_table <- adj_matrix_to_edgelist(adj_matrix)

  # Convert edge weight to edge width ---------------------------------------

  edge_table <- edge_weight_to_widths(edge_table, type = width_type)

  # Add colour column to edge table -----------------------------------------

  edge_table <- weights_to_color(edge_table)

  return(list("edge_table" = edge_table,
              "node_table" = node_table))
}
