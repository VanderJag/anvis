#' Adjacency matrix to edge list
#'
#' Converts an adjacency matrix into an weighted edge list. Interactions of nodes
#' with themselves are removed. Assumes the matrix to be symmetric over the
#' diagonal, the resulting edges will be undirected.
#'
#' @param adj_matrix Data frame or matrix. Square adjacency matrix. Nodes must be specified
#'   in row names and column names.
#' @return Returns data frame representing a network as weighted edge list. Columns are
#'   source, target, and weight.
adj_matrix_to_edgelist <- function(adj_matrix) {
  # Column names and row names are required to determine connected vertices
  if (is.null(colnames(adj_matrix))) {
    warning("No column names found for adjacency matrix.",
            "\nNumbers have been set as column names: 1 2 3 ...",
            "\nℹ to set column names use e.g.: colnames(adj_mat) <- names", call.=FALSE)

    colnames(adj_matrix) <- 1:ncol(adj_matrix)
  }

  # Column names and row names are required to determine connected vertices
  if (is.null(row.names(adj_matrix))) {
    warning("No row names found found for adjacency matrix.",
            "\nColumn names are now used as rownames.",
            "\nℹ to set row names use e.g.: row.names(adj_mat) <- colnames(adj_mat)", call.=FALSE)

    row.names(adj_matrix) <- colnames(adj_matrix)
  }

  # If the upper and lower diagonal are not the same we would be discarding information
  is_equal <- isTRUE(all.equal(adj_matrix[upper.tri(adj_matrix, diag=FALSE)],
                               t(adj_matrix)[upper.tri(t(adj_matrix), diag=FALSE)]))
  if(!is_equal) {
    stop("Must provide adjacency matrix with identical upper and lower triangle.",
         "\nℹ Check your adj. matrix with `all.equal(adj[upper.tri(adj, diag=FALSE)], ",
         "t(adj)[upper.tri(t(adj), diag=FALSE)])`",
         "\n✖ Lower triangle can't be discarded since it doesn't match upper triangle.",
         call.=FALSE)
  }

  # It is assumed that there is no interest in self intaction, and that the
  #   upper and lower triangles of the adjacency matrix are identical.
  diag(adj_matrix) = 0
  adj_matrix[lower.tri(adj_matrix, diag=TRUE)] <- 0

  # Prepare to convert to long format by making the rownames a column of their own
  adj_matrix <- adj_matrix %>%
    tibble::as_tibble(rownames = "source")

  # Convert into the edgelist format
  edgelist <- tidyr::pivot_longer(adj_matrix, cols = -source, names_to = "target", values_to = "weight") %>%
    # Self interaction and dulicate info from lower triangle have been set to 0
    dplyr::filter(weight != 0)

  return(edgelist)
}


adj_matrix_to_nodetable <- function(adj_matrix, group_vec = NULL) {

  data.frame("node" = colnames(adj_matrix))
}


adj_matrix_to_network <- function(adj_matrix, group_vec, width_type, add_color = T) {
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

  # Add colors for the groups -----------------------------------------------

  node_table <- add_colors(node_table)

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
