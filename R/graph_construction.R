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


#' Create node table from adjacency matrix
#'
#' Extracts column names from the adjacency matrix and creates a basic node table
#' with the column 'node'.
#'
#' @param adj_matrix An adjacency matrix (data frame or matrix), with column
#'   names.
#' @return Returns a data frame with a single column: 'node', which holds the
#'   names of the network's nodes.
adj_matrix_to_nodetable <- function(adj_matrix) {

  data.frame("node" = colnames(adj_matrix))
}


# TODO finish this documentation
# use color_group in combination with group
# NULL for size_type and for width_type will use the default arguments of the fucntions

#' Create edge and node table from adjacency matrix
#'
#' Creates edge and node tables with various optional attributes.
#'
#' When `node_attrs` or `edge_attrs` is a vector containing 'none' and any other
#' option, no additional attributes will be added.
#' # TODO write about the options for attributes, for node and edge individually
#' # TODO integrate choice of your own color
#'
#' @inheritParams adj_matrix_to_edgelist
#' @param node_attrs Character strings, one or multiple of 'none', 'all', 'group',
#'   'color_group', and 'size'.
#' @param edge_attrs Character strings, one or multiple of 'none, 'all', 'width',
#'   'color'.
#' @inheritParams group_nodes
#' @inheritParams node_size_connectivity
#' @inheritParams edge_weight_to_widths
#' @return The section on the returned values
adj_matrix_to_network <- function(adj_matrix,
                                  node_attrs = c("none", "all", "group", "color_group", "size"),
                                  edge_attrs = c("none", "all", "width", "color"),
                                  group_vec = NULL,
                                  size_type = NULL,
                                  width_type = NULL,
                                  group_colors = NULL) {

  # Check which attributes should be added
  node_attrs <- match.arg(node_attrs, several.ok = TRUE)
  edge_attrs <- match.arg(edge_attrs, several.ok = TRUE)

  # If the number and column of the adjacency matrix is not equal there may be
  #   missing info and an error with the data input
  if (ncol(adj_matrix) != nrow(adj_matrix)) {
    stop("Adjacency matrix should be a square matrix with equal number of rows ",
         "and columns", call. = FALSE)
  }

  # Node table with group info ----------------------------------------------

  node_table <- adj_matrix_to_nodetable(adj_matrix)

  # Adding grouping information ---------------------------------------------

  if ((!"none" %in% node_attrs) &
      ("all" %in% node_attrs | "group" %in% node_attrs)) {
    if (is.null(group_vec)) {
      stop("Must provide grouping vector:",
           "\n✖ `group_vec` should not be NULL when `node_attrs` is 'all' or 'group'.",
           call.=FALSE)
    }
    node_table <- group_nodes(node_table, group_vec = group_vec)
  }

  # Add colors for the groups -----------------------------------------------

  if ((!"none" %in% node_attrs) &
      ("all" %in% node_attrs | "color_group" %in% node_attrs)) {
    node_table <- add_colors(node_table, group_colors = group_colors)
  }


  # Add node size -----------------------------------------------------------

  if ((!"none" %in% node_attrs) &
      ("all" %in% node_attrs | "size" %in% node_attrs)) {
    node_table <- node_size_connectivity(node_table = node_table,
                                         adj_matrix = adj_matrix,
                                         size_type = size_type)
  }


  # adjecency to edgelist ---------------------------------------------------

  edge_table <- adj_matrix_to_edgelist(adj_matrix)

  # Convert edge weight to edge width ---------------------------------------

  if ((!"none" %in% edge_attrs) &
      ("all" %in% edge_attrs | "width" %in% edge_attrs)) {
    edge_table <- edge_weight_to_widths(edge_table, width_type = width_type)
  }

  # Add colour column to edge table -----------------------------------------

  if ((!"none" %in% edge_attrs) &
      ("all" %in% edge_attrs | "color" %in% edge_attrs)) {
    edge_table <- weights_to_color(edge_table)
  }

  return(list("edge_table" = edge_table,
              "node_table" = node_table))
}


edgelist_to_adj <- function(edge_list, weight_col = "weight") {
    # Check if a weight column exists with the correct name
    if (!is.null(weight_col) && !(weight_col %in% colnames(edge_list))) {
        stop("Weight column name must be NULL or present in names of edge_list: ",
             "\nℹ your `weight_col`: ",
             weight_col,
             "\n  `colnames(edge_list)`: ",
             colnames(edge_list) %>% paste(collapse = ", "),
             ".", call.=FALSE)
    }

    # Igraph has a function to convert it's objects into adj, create igraph obj
    graph <- igraph::graph_from_data_frame(edge_list,
                                           directed = FALSE)

    adj <- igraph::as_adjacency_matrix(graph = graph, type = "both", attr = weight_col,
                                       sparse = F)

    return(adj)
}
