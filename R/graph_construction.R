#' Adjacency matrix to edge list
#'
#' Converts an adjacency matrix into an weighted edge list. Interactions of nodes
#' with themselves are removed. Assumes the matrix to be symmetric over the
#' diagonal, the resulting edges will be undirected.
#'
#' @param adj_matrix Data frame or matrix. Square adjacency matrix. Nodes must be
#'     specified in row names and column names.
#' @return Returns data frame representing a network as weighted edge list.
#'     Columns are source, target, and weight.
#'
#' @export
adj_matrix_to_edgelist <- function(adj_matrix, directed = FALSE) {

    # If the number and column of the adjacency matrix is not equal there may be
    #   missing info and an error with the data input
    if (ncol(adj_matrix) != nrow(adj_matrix)) {
        stop("Adjacency matrix should be a square matrix with equal number of rows ",
             "and columns", call. = FALSE)
    }

    # Column names and row names are required to determine connected vertices
    if (is.null(colnames(adj_matrix))) {
        warning("No column names found for adjacency matrix.",
                "\nNumbers have been set as column names: 1 2 3 ...",
                "\nℹ to set column names use e.g.: colnames(adj_mat) <- names",
                call.=FALSE)

        colnames(adj_matrix) <- 1:ncol(adj_matrix)
    }

    # Column names and row names are required to determine connected vertices
    if (is.null(row.names(adj_matrix))) {
        warning("No row names found found for adjacency matrix.",
                "\nColumn names are now used as rownames.",
                "\nℹ to set row names use e.g.: ",
                "row.names(adj_mat) <- colnames(adj_mat)",
                call.=FALSE)

        row.names(adj_matrix) <- colnames(adj_matrix)
    }

    # It is assumed that there is no interest in self interaction
    diag(adj_matrix) <- 0

    if (isFALSE(directed)) {
        # If the upper and lower diagonal are not the same we would be
        #     discarding information
        is_equal <- isTRUE(all.equal(adj_matrix[upper.tri(adj_matrix, diag=FALSE)],
                                     t(adj_matrix)[
                                         upper.tri(t(adj_matrix), diag=FALSE)]))
        if(!is_equal) {
            stop("Must provide adjacency matrix with identical upper ",
                 "and lower triangle.",
                 "\nℹ Check your adj. matrix with ",
                 "`all.equal(adj[upper.tri(adj, diag=FALSE)], ",
                 "t(adj)[upper.tri(t(adj), diag=FALSE)])`",
                 "\n✖ Lower triangle can't be discarded since it",
                 "doesn't match upper triangle.",
                 call.=FALSE)
        }

        # Since the information of the upper and lower triangles of the adjacency
        #     matrix are identical, the lower triangle can be discarded
        adj_matrix[lower.tri(adj_matrix, diag=TRUE)] <- 0
    }

    # Prepare to convert to long format by making the rownames a column of their own
    adj_matrix <- adj_matrix %>%
        tibble::as_tibble(rownames = "source")

    # Convert into the edgelist format
    edgelist <- tidyr::pivot_longer(adj_matrix, cols = -source,
                                    names_to = "target", values_to = "weight") %>%
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
#'
#' @export
adj_matrix_to_nodetable <- function(adj_matrix) {

  data.frame("node" = colnames(adj_matrix))
}


#' Create edge and node table from adjacency matrix
#'
#' Creates edge and node tables with various optional attributes.
#'
#' When `node_attrs` or `edge_attrs` is a vector containing 'none' and any other
#' option, no additional attributes will be added.
#'
#' When selecting 'group' as node attribute `group_vec` becomes a required
#' argument. When selecting 'color_group' as a node attribute, 'group' must also
#' be selected. The colors for the groups can be adjusted with `group_colors`,
#' but this is optional. When selecting 'size' as node attribute the range of
#' node sizes can be adjusted with the `size_type` argument (when this argument
#' is NULL the nodes sizes will be set to work neatly for igraph visualization).
#' When 'width' is added as edge attribute, it is wise to check the options for
#' `width_type`, to obtain width that show the most logical contrasts for your
#' data. `width_type = NULL` (default), will scale the data to range 0 to 1 and
#' apply a sigmoid, so the lowest edge weights will have even lower width, and
#' higher weight maintain high values.
#'
#' @inheritParams adj_matrix_to_edgelist
#' @param node_attrs Character strings, one or multiple of 'none', 'all', 'group',
#'     'color_group', and 'size'. This argument can be used to choose which
#'     additional attributes should be added to the node table. Selecting 'group'
#'     will append a column to the node table that corresponds to `group_vec`.
#'     Selecting 'color_group' in addition to 'group' will add a column that
#'     contains a color for each node, selected based on the group of this node.
#'     Selecting 'size' will add a column with node sizes, that are based on the
#'     connectivity of the nodes.
#' @param edge_attrs Character strings, one or multiple of 'none, 'all', 'width',
#'   'color'. This argument can be used to choose which additional attributes
#'   should be added to the edge table. Selecting 'width' will add a column to
#'   edge table that contains edge width values. These are determined by scaling
#'   the edge weights to range 0 to 1 using a sigmoid and a method based on
#'   `width_type` (more info in [edge_weight_to_widths]).
#' @inheritParams group_nodes
#' @inheritParams add_colors
#' @inheritParams node_size_connectivity
#' @inheritParams edge_weight_to_widths
#' @inheritParams weights_to_color
#'
#' @return Will return a list with two data frames, named 'edge_table' and
#' 'node_table'. The 'node_table' data frame has a 'node' column and other columns
#' for the additional attributes that were added. The 'edge_table' has columns
#' 'source' and 'target', and more columns for the added attributes.
#'
#' @export
adj_matrix_to_network <- function(adj_matrix,
                                  node_attrs = c("none", "all", "group", "color_group", "size"),
                                  edge_attrs = c("none", "all", "width", "color"),
                                  group_vec = NULL,
                                  group_colors = NULL,
                                  size_type = NULL,
                                  width_type = NULL,
                                  edge_color_func = NULL) {

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
    edge_table <- weights_to_color(edge_table, edge_color_func = edge_color_func)
  }

  return(list("edge_table" = edge_table,
              "node_table" = node_table))
}


#' Convert edge list to adjacency matrix
#'
#' Creates an adjacency matrix from edgelist.
#'
#' @param edge_list Data frame. The first two columns should describe the
#'   connected nodes. Another column with edge weights can be used by this
#'   function. Any additional columns/attributes of the edges will be lost.
#' @param weight_col Character string, the name of the column that contains edge
#'   weights (default: 'weight'). If this argument is NULL all edges will have
#'   weight 1 in the adjacency matrix.
#'
#' @return Returns a square adjacency matrix with column and row names representing
#'   the interacting nodes. The matrix will by symmetric along the diagonal.
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
