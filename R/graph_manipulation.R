#' Add group column and sort node table
#'
#' Adds 'group' column to `node_table` with values corresponding to  `group_vec`.
#' The rows of `node_table` will be reordered so the group column is sorted
#' alphabetically.
#'
#' @param node_table A data.frame in which rows correspond to the nodes of a
#'   network.
#' @param group_vec A vector contains group labels for each row in `node_table`.
#' @return Returns `node_table` with 'group' column added, sorted by group.
group_nodes <- function(node_table, group_vec) {

  group_vec <- as.character(group_vec)

  # Incomplete group information can not be correctly assigned
  if (nrow(node_table) != length(group_vec)) {
    stop("`Must provide matching `node_table` and `group_vec`:",
    "\nℹ Number of rows for `node_table`: ", nrow(node_table),
    ", length of `group_vec`: ", length(group_vec),
    ".\n✖ number of rows of `node_table` and length of `group_vec` must match.",
    call.=FALSE)
  }

  # Add grouping vector as column and sort the rows
  node_table$group <- group_vec
  node_table <- node_table[order(node_table$group),]
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


add_colors <- function(node_table) {
  # Group information is required to add group coloring
  if (!"group" %in% colnames(node_table)) {
    stop("Must provide node table with group info:",
         "\nℹ Your node table contains these columns: ",
         colnames(node_table) %>% paste(collapse = ", "),
         ".\n✖ `colnames(node_table)` must include 'group'.",
         call.=FALSE)
  }

  # extract the unique groups
  groups <- node_table$group %>% unique()

  # same number of colors is required as group count
  colors <- n_distinct_cols(length(groups))

  # index which group is assigned to which row
  idx <- match(node_table$group, groups)

  # Use group index to select correct colors for each row
  node_table$color <- colors[idx]

  return(node_table)
}


#' Add connectivity based 'size' column to node table
#'
#' Calculates the connectivity of nodes as the row sums from an adjacency matrix.
#' For use as node size, the values are scaled to range 0 to 1, transformed
#' with a sigmoid, and then multiplied with a constant factor that is determined
#' by `size_type`
#'
#' @param node_table A data frame with a column named 'node'. The values in this
#'   column (names of nodes), must be the same as the row names
#'   of `adj_matrix`. Having nodes in the same order in `node_table` and the
#'   matrix not required.
#' @param size_type One of "igraph" (default), "cytoscape", or "scaled_only".
#'   This argument determines a factor for linear scaling of node size. Factors
#'   are 15, 25, and 1, respectively.
#' @return Returns `node_table` with an added column 'size'.
#'
#' @inheritParams adj_matrix_to_edgelist
node_size_connectivity <- function(node_table,
                                   adj_matrix,
                                   size_type = c("igraph", "cytoscape", "scaled_only")) {

  # Matching argument, allow abbreviation
  size_type <- match.arg(size_type)

  # Check if the same nodes are present in node table and the adjacency matrix
  only_nodes <- setdiff(node_table$node, colnames(adj_matrix))
  only_adj <- setdiff(colnames(adj_matrix), node_table$node)
  if (length(c(only_adj, only_nodes)) != 0) {
    stop("Must provide node_table and adj_matrix with the same nodes.",
    "\nℹ Unique elements in node_table$node: ", only_nodes %>% paste(collapse = ", "),
    ", unique elements in names(adj_matrix): ", only_adj %>% paste(collapse = ", "),
    ".\n✖ node_table$nodes and colnames(adj_matrix) should contain the same elements.",
    call.=FALSE)
  }

  # Connections to self are omitted in this function
  diag(adj_matrix) <- 0

  connectivity <- rowSums(abs(adj_matrix))

  # Scale to range 0 to 1, required for sigmoid
  scale_conn <- connectivity / max(connectivity)

  # Transform with sigmoid to emphasize the constrasts
  scale_conn <- sigmoid_xB(scale_conn, 3)

  # Match with default node size for the visualization
  if (size_type == "igraph") scale_conn <- scale_conn * 15
  if (size_type == "cytoscape") scale_conn <- scale_conn * 25
  # if (size_type == "scaled_only") scale_conn <- scale_conn

  # Make sure we can add the node size in correct order
  match_idx <- match(node_table$node, names(scale_conn))

  # Add to node table
  node_table$size <- scale_conn[match_idx]

  return(node_table)
}

sort_avg_connectivity <- function(nodes_list) {
  col_in_colnames <- function(list_item, col) {
    col_names <- colnames(list_item)
    any(stringr::str_detect(col_names, col))
  }

  has_connectivity <- sapply(nodes_list, col_in_colnames, "size")

  if (!any(has_connectivity)) {
    stop("Must provide data frames with 'size' connectivity column: ",
         "\n✖ Node tables require size column to calculate average connectivity.",
         call.=FALSE)
  } else if (!all(has_connectivity)) {
    warning("Not all networks have node size attribute: ",
            "\nAverage connectivity will be calculated without the missing info.",
            call. = FALSE)
  }

  # Check if all node tables have a column called 'node', needed to match rows
  has_nodenames <- sapply(nodes_list, col_in_colnames, "node")

  if (!all(has_nodenames)) {
    stop("Must provide node names to calculate average connectivity:",
    "\nℹ Indices of your node tables not containing 'node' column: ",
    which(!has_nodenames) %>% paste(collapse = " "),
    "\n✖ All node tables must contain 'node' column.", call.=FALSE)
  }

  # To avoid unexpected results, test if the same nodes are found in all tables
  same_nodes <- sapply(nodes_list,
                       function (df_nodes) setequal(nodes_list[[1]]$node,
                                                    df_nodes$node))

  if (!all(same_nodes)) {
    stop("Must have the same nodes in node tables:",
    "\nℹ Indices of node tables not matching with the first node table: ",
    which(!same_nodes) %>% paste(collapse = " "),
    "\n✖ All node tables must have the same values in their 'node' column.", call.=FALSE)
  }
    # TODO check if nodes are in the list same or not: error
  # there might be a different order of the nodes, use the first as reference
  #   to prevent averaging different nodes
  # TODO Check if groups are the same

  ref_nodes <- nodes_list[[1]]$node
  # It's possible that not all node_tables have the size attribute
  conn_tables <- nodes_list[has_connectivity]

  # Extract the connectivity scores from the tables that hold them
  connectivities <- sapply(seq_along(conn_tables),
         function(x) {
           nodes_i <- conn_tables[[x]]
           nodes_i[match(ref_nodes, nodes_i$node),]$size
         }
  )

  # Calculate average connectivities
  avg_conns <- rowMeans(connectivities) %>%
    tibble::as_tibble_col(column_name = "conns")

  # Add the node names to the averages
  avg_conns$node <- ref_nodes

  # Sort and keep groups together if they are present
  if ("group" %in% colnames(nodes_list[[1]])) {
    avg_ordered <- avg_conns %>%
      dplyr::mutate(group = nodes_list[[1]]$group) %>%
      dplyr::arrange(group, conns)
  } else {
    avg_ordered <- dplyr::arrange(avg_conns, conns)
  }

  node_order <- avg_ordered$node

  nodes_list <- lapply(nodes_list,
                       function(nodes_i) nodes_i[match(node_order, nodes_i$node),]
  )

  return(nodes_list)
}


#' Calculate edge widths
#'
#' Adds a column called "width" to the input `edge_table`. The widths for the
#' edges can be calculated using five different methods. Which method is most
#' appropriate depend on the range of the input data. For more details on the
#' choice of the method check `width_type` in the Arguments section.
#'
#' The methods have in common that they relate to the absolute values of the column
#' "weight", which is a required argument. The most extreme values for "weight"
#' get the highest value for edge width. The range of edge widths will be between
#' 0 and 1, irrespective of the chosen method.
#'
#' @param edge_table A data frame with a column called "weight" containing
#'   numeric values.
#' @param width_type A character string that determines the method to be used for
#'   converting edge weight to edge width. Options are: `"cor"`, which is
#'   intended to be used with Pearson or Spearman correlation values (range -1
#'   to 1). Widths will be the absolute value of the correlation, scaled with a
#'   sigmoid. `"partcor"`, which is intended to be use for partial correlation
#'   values (range -1 to 1). For this method, edge widths will be the cube root
#'   of absolute weight values, scaled with a sigmoid. The option `"MI"` is meant
#'   to be used with weights derived from mutual information (range 0 to +∞).
#'   Widths will be the weights divided by to maximum weight, then scaled with
#'   a sigmoid. `"default_scaling"` applies the same transformation as `"MI"`,
#'   as a result scaling the any weights to range 0 to 1.
#'   `"ranked"` will calculate percentage ranks of the weight, and
#'   scale them with a sigmoid. `"percentile"` will chose a set of fixed widths
#'   determined by the percentile of a weight value. The highest percentiles will
#'   be assigned the largest width, but they are the smallest group, vice versa
#'   for the lowest percentiles. This argument can be abbreviated.
#' @return Return the input data frame with an added column "width", which can
#'   be used to scale edges in a network visualization.
edge_weight_to_widths <- function(edge_table,
                                  width_type = c("default_scaling", "MI", "cor",
                                                 "partcor", "ranked", "percentile")) {

  # See if the width_type is one of the expected choices, this allows
  #   partial matching and will throw an error for invalid arguments
  width_type <- match.arg(width_type)

  # A column named 'weight' is required to calculate the edge widths
  if (!"weight" %in% colnames(edge_table)) {
    stop("Must provide edge table with weights:",
    "\nℹ Your edge table contains these columns: ",
    colnames(edge_table) %>% paste(collapse = ", "),
    ".\n✖ `colnames(edge_table)` must include 'weight'.",
    call.=FALSE)
  }

  # Unexpected results may be returned when the range of the data does not match
  #   with the selected width type
  if (width_type %in% c("partcor", "cor") &
      (max(edge_table$weight) > 1 | min(edge_table$weight) < -1)) {
    warning("'partcor' or 'cor' width ",
            "types expect edge weight range -1 to 1, while your data has range: ",
            range(edge_table$weight) %>% paste(collapse = " to "), ". ",
            "Unexpected results may be returned for edge widths.")
  }

  if (width_type == "MI" &
      (min(edge_table$weight) < 0)) {
    warning("'MI' width ",
            "type expect edge weight range 0 to +∞, while your data has range: ",
            range(edge_table$weight) %>% paste(collapse = " to "), ". ",
            "Unexpected results may be returned for edge widths.")
  }

  # Warning for overwriting data
  if ("width" %in% colnames(edge_table)) {
    warning("A column with the name 'width' is already found in `edge_table`,",
            "this column will be overwritten.", call. = F)
  }

  # number of edges is used in some calculations of edge width
  n_edges = nrow(edge_table)

  # Change the weights to widths according to type
  if (width_type == "partcor") {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = nthroot(abs(weight), 3), B = 3))
  } else if (width_type == "cor") {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = abs(weight), B = 3))
  } else if (width_type == "MI" | width_type == "default_scaling") {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = (abs(weight) / max(abs(weight))), B = 3))
  } else if (width_type == "ranked") {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = (rank(abs(weight)) / n_edges), B = 3))
  } else if (width_type == "percentile") {
    # Get percentile widths (descending order)
    width_col <- data.frame("width" = percentile_widths(n_edges = n_edges))
    # Get index for edges ordered by weight, this is done with ranks,
    #   for ties 'min' is chosen to provide the tied edges with the lower of the
    #   two options for their rank
    widths_idx <- rank(-abs(edge_table$weight), ties.method = "min")
    # Combine edges and widths
    edge_table <- dplyr::bind_cols(edge_table, "width" = width_col[widths_idx,])
  }

  return(edge_table)
}


#' Edge width based on percentiles
#'
#' Creates a vector of edge widths that matched with the number of edges. Edges
#' widths are determined on the percentiles of connection strengths. The highest
#' 2 percentiles have width 10, the next 3 percentiles have width 8, etc.
#'
#' @param n_edges Integer to determine the number of edge widths that should be
#'   returned
#' @return A vector of edge widths which matches `n_edges` with its length. Widths
#'   vary between 1 and 0.025.
percentile_widths <- function(n_edges) {
  # The widths will be assigned based on the percentile a connection is in.
  # Specify percentiles that will get distinct widths
  percentile_groups = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36) / 100)

  # How often will each width be used
  width_times = round((percentile_groups * n_edges), 0)
  # Rounding can give the incorrect total number of edge widths
  n_edge_diff = sum(width_times) - n_edges
  if (!n_edge_diff == 0) width_times[8] <- width_times[8] - n_edge_diff

  # Preset edge widths for the different percentiles
  width_opts <- c(1, 0.8, 0.4, 0.2, 0.1, 0.05, 0.025, 0.025)

  # Edge width will be repeated so each edge can receive a width
  edge_widths = NULL
  for (i in 1:length(width_times)) {
    times = width_times[i]
    value = width_opts[i]
    edge_widths <- as.vector(append(edge_widths, c(rep(value, times))))
  }

  return(edge_widths)
}


weights_to_color <- function(edge_table) {
  # A column named 'weight' is required to determine edge colors
  if (!"weight" %in% colnames(edge_table)) {
    stop("Must provide edge table with weights:",
         "\nℹ Your edge table contains these columns: ",
         colnames(edge_table) %>% paste(collapse = ", "),
         ".\n✖ `colnames(edge_table)` must include 'weight'.",
         call.=FALSE)
  }

  # If negative numbers are found in the weights use a diverging color palette,
  #   otherwise use a sequential color palette
  if (min(edge_table$weight) < 0) {
    color <- as.vector(colorspace::diverging_hcl(n=nrow(edge_table), palette = "Blue-Red"))
    edge_table <- edge_table[sort(edge_table$weight, decreasing=T, index.return=T)[[2]],]
    edge_table <- cbind(edge_table, color)
  } else {
    color <- as.vector(colorspace::sequential_hcl(n=nrow(edge_table), palette = "Reds2"))
    edge_table <- edge_table[sort(abs(edge_table$weight), decreasing=T, index.return=T)[[2]],]
    edge_table <- cbind(edge_table, color)
  }

  return(edge_table)
}
