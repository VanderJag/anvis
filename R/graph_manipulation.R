

# adds a column with the grouping info to a node table, and sorts it
group_nodes <- function(node_table, group_vec = NULL) {

  # TODO Check if grouping vector has the correct length and ensure it's character type

  # If a grouping vector has been provided
  if (!is.null(group_vec)) {
    node_table$group <- group_vec
    node_table <- node_table[order(node_table$group),]
  } else {
    node_table$group <- "A"
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


add_colors <- function(node_table) {
  # Group information is required to add group colouring
  if (!"group" %in% names(node_table)) {
    warning("No group column found while adding node group columns")
    node_table$group <- "A"
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


# TODO create function that automatically infers the type of scaling for width,
#   by checking the range of the weights
pick_width_type <- function() {
  NULL
}

# For visualising the Edge Weights,
#   Choose "type = 1" for grading the edges based on partial correlation values
#   Choose "type = 2" for grading the edges based on Pearson or Spearman correlation values
#   Choose "type = 3" for grading the edges on a ranked percentile system (such that the edges are ranked and on an exponential scale
#   the gradient of the width and colour of edges are assigned. for eg. 98th percentile with the highest width, 95th percentile with the next width etc.)
#
#  1 is partcor, 2 is cor, 3 is MI, 4 is ranked, 5 is percentile.
#
# requires df with a column called weight
edge_weight_to_widths <- function(edge_table, width_type) {

  # TODO argument matching and selecting by numbers

  # number of edges is used in some calculations of edge width
  n_edges = nrow(edge_table)

  # Change the weights to widths according to type
  if (width_type == 1) {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = nthroot(abs(weight), 3), B = 3))
  } else if (width_type == 2) {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = abs(weight), B = 3))
  } else if (width_type == 3) {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = (abs(weight) / max(abs(weight))), B = 3))
  } else if (width_type == 4) {
    edge_table <- edge_table %>%
      dplyr::mutate(width = sigmoid_xB(x = (rank(-weight) / n_edges), B = 3))
  } else if (width_type == 5) {
    # Get percentile widths (descending order)
    width_col <- data.frame("width" = percentile_widths(n_edges = n_edges))
    # Sort edges descending
    edge_table <- edge_table[sort(abs(edge_table$weight),
                                  decreasing = T,
                                  index.return = T)[[2]],]
    # Combine edges and widths
    edge_table <- cbind(edge_table, width_col)
  } else {
    print("type not selected")
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
#'   vary between 10 and 0.25
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
  width_opts <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))

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
