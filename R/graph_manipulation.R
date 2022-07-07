

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

  if ("width" %in% colnames(edge_table)) {
    warning("A column with the name 'width' is already found in `edge_table`,",
            "this column will be overwritten.")
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
