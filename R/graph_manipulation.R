#' Add group column and sort node table
#'
#' Adds 'group' column to `node_table` with values corresponding to  `group_vec`.
#' The rows of `node_table` will be reordered so the group column is sorted
#' alphabetically.
#'
#' @param node_table A data frame in which rows correspond to the nodes of a
#'   network.
#' @param group_vec A vector contains group labels for each row in `node_table`.
#' @return Returns `node_table` with 'group' column added, sorted by group.
#'
#' @noRd
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

  # If the column to be added is already present
  if ("group" %in% colnames(node_table)) {
      warning("Node attributes already include 'group', this attribute will ",
              "be overwritten.", call. = F)
  }

  # Add grouping vector as column and sort the rows
  node_table$group <- group_vec
  node_table <- node_table[order(node_table$group),]

  return(node_table)
}


#' Calculate x and y coordinates for nodes
#'
#' Calculates coordinates for each node in the node table. To avoid overlap the
#' size of nodes is takes into account when creating the layout.
#'
#' @inheritParams group_nodes
#' @param nodesize A number that represents the maximum node width. This will be
#'   used to calculate the minimum radius that a circular layout can have to
#'   avoid overlap of nodes.
#' @param layout A character string (default: 'circle') that determines which
#'   layout will be used to arrange the nodes.
#' @param space_fct A number (default: 1.2) that is used as multiplier for
#'   nodesize. It increases the spacing between nodes.
#'
#' @return Returns `node_table` with 'x' and 'y' column added or overwritten.
#'
#' @noRd
add_node_pos <- function(node_table, nodesize, layout = c("circle"), space_fct = 1.2) {

  layout <- match.arg(layout)
  n_nodes <- nrow(node_table)

  if (!is.numeric(space_fct)) {
    stop("Must provide numeric for node space scaling:",
    "\nℹ Class of your `space_fct`: ", class(space_fct), call.=FALSE)
  }

  # Adjust radius to node sizes (this value should be the max nodesize)
  R <- (n_nodes * (nodesize * space_fct)) / (2 * pi)

  if (layout == "circle") {
    # Calculate the postions in a circle for all nodes
    X <- sapply(0:(n_nodes - 1), function (i) R * cos((i * 2 * pi) / (n_nodes)))
    Y <- sapply(0:(n_nodes - 1), function (i) R * sin((i * 2 * pi) / (n_nodes)))
  }

  node_table$x <- X
  node_table$y <- Y

  return(node_table)
}


#' Add a distinct color to each group
#'
#' Adds a 'color' column with hex values to `node_table`. The colors will be
#' assigned based on the 'group' column of `node_table`. Nodes that are in the
#' same group obtain the same color.
#'
#' @param node_table A data frame in which rows correspond to the nodes of a
#'   network. Must contain a column names `group`.
#' @param group_colors Optional (default: `NULL`), vector of character strings
#'   representing colors, with a color for each node group. Colors may be
#'   provided either as color names, e.g. 'green', or as hex, e.g. "#00FF00".
#'
#' @return Returns `node_table` with 'color' column, that contains a different
#'   color for each node group.
#'
#' @noRd
add_colors <- function(node_table, group_colors = NULL) {
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
  colors <- n_distinct_cols(length(groups), group_colors)

  # index which group is assigned to which row
  idx <- match(node_table$group, groups)

  # If the column to be added is already present
  if ("color" %in% colnames(node_table)) {
      warning("Node attributes already include 'color', this attribute will ",
              "be overwritten.", call. = F)
  }

  # Use group index to select correct colors for each row
  node_table$color <- colors[idx]

  return(node_table)
}


#' Add connectivity based 'size' column to node table
#'
#' Calculates the connectivity of nodes as the sum of all weights of edges
#' originating or ending at a node.
#' For use as node size, the values are scaled to range 0 to 1, transformed
#' with a sigmoid, and then multiplied with a constant factor that is determined
#' by `size_type`.
#'
#' @param node_table A data frame with a column named 'node'.
#' @param edge_table A data frame with with columns source and target, containing
#'   the names of nodes connected by an edge, and a column called "weight" containing
#'   numeric values.
#' @param size_type One of "igraph" (default), "cytoscape", or "scaled_only".
#'   This argument determines a factor for linear scaling of node size. Factors
#'   are 15, 25, and 1, respectively.
#' @return Returns `node_table` with an added column 'size'.
#'
#' @noRd
node_size_connectivity <- function(node_table,
                                   edge_table,
                                   size_type = c("igraph", "cytoscape", "scaled_only")) {

  # Matching argument, allow abbreviation
  size_type <- match.arg(size_type)

  # Calculate connectivity from edges
  connectivity <- sapply(node_table$node,
            function (x) sum(abs(c(edge_table[edge_table$source == x,]$weight,
                                   edge_table[edge_table$target == x,]$weight)),
                             na.rm = T))

  # Scale to range 0 to 1, required for sigmoid
  scale_conn <- connectivity / max(connectivity)

  # Transform with sigmoid to emphasize the contrasts
  scale_conn <- sigmoid_xB(scale_conn, 3)

  # Match with default node size for the visualization
  if (size_type == "igraph") scale_conn <- (scale_conn * 12) + 3
  if (size_type == "cytoscape") scale_conn <- (scale_conn * 20) + 7
  # if (size_type == "scaled_only") scale_conn <- scale_conn

  # Make sure we can add the node size in correct order
  match_idx <- match(node_table$node, names(scale_conn))

  # If the column to be added is already present
  if ("size" %in% colnames(node_table)) {
      warning("Node attributes already include 'size', this attribute will ",
              "be overwritten.", call. = F)
  }

  # Add to node table
  node_table$size <- scale_conn[match_idx]

  return(node_table)
}


#' Reorder node tables by their average connectivity
#'
#' Calculates the average connectivity and reorders the rows of the input node
#' tables. If a 'group' column is present in the first node table, the groups
#' will only be ordered internally by average connectivity.
#'
#' @param nodes_list A list of node tables. Each must have a 'node' column holding
#'   node names. A least some of the tables should have a 'size' column, containing
#'   the connectivity information.
#' @return Returns the same `nodes_list` as was used as input, with rows reordered.
#'
#' @noRd
sort_avg_connectivity <- function(nodes_list) {
  # Allow sorting of a single node_table that is not a list
  if (!inherits(nodes_list, "list")) {
    if (inherits(nodes_list, "data.frame")) {
      if (all(c("node", "size") %in% colnames(nodes_list))) {
        nodes_list <- list(nodes_list)
      } else {
        stop("Must provide list of node tables with attributes 'node' and 'size'.",
             call.=FALSE)
      }
    } else {
      stop("Must provide a list of node tables to calculate average connectivity.",
           all.=FALSE)
    }
  }


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

  # there might be a different order of the nodes, use the first as reference
  #   to prevent averaging different nodes
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
#'   to be used with weights derived from mutual information (range 0 to +Inf).
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
#'
#' @noRd
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
            "type expect edge weight range 0 to +Inf, while your data has range: ",
            range(edge_table$weight) %>% paste(collapse = " to "), ". ",
            "Unexpected results may be returned for edge widths.")
  }

  # If the column to be added is already present
  if ("width" %in% colnames(edge_table)) {
      warning("Edge attributes already include 'width', this attribute will ",
              "be overwritten.", call. = F)
      edge_table <- edge_table %>% dplyr::select(-width)
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
#'
#' @noRd
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


#' Color edges based on weight
#'
#' This function adds a column of hex colors to and edge table. The colors are
#' determined by the weight of an edge. When both positive and negative weights
#' are found, a blue-red divergent color palette will be used to select colors.
#' For only positive weights, colors are selected from a white to red color
#' gradient. Optionally, custom color palettes can be used
#'
#' Colors are assigned by placing the edge weights into 100 bins, and assigning
#' 100 colors from the appropriate palette.
#'
#' @inheritParams edge_weight_to_widths
#' @param edge_color_func A function that takes as first argument a number, and
#'     returns a vector of colors, the length of which equals the input number.
#'     The first colors in this vector will be used for low values, the last
#'     colors will be assigned to high values. This argument is optional
#'     (default `NULL`).
#'
#' @return Returns the input `edge_table` with a column called 'color' added
#' (or overwritten if it was already present).
#'
#' @noRd
weights_to_color <- function(edge_table, edge_color_func = NULL) {
    # A column named 'weight' is required to determine edge colors
    if (!"weight" %in% colnames(edge_table)) {
        stop("Must provide edge table with weights:",
             "\nℹ Your edge table contains these columns: ",
             colnames(edge_table) %>% paste(collapse = ", "),
             ".\n✖ `colnames(edge_table)` must include 'weight'.",
             call.=FALSE)
    }

    # Validate user color function input
    if (!is.null(edge_color_func)) {
        if (!is.function(edge_color_func)) {
            stop("Must provide a function to change edge colors: ",
                 "\nℹ Class of your `edge_color_func`: ", class(edge_color_func),
                 "\n✖ `edge_color_func` must be a function.", call.=FALSE)
        }

        message_base <- "While getting 100 colors from your `edge_color_func`"
        color_pal <- tryCatch(edge_color_func(100),
                              error = function (e) {
                                  message(paste(message_base, "an error occurred."))
                                  message("Make sure that `edge_color_func(100)` ",
                                          "works without errors: ")
                                  stop(e)
                              },
                              warning = function (w) {
                                  message(paste(message_base, "the following",
                                                "warning was raised: "))
                                  warning(w)

                                  return(edge_color_func(100) %>%
                                             suppressWarnings())
                              })

        if (length(color_pal) != 100) {
            stop("`edge_color_func(100)` must return 100 colors:",
                 "\nℹ Length of return: ", length(color_pal),
                 call.=FALSE)
        }

        tryCatch(are_colors(color_pal),
                 error = function (e) {
                     message("Make sure your `edge_color_func` returns valid ",
                             "colors, the following error occurred when ",
                             "validating colors: ")
                     stop(e)
                 })
    }

    # If the column to be added is already present
    if ("color" %in% colnames(edge_table)) {
        warning("Edge attributes already include 'color', this attribute will ",
                "be overwritten.", call. = F)
    }

    # If negative numbers are found in the weights use a diverging color palette,
    #   otherwise use a sequential color palette
    if (min(edge_table$weight) < 0) {

        # If the user didn't provide colors
        if (is.null(edge_color_func)) {
            color_pal <- colorspace::diverging_hcl(n = 120, palette = "Blue-Red")
            # The center colors are barely visible in the graphs, remove them
            color_pal <- c(color_pal[1:50], color_pal[71:120])
        }

        # Make seperate groups for positive and negative values
        neg_idx <- which(edge_table$weight <= 0)
        pos_idx <- which(edge_table$weight > 0)

        neg_cols <- color_pal[
            as.numeric(cut(edge_table$weight[neg_idx], 50))]
        pos_cols <- color_pal[
            as.numeric(cut(edge_table$weight[pos_idx], 50)) + 50]

        edge_table$color <- ""
        edge_table$color[neg_idx] <- neg_cols
        edge_table$color[pos_idx] <- pos_cols

    } else {

        # Or user colors if they are provided
        if (is.null(edge_color_func)) {
            color_pal <- rev(colorspace::sequential_hcl(n = 100,
                                                        palette = "Reds2"))
        }

        edge_table <- dplyr::mutate(edge_table,
                                    color = color_pal[
                                        as.numeric(cut(weight, 100))])
    }

    return(edge_table)
}
