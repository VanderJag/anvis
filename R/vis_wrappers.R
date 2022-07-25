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
#' @param arrange_co Logical (default FALSE), should nodes be reordered based on
#'   their average connectivity in multiple networks? Requires
#'   the same names nodes to be present in all networks. Also requires 'size'
#'   column to be present in node tables, so `node_attrs` should be 'all' or
#'   include 'size'.
#' @param output_type Choose "igraph" (default), "cytoscape", or "xgmml" to get
#'   a visualization with the respective software for the first two options, or
#'   the appropriate download for the third option. Node widths columns will be
#'   adjusted to match your chosen output.
#' @param do_save Logical (default TRUE), should network visualizations be saved?
#'   If this parameter is FALSE igraph will show a plot in your R session and
#'   cytoscape will keep the session with all networks open.
#' @inheritParams adj_matrix_to_network
#' @return The section on the returned values
#'
#' @section Additional criteria for the use of this function:
#' * The Cytoscape software needs to be running.
#' * ...
VisualiseNetwork <- function(adj_mats,
                             node_attrs = c("none", "all", "group", "color_group", "size"),
                             edge_attrs = c("none", "all", "width", "color"),
                             group_vec = NULL,
                             width_type = NULL,
                             arrange_co = FALSE,
                             output_type = c("igraph", "cytoscape", "xgmml"),
                             radial_labs = T,
                             do_save = T,
                             save_names = "network",
                             export_type = c("png", "jpeg", "pdf", "svg", "ps"),
                             export_opts = list(),
                             cyto3.8_check = T,
                             igr_rad_lab_opts = list(),
                             igr_plot_opts = list(),
                             cyto_save_session = !do_save,
                             cyto_close_session = do_save
                             ) {
  export_type <- match.arg(export_type)

  # Check which visualization should be used, allow abbreviations
  output_type <- match.arg(output_type)

  # Get size type from output type
  if (output_type %in% c("igraph", "cytoscape")) {
    size_type <- output_type
  } else {
    size_type <- "scaled_only"
  }

  # Since this function uses a for loop to iterate over the visualizations that
  #   are created, the input needs to be converted into a list.
  if(inherits(adj_mats, "data.frame") == TRUE |
     inherits(adj_mats, "matrix") == TRUE) {
    adj_mats <- list(adj_mats)
  } else if (!inherits(adj_mats, "list")) {
    stop("Must provide data.frame, matrix, or list of these as input \n",
         "You provided: ", class(adj_mats), call. = FALSE)
  }

  # Check number of matrices for later tests
  n_mats <- length(adj_mats)


  # Check if grouping vector is list, if not, turn into list
  if (!is.null(group_vec)) {
    if (!is.list(group_vec)) {
      group_vec <- list(group_vec)
    } else {
      if (!length(group_vec) == n_mats && !length(group_vec) == 1) {
        stop("Grouping vector list must be of equal length as adj_mats, or length 1",
             "\nℹ Length of `group_vec` = ", length(group_vec),
             ", length of `adj_mats` = ", n_mats, ".", call.=FALSE)

      }
    }
  }

  # Convert all adjacency matrices into edge and node tables
  networks <- lapply(seq_along(adj_mats),
     function(x) {
       adj_matrix_to_network(adj_mats[[x]],
                             node_attrs = node_attrs,
                             edge_attrs = edge_attrs,
                             group_vec = group_vec[[
                               if (length(group_vec) == n_mats) x else 1]],
                             width_type = width_type,
                             size_type = size_type)})
  nodes <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$node_table)
  edges <- lapply(seq_along(adj_mats),
                  function(x) networks[[x]]$edge_table)

  # Node ordering by average connectivity
  if (arrange_co) {
    nodes <- sort_avg_connectivity(nodes_list = nodes)
  }

  # Check save names
  names_match <- length(save_names) == n_mats
  if (do_save) {
    if (!(names_match || length(save_names) == 1)) {
      stop("Length of save names must be 1 or matching with number of matrices: ",
           "\nℹ Length of `save_names` = ", length(save_names),
           ", length of `adj_mats` = ", n_mats, ".", call.=FALSE)
    }
  }

  # Choose visualization
  if (output_type == "igraph") {
    # If plots are not saved show them in the R session
    if (!do_save) export_type <- "print"
    # Create igraph plots
    for (i in seq_along(edges)) {
      vis_igraph(edge_table = edges[[i]],
                 node_table = nodes[[i]],
                 save_name = save_names[[if (names_match) i else 1]],
                 export_type = export_type,
                 export_opts = export_opts,
                 radial_labs = radial_labs,
                 rad_lab_opts = igr_rad_lab_opts,
                 ... = igr_plot_opts)
    }
  } else if (output_type == "cytoscape") {
    # If plots are not saved keep igraph session open
    for (i in seq_along(edges)) {
      vis_in_cytoscape(edge_table = edges[[i]],
                       node_table = nodes[[i]],
                       save_name = save_names[[if (names_match) i else 1]],
                       export_type = export_type %>% stringr::str_to_upper(),
                       close_session = cyto_close_session,
                       save_session = cyto_save_session,
                       export_image = do_save,
                       export_opts = export_opts,
                       cyto3.8_check = cyto3.8_check,
                       radial_labs = radial_labs)
    }
  }

  Network = list(adjacencies = adj_mats, nodes = nodes, edges = edges)
  return(Network)
}
