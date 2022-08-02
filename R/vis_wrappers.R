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
                             output_type = c("igraph", "cytoscape", "network", "return_only"),
                             radial_labs = T,
                             do_save = FALSE,
                             save_names = "network",
                             export_type = c("png", "jpeg", "pdf", "svg", "ps"),
                             export_opts = list(),
                             edge_factor = NULL,
                             group_colors = NULL,
                             igr_rad_lab_opts = list(),
                             igr_plot_opts = list(),
                             igr_grid = FALSE,
                             igr_par_opts = list(),
                             cyto3.8_check = T,
                             cyto_save_session = FALSE,
                             cyto_close_session = do_save,
                             cyto_node_space = 1.2,
                             netw_ext = c("XGMML", "table", "sif", "tab", "tgf", "net"),
                             netw_xgmml_title = NULL
                             ) {
  # TODO add to documenation:
  # what are the defaults for edge factor
  # width type can be length 1 or same as n matrices
  # igr_grid, can be T, F, or a vector of two integers
  # netw_xgmml_title allows vector, netw_ext not

  # visualization output type
  export_type <- match.arg(export_type)

  # Check which visualization should be used, allow abbreviations
  output_type <- match.arg(output_type)

  # network saving file type
  netw_ext <- match.arg(netw_ext)

  # Set default for edge width scaling factor
  if (output_type == "igraph") edge_factor <- edge_factor %||% 3.25
  if (output_type == "cytoscape") edge_factor <- edge_factor %||% 2

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

  if (!is.null(width_type)){
    if (!(length(width_type) == n_mats || length(width_type) == 1)) {
      stop("Length of width type must be 1 or matching with number of matrices: ",
           "\nℹ Length of `width_type` = ", length(width_type),
           ", length of `adj_mats` = ", n_mats, ".", call.=FALSE)
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
                             width_type = width_type[[
                               if (length(width_type) == n_mats) x else 1]],
                             size_type = size_type,
                             group_colors = group_colors)})
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

    if (!(isTRUE(igr_grid) || isFALSE(igr_grid))) {
      if (!(length(igr_grid) == 2 && is.numeric(igr_grid))) {
        stop("`igr_grid` must be TRUE, FALSE, or a vector of two integers. ",
             call. = FALSE)
      } else {
        user_dims <- as.integer(igr_grid)
        igr_grid <- TRUE
        if ((user_dims[1] * user_dims[2]) < n_mats) {
          stop("Product of plotting grid dimensions must exceed or equal number of networks:",
          "\nℹ Number of networks: ", n_mats,
          "\n✖ Provided grid dimensions: ", user_dims %>% paste(collapse = " "), call.=FALSE)
        }
      }
    } else {
      user_dims <- NULL
    }


    # Start graphics device for multiple plots in grid situation
    if (igr_grid) {
      if (do_save) start_saving(export_type, export_opts, save_names[[1]])
      # Allow that plots can be arranged in grid
      par(mfrow= user_dims %||% n2mfrow(n_mats))
    }

    # If plots are not saved show them in the R session, to arrange on grid
    #   also print first
    if (!(do_save) || (do_save && igr_grid)) export_type <- "print"

    # Create igraph plots
    for (i in 1:n_mats) {
      # When placing in grid only save when maknig the last plot
      vis_igraph(edge_table = edges[[i]],
                 node_table = nodes[[i]],
                 save_name = save_names[[if (names_match) i else 1]],
                 export_type = export_type,
                 export_opts = export_opts,
                 radial_labs = radial_labs,
                 scale_width = edge_factor,
                 rad_lab_opts = igr_rad_lab_opts,
                 par_opts = igr_par_opts,
                 ... = igr_plot_opts)
    }

    if (igr_grid) {
      if (do_save) dev.off()
      par(mfrow=c(1,1)) # Reset if this has been changed
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
                       scale_width = edge_factor,
                       cyto3.8_check = cyto3.8_check,
                       radial_labs = radial_labs,
                       node_space = cyto_node_space)
    }

  } else if (output_type == "network") {

    # Check network, e.g. xgmml, saving options
    if (!is.null(netw_xgmml_title)){
      if (!(length(netw_xgmml_title) == n_mats || length(netw_xgmml_title) == 1)) {
        stop("Length of xgmml titles must be 1 or matching with number of matrices: ",
             "\nℹ Length of `netw_xgmml_title` = ", length(netw_xgmml_title),
             ", length of `adj_mats` = ", n_mats, ".", call.=FALSE)
      }
    }

    for (i in 1:n_mats) {
      # Avoid overwriting by appending number
      save_name <- file_sequence(name_base = save_names[[if (names_match) i else 1]],
                                 ext = paste0(".", netw_ext))

      if (!is.null(netw_xgmml_title)) {
        xgmml_title <- netw_xgmml_title[[if (length(netw_xgmml_title) == n_mats) i else 1]]
      } else {
        xgmml_title <- save_name
      }

      netw <- igraph::graph_from_data_frame(edges[[i]], vertices = nodes[[i]],
                                            directed = FALSE)
      BioNet::saveNetwork(network = netw,
                          name = xgmml_title,
                          file = save_name,
                          type = netw_ext)
    }
  }

  Network = list(adjacencies = adj_mats, nodes = nodes, edges = edges)

  return(invisible(Network))
}
