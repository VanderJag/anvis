#' Create network and visualize or export
#'
#' This function takes one or more adjacency matrices as input. It uses the
#' adjacency information (weights) to create several attributes for network
#' visualization such as scaled edge widths and node sizes based on node
#' connectivity. The created network with additional attributes can be visualized
#' with igraph or cytoscape, or exported in several common file formats.
#'
#' Note: When using this function to create visualizations with cytoscape, the
#' cytoscape software needs to be running in the background already.
#'
#' @param adj_mats A square adjacency matrix or data frame or a list of these.
#'     The data in the matrix is used as edge weights for the network. Row names
#'     and column names specify interacting nodes, and are required.
#' @param directed Logical (default `FALSE`), whether edges are directed in the
#'     network. If `FALSE`, the information in the lower triangle of the
#'     adjacency matrix will be discarded. If `TRUE` this information will be
#'     retained and the style of visualizations will be adjusted to feature
#'     edge curvature and arrows.
#' @inheritParams adjToNetwork
#' @param group_vec A vector of character strings that assigns group labels to
#'     the nodes. The order of this vector should match the order of column and
#'     rownames of the input adjacency matrices. If `adj_mats` is a list, a single
#'     group vector can be used if it matches all adjacency matrices.
#'     Alternatively, provide a list of group vectors with one vector for each
#'     adj. matrix in the list. For this information to be added, `node_attrs`
#'     must be 'group' or 'all'.
#' @inheritParams weights_to_color
#' @param colorblind Logical (default `FALSE`), determining if the default colors
#'     should be exchanged for colorblind accessible colors.
#' @param edge_factor Numeric, a number that will be multiplied with the edge
#'     widths, scaling the edge widths linearly. When this argument is `NULL`,
#'     default values will be used (3.25 for igraph and 2 for cytoscape).
#' @param width_type Argument used to convert edge weights into widths for
#'     visualizations that are more easy to interpret. Find a description of the
#'     options in [edge_weight_to_widths]. This argument can be a single type,
#'     that will be used for all networks, or a vector of types that matches the
#'     number of networks.
#' @param arrange_co Logical (default `FALSE`), should nodes be reordered based on
#'     their average connectivity in multiple networks? Requires
#'     the same names nodes to be present in all networks. Also requires 'size'
#'     column to be present in node tables, so `node_attrs` should be 'all' or
#'     include 'size'.
#' @param save_names Base name for the files that are saved by this function
#'     (default: 'network'). If a single name is provided for multiple networks,
#'     numbers will be appended to the base name while saving. Alternatively,
#'     this argument can be a vector with a name for each network that will be
#'     saved. If in any case one of the same names already exist in the save
#'     target directory, numbers will added to the base file name, and incremented
#'     until a no file with the same base name and number exists.
#' @param output_type Choose 'igraph' (default), 'cytoscape', 'network', or
#'     'return_only', can be abbreviated. This argument determines the main
#'     output of this function. The first two options will make a
#'     visualization with the respective software. The third option will save
#'     the networks in a format that is determined by the `netw_ext` argument.
#'     The 'return_only' returns the networks created by this function, just
#'     like all other options, but without any side effects.
#' @param vis_radial_labs A logical (default `TRUE`) to indicate whether node
#'     labels should be positioned radially around the circular arrangement of
#'     vertices. If `FALSE`, they will be placed on top or next to the vertices,
#'     depending on your visualization method. This argument is only used when
#'     the output type is 'cytoscape' or 'igraph'.
#' @param vis_save Logical (default `FALSE`), should network visualizations be saved?
#'     If this parameter is `FALSE` igraph will show a plot in your R session and
#'     cytoscape will keep the session with all networks open.
#' @param vis_export_type Character string, one of 'png' (default), 'jpeg', 'pdf',
#'     'svg' and 'ps'. Use this argument to choose the file type with which
#'     visualizations are saved.
#' @param vis_export_opts A list with named elements. The list items will be used
#'     as arguments for the function that exports your visualization. When
#'     exporting a visualization made with igraph, the available options for
#'     exporting are those that work for the graphical device you selected with
#'     `vis_export_type`. In this case, you can check which options are available
#'     for your graphical device with e.g. [png]. When the visualization was
#'     made with cytoscape, valid export options are those accepted by
#'     [RCy3::exportImage].
#' @param igr_rad_lab_opts A named list, in which the names are valid arguments
#'     for [text]. These styling options apply to igraph node labels when
#'     `vis_radial_labs` is `TRUE`.
#' @param igr_plot_opts A named list containing additional options to be used with
#'     [igraph::plot.igraph] for customizing the igraph visualizations. If
#'     `vis_radial_labs` is `FALSE`, this argument can also be used to
#'     customize igraph visualization node labels.
#' @param igr_par_opts A list with named elements. The list items will be used
#'     as arguments for [par] while making visualizations with igraph. Changes to
#'     the graphical parameters will be applied to your selected graphical device
#'     before making the visualization. After the visualization is completed,
#'     the graphical parameters will be reset to their original value.
#' @param igr_grid Logical (default: `FALSE`), or a vector with two numbers
#'     representing grid dimensions. This argument will determine whether igraph
#'     visualization are be made separately, or arranged on a grid in a single
#'     image. `TRUE` will automatically determine grid size.
#' @param igr_grid_names Logical (default: `FALSE`) determining whether titles
#'     should be placed above the individual networks in the grid layout. `TRUE`
#'     will use the names of the `adj_mats` list to place titles. Alternatively,
#'     a vector with a name for each network can be provided as input for this
#'     argument.
#' @param cyto3.8_check Logical (default `TRUE`). Should execution stop if
#'     Cytoscape version 3.8.x is detected? `FALSE` to skip this test.
#'     Cytoscape version 3.8.x has problems interacting with `RCy3`, first
#'     visualization may not show.
#' @param cyto_save_session Logical (default `FALSE`). When visualizing with
#'     cytoscape, should the session be saved (as .cys file)?
#' @param cyto_close_session Logical (default same as `vis_save`), should the
#'     cytoscape session be closed after visualization has been completed?
#' @param cyto_node_space Numeric (default 1.2). By default nodes are arranged
#'     with a circular layout, where the space between the nodes is adjusted with
#'     this argument. When this argument is 1, the borders of the largest nodes
#'     can be touching. When e.g. a value of 2 is chosen for this argument, the
#'     space around a node will be twice the (maximum) node size. This argument
#'     only increases node spacing for cytoscape visualizations.
#' @param netw_ext Character string (default 'XGMML'). When `output_type` is
#'     'network', which file format should be used to save the network? Options
#'     are 'XGMML', 'table', 'sif', 'tab', 'tgf', 'net'.
#' @param netw_xgmml_title Only used when `output_type` is 'network' and `netw_ext`
#'     is 'XGMML'. This argument determines the title attribute of a network
#'     when it's saved as XGMML file. Possible values are `NULL` (default), to use
#'     the `save_names` as a network title, a single string, to give all networks
#'     the same title, or a vector of strings matching the length of input
#'     networks, to provide each networks its own title.
#' @inheritParams adjToNetwork
#'
#' @return The main purpose of this function is to create visualizations or
#' export networks. In addition, this function returns a list of two lists,
#' containing the information of the created networks. The first is named 'nodes'
#' and it is a list containing a node table (data frame) for each input adjacency
#' matrix. The second list is named 'edges' and it contains edge tables. The
#' return is invisible, so it will not print when not assigned.
#'
#' @export
VisualiseNetwork <- function(adj_mats,
                             directed = FALSE,
                             self_loops = FALSE,
                             node_attrs = c("none", "all", "group", "color_group", "size"),
                             edge_attrs = c("none", "all", "width", "color"),
                             group_vec = NULL,
                             group_colors = NULL,
                             edge_color_func = NULL,
                             colorblind = FALSE,
                             width_type = NULL,
                             edge_factor = NULL,
                             arrange_co = FALSE,
                             save_names = "network",
                             output_type = c("igraph", "cytoscape", "network", "return_only"),
                             vis_radial_labs = T,
                             vis_save = FALSE,
                             vis_export_type = c("png", "jpeg", "pdf", "svg", "ps"),
                             vis_export_opts = list(),
                             igr_rad_lab_opts = list(),
                             igr_plot_opts = list(),
                             igr_par_opts = list(),
                             igr_grid = FALSE,
                             igr_grid_names = FALSE,
                             cyto3.8_check = TRUE,
                             cyto_save_session = FALSE,
                             cyto_close_session = vis_save,
                             cyto_node_space = 1.2,
                             netw_ext = c("XGMML", "table", "sif", "tab", "tgf", "net"),
                             netw_xgmml_title = NULL
                             ) {

    # Check if arguments are list with named elements
    named_list_check(vis_export_opts)
    named_list_check(igr_rad_lab_opts)
    named_list_check(igr_plot_opts)
    named_list_check(igr_par_opts)

  # visualization output type
  vis_export_type <- match.arg(vis_export_type)

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

  # See if colorblind accessible colors should be used
  if (colorblind) {
      # Manually chosed colors overrule colorblind option
      if (!is.null(group_colors)) {
          warning("Colors provided with `group_colors` will be used instead of ",
                  "colorblind accessible colors. To prevent this use ",
                  "`group_colors = NULL`.")
      }

      group_colors <- group_colors %||% palette.colors(palette = "Okabe-Ito")
  }

  # Convert all adjacency matrices into edge and node tables
  networks <- lapply(seq_along(adj_mats),
     function(x) {
         adjToNetwork(adj_mats[[x]],
                             directed = directed,
                             self_loops = self_loops,
                             node_attrs = node_attrs,
                             edge_attrs = edge_attrs,
                             group_vec = group_vec[[
                               if (length(group_vec) == n_mats) x else 1]],
                             width_type = width_type[[
                               if (length(width_type) == n_mats) x else 1]],
                             size_type = size_type,
                             group_colors = group_colors,
                             edge_color_func = edge_color_func)})
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
  if (!(names_match || length(save_names) == 1)) {
    stop("Length of save names must be 1 or matching with number of matrices: ",
         "\nℹ Length of `save_names` = ", length(save_names),
         ", length of `adj_mats` = ", n_mats, ".", call.=FALSE)
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
        if (!is.logical(igr_grid_names)) {
            if (length(igr_grid_names) != n_mats) {
                stop("Grid names must be TRUE, FALSE, or of length matching with number of matrices: ",
                     "\nℹ Length of `igr_grid_names` = ", length(igr_grid_names),
                     ", length of `adj_mats` = ", n_mats, ".", call.=FALSE)
            }
        } else if (isTRUE(igr_grid_names)) {
            if (length(names(adj_mats)) != n_mats) {
                warning("`igr_grid_names` is TRUE but no names were found in ",
                        "adjacency matrix list.")
            }
        }

        if (vis_save) {
            start_saving(vis_export_type, vis_export_opts, save_names[[1]])
            on.exit(if (dev.cur() > 1) dev.off())
        }

        # Allow that plots can be arranged in grid
        par(mfrow= user_dims %||% n2mfrow(n_mats))
    }

    # If plots are not saved show them in the R session, to arrange on grid
    #   also print first
    if (!(vis_save) || (vis_save && igr_grid)) vis_export_type <- "print"

    # Create igraph plots
    for (i in 1:n_mats) {
      # Set plot title for grip plots when it is requested
      if (igr_grid && isTRUE(igr_grid_names)) {
          igr_plot_opts[["main"]] <- names(adj_mats)[i]
      } else if (igr_grid && !isFALSE(igr_grid_names)) {
          igr_plot_opts[["main"]] <- igr_grid_names[i]
      }

      do.call(vis_igraph,
              c(list(edge_table = edges[[i]],
                 node_table = nodes[[i]],
                 directed = directed,
                 save_name = save_names[[if (names_match) i else 1]],
                 export_type = vis_export_type,
                 export_opts = vis_export_opts,
                 radial_labs = vis_radial_labs,
                 scale_width = edge_factor,
                 rad_lab_opts = igr_rad_lab_opts,
                 par_opts = igr_par_opts),
                 igr_plot_opts)
      )
    }

    if (igr_grid) {
      if (vis_save) dev.off()
      par(mfrow=c(1,1)) # Reset if this has been changed
    }

  } else if (output_type == "cytoscape") {
    # If plots are not saved keep igraph session open
    for (i in seq_along(edges)) {
      vis_in_cytoscape(edge_table = edges[[i]],
                       node_table = nodes[[i]],
                       directed = directed,
                       save_name = save_names[[if (names_match) i else 1]],
                       export_type = vis_export_type %>% stringr::str_to_upper(),
                       close_session = cyto_close_session,
                       save_session = cyto_save_session,
                       export_image = vis_save,
                       export_opts = vis_export_opts,
                       scale_width = edge_factor,
                       cyto3.8_check = cyto3.8_check,
                       radial_labs = vis_radial_labs,
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

  Network = list(nodes = nodes, edges = edges)

  return(invisible(Network))
}
