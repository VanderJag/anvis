#' Visualize or export networks
#'
#' This function takes a (list of) network object(s) and visualizes them or
#' exports them in various formats. Based on additional attributes of
#' nodes and edges, visual elements such as edge widths and node sizes are
#' adjusted in the visualizations. Networks can be visualized with Cytoscape
#' or igraph. For the latter, there are options available to visualize multiple
#' networks in a grid, for easier comparison.
#'
#' Note: When using this function to create visualizations with Cytoscape, the
#' cytoscape software needs to be running in the background already.
#'
#' @param networks A network or a list of these. Valid classes for the network are
#'     "igraph", "graphNEL", and "list" of two data frames with the names "edges"
#'     and "vertices". If the edges in the network have the additional attributes
#'     'width' or 'color' those attributes will be used to style the edges in
#'     the visualization. Optional node attribute 'group' will be used to arrange
#'     vertices so those with the same group label will be placed next to each
#'     other, 'color' and 'size' will be used to adjust the corresponding
#'     features of the vertices.
#' @param directed Logical (default `NULL`), whether visualizations should
#'     feature edge curvature and arrows. If this argument is `NULL` this
#'     property will be automatically determined for 'igraph' or 'graphNEL'
#'     object, and otherwise by it will be `FALSE` by default.
#' @param vis_edge_factor Numeric, a number that will be multiplied with the edge
#'     widths, scaling the edge widths linearly. When this argument is `NULL`,
#'     default values will be used (3.25 for igraph and 2 for cytoscape).
#' @param save_names Base name for the files that are saved by this function
#'     (default: 'network'). If a single name is provided for multiple networks,
#'     numbers will be appended to the base name while saving. Alternatively,
#'     this argument can be a vector with a name for each network that will be
#'     saved. If in any case one of the same names already exist in the save
#'     target directory, numbers will added to the base file name, and incremented
#'     until a no file with the same base name and number exists.
#' @param output_type Choose 'igraph' (default), 'cytoscape', or 'network',
#'     can be abbreviated. This argument determines the main
#'     output of this function. The first two options will make a
#'     visualization with the respective software. The third option will save
#'     the networks in a format that is determined by the `netw_ext` argument.
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
#' @param igr_grid Logical (default: `TRUE`), or a vector with two numbers
#'     representing grid dimensions. This argument will determine whether igraph
#'     visualization are made separately, or arranged on a grid in a single
#'     image. `TRUE` will automatically determine grid size. Note: the defaults
#'     (`output_type = "igraph"`, and `vis_save = TRUE`) will output to the
#'     Rstudio graphical device, which will fail to show plots when the plot's
#'     margins are too large for the plotting window. This issue is more likely
#'     to occur for larger grids.
#' @param igr_grid_names Logical (default: `TRUE`) determining whether titles
#'     should be placed above the individual networks in the grid layout. `TRUE`
#'     will use the names of the `networks` list to place titles. Alternatively,
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
#'     are 'XGMML', 'table', 'sif', 'tab', 'tgf', 'net'. Learn more about these
#'     formats in [BioNet::saveNetwork].
#' @param netw_xgmml_title Only used when `output_type` is 'network' and `netw_ext`
#'     is 'XGMML'. This argument determines the title attribute of a network
#'     when it's saved as XGMML file. Possible values are `NULL` (default), to use
#'     the `save_names` as a network title, a single string, to give all networks
#'     the same title, or a vector of strings matching the length of input
#'     networks, to provide each networks its own title.
#'
#' @seealso Generate more interesting visualizations by using [addVisAttrs]
#'     for adding attributes to an existing network objects, and
#'     [adjToNetwork] for creating a network from an adjacency matrix and
#'     adding node and edge attributes in a single step.
#'
#' @export
anvis <- function(networks,
                  directed = NULL,
                  save_names = "network",
                  output_type = c("igraph", "cytoscape", "network"),
                  vis_edge_factor = NULL,
                  vis_radial_labs = T,
                  vis_save = FALSE,
                  vis_export_type = c("png", "jpeg", "pdf", "svg", "ps"),
                  vis_export_opts = list(),
                  igr_rad_lab_opts = list(),
                  igr_plot_opts = list(),
                  igr_par_opts = list(),
                  igr_grid = TRUE,
                  igr_grid_names = TRUE,
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
    if (output_type == "igraph") vis_edge_factor <- vis_edge_factor %||% 3.25
    if (output_type == "cytoscape") vis_edge_factor <- vis_edge_factor %||% 2

    # For the other steps we need a list in which each element is a single network
    if (!inherits(networks, "list") || is_network_list(networks)) {
        networks <- list(networks)
    }

    for (i in seq_along(networks)) {
        net <- networks[[i]]
        if (!(is(net, "graphNEL") || is(net, "igraph") || is_network_list(net))) {
            stop("Input networks must be graphNEL, igraph, or list containing data ",
                 "frames named 'vertices' and 'edges'. \nℹ Class of your network: ",
                 class(net), "\n Error occured for network nr.: ", i, call.=FALSE)
        }
    }

    # Check number of matrices for later tests
    n_nets <- length(networks)

    # Check save names
    names_match <- length(save_names) == n_nets
    if (!(names_match || length(save_names) == 1)) {
        stop("Length of save names must be 1 or matching with number of networks: ",
             "\nℹ Length of `save_names` = ", length(save_names),
             ", length of `networks` = ", n_nets, ".", call.=FALSE)
    }

    # Get edge mode (directed or not?) from network if it wasn't explicitly provided
    if (is.null(directed)) {
        directed <- sapply(networks, function(net) {
            if (is(net, "graphNEL")) {
                graph::edgemode(net) == "directed"
            } else if (is(net, "igraph")) {
                igraph::is_directed(net)
            } else {
                FALSE
            }
        })
    }

    # Check if length of network mode matches
    directed_match <- length(directed) == n_nets
    if (!(directed_match || length(directed) == 1)) {
        stop("Length of `directed` must be 1 or matching with number of networks: ",
             "\nℹ Length of `directed` = ", length(directed),
             ", length of `network` = ", n_nets, ".", call.=FALSE)
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
                if ((user_dims[1] * user_dims[2]) < n_nets) {
                    stop("Product of plotting grid dimensions must exceed or equal number of networks:",
                         "\nℹ Number of networks: ", n_nets,
                         "\n✖ Provided grid dimensions: ", user_dims %>% paste(collapse = " "), call.=FALSE)
                }
            }
        } else {
            user_dims <- NULL
        }


        # Start graphics device for multiple plots in grid situation
        if (igr_grid) {
            if (!is.logical(igr_grid_names)) {
                if (length(igr_grid_names) != n_nets) {
                    stop("Grid names must be TRUE, FALSE, or of length matching with number of matrices: ",
                         "\nℹ Length of `igr_grid_names` = ", length(igr_grid_names),
                         ", length of `networks` = ", n_nets, ".", call.=FALSE)
                }
            }
            # Below code make more sense with default igr_grid_names = FALSE
            #  else if (isTRUE(igr_grid_names)) {
            #     if (length(names(networks)) != n_nets) {
            #         warning("`igr_grid_names` is TRUE but no names were found in ",
            #                 "network list.")
            #     }
            # }

            if (vis_save) {
                start_saving(vis_export_type, vis_export_opts, save_names[[1]])
                # Close the device if an error occurs
                dev_name <- if (vis_export_type == "ps") "postscript" else vis_export_type
                on.exit(if (names(dev.cur()) == dev_name) dev.off())
            }

            # Allow that plots can be arranged in grid
            par(mfrow= user_dims %||% rev(n2mfrow(n_nets)))
        }

        # If plots are not saved show them in the R session, to arrange on grid
        #   also print first
        if (!(vis_save) || (vis_save && igr_grid)) vis_export_type <- "print"

        # Unclear errors may occur when plot margins are too large
        tryCatch({
            # Create igraph plots
            for (i in seq_along(networks)) {
                # Set plot title for grid plots when it is requested
                if (igr_grid && isTRUE(igr_grid_names)) {
                    igr_plot_opts[["main"]] <- names(networks)[i]
                } else if (igr_grid && !isFALSE(igr_grid_names)) {
                    igr_plot_opts[["main"]] <- igr_grid_names[i]
                }

                # Run visualization of networks with visIgraph
                do.call(visIgraph,
                        c(list(networks[[i]],
                               directed = directed[[if (directed_match) i else 1]],
                               save_name = save_names[[if (names_match) i else 1]],
                               export_type = vis_export_type,
                               export_opts = vis_export_opts,
                               radial_labs = vis_radial_labs,
                               scale_width = vis_edge_factor,
                               rad_lab_opts = igr_rad_lab_opts,
                               par_opts = igr_par_opts),
                          igr_plot_opts)
                )
            }

            # Extend potentially uninformative error message
        }, error=function(cond) {
            if (stringr::str_detect(cond$message, "figure margins too large") ||
                stringr::str_detect(cond$message, "invalid graphics state")) {
                message("While creating your visualization an error occurred",
                        " that might be caused by a plot window that is too",
                        " small to show your visualization. Resizing the plot",
                        " window in Rstudio, changing the margins size of the visualization",
                        " or saving the visualization might resolve the issue.")
            }
            stop(cond)
        })


        if (igr_grid) {
            if (vis_save) dev.off()
            par(mfrow=c(1,1)) # Reset if this has been changed
        }

    } else if (output_type == "cytoscape") {
        # If plots are not saved keep igraph session open
        for (i in seq_along(networks)) {
            visCytoscape(networks[[i]],
                         directed = directed[[if (directed_match) i else 1]],
                         save_name = save_names[[if (names_match) i else 1]],
                         export_type = vis_export_type %>% stringr::str_to_upper(),
                         close_session = cyto_close_session,
                         save_session = cyto_save_session,
                         export_image = vis_save,
                         export_opts = vis_export_opts,
                         scale_width = vis_edge_factor,
                         cyto3.8_check = cyto3.8_check,
                         radial_labs = vis_radial_labs,
                         node_space = cyto_node_space)
        }

    } else if (output_type == "network") {

        # Check network, e.g. xgmml, saving options
        if (!is.null(netw_xgmml_title)){
            if (!(length(netw_xgmml_title) == n_nets || length(netw_xgmml_title) == 1)) {
                stop("Length of xgmml titles must be 1 or matching with number of matrices: ",
                     "\nℹ Length of `netw_xgmml_title` = ", length(netw_xgmml_title),
                     ", length of `networks` = ", n_nets, ".", call.=FALSE)
            }
        }

        # To make sure the networks are either igraph or graphNEL, otherwise
        #   BioNet::saveNetwork will run into problems
        networks <- lapply(seq_along(networks), function(i) {
            net <- networks[[i]]
            if (is_network_list(net)) {
                igraph::graph_from_data_frame(
                    net$edges, vertices = net$vertices,
                    directed = directed[[if (directed_match) i else 1]])
            } else {
                net
            }
        })

        for (i in seq_along(networks)) {
            netw <- networks[[i]]

            # Avoid overwriting by appending number
            save_name <- file_sequence(name_base = save_names[[if (names_match) i else 1]],
                                       ext = paste0(".", netw_ext))

            if (!is.null(netw_xgmml_title)) {
                xgmml_title <- netw_xgmml_title[[if (length(netw_xgmml_title) == n_nets) i else 1]]
            } else {
                xgmml_title <- save_name
            }

            BioNet::saveNetwork(network = netw,
                                name = xgmml_title,
                                file = save_name,
                                type = netw_ext)
        }
    }
}
