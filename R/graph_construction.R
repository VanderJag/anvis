#' Adjacency matrix to edge list
#'
#' Converts an adjacency matrix into an weighted edge list. Interactions of nodes
#' with themselves are removed. Edges can be directed or undirected.
#'
#' @param adj_matrix Data frame or matrix. Square adjacency matrix. Nodes must be
#'     specified in row names and column names.
#' @param directed Logical (default `FALSE`), whether edges are directed in the
#'     network. If `FALSE`, the information in the lower triangle of the
#'     adjacency matrix will be discarded.
#' @param self_loops Logical (default `FALSE`). Should the values for interaction
#'     of nodes with themselves (diagonal of the adjacency matrix) be retained?
#' @return Returns data frame representing a network as weighted edge list.
#'     Columns are source, target, and weight.
adj_matrix_to_edgelist <- function(adj_matrix, directed = FALSE, self_loops = FALSE) {

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

    # Check if there are missing values in the matrix, replace with 0
    if (sum(is.na(adj_matrix)) > 0) {
        adj_matrix[is.na(adj_matrix)] <- 0
        message("Missing values (`NA`) for edge weights have been replaced with 0.")
    }

    # Remove self interaction
    if (!self_loops) diag(adj_matrix) <- 0

    if (isFALSE(directed)) {
        # If the upper and lower diagonal are not the same we would be
        #     discarding information
        is_equal <- isTRUE(all.equal(adj_matrix[upper.tri(adj_matrix, diag=FALSE)],
                                     t(adj_matrix)[
                                         upper.tri(t(adj_matrix), diag=FALSE)]))
        if(!is_equal) {
            warning("Creating undirected edges from an adjacency matrix with ",
                 "unequal upper and lower triangle. Lower triangle will be ",
                 "discarded, so information will be lost.",
                 "\nℹ To retain all information a directed network can be created.",
                 call.=FALSE)
        }

        # Since the information of the upper and lower triangles of the adjacency
        #     matrix are identical, the lower triangle can be discarded
        adj_matrix[lower.tri(adj_matrix, diag=FALSE)] <- 0
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
adj_matrix_to_nodetable <- function(adj_matrix) {

  data.frame("node" = colnames(adj_matrix))
}


#' Create network from adjacency matrix
#'
#' Creates a graphNEL network from an adjacency matrix, with the option to add
#' various attributes for nodes and edges that are helpful for making neat visualizations.
#' A list of adjacency matrices can be provided as input, to obtain a list of
#' graphNEL networks as return.
#'
#' @section Details:
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
#' For `width_type` several option are available, all of which scale the absolute
#' values of the edge attribute "weight". The most extreme values for "weight"
#' get the highest value for edge width, and vice versa. The range of
#' edge widths will be between 0 and 1, irrespective of the chosen method.
#' Available options are: `"cor"`, which is
#' intended to be used with Pearson or Spearman correlation values (range -1
#' to 1). Widths will be the absolute value of the correlation, scaled with a
#' sigmoid. `"partcor"`, which is intended to be use for partial correlation
#' values (range -1 to 1). For this method, edge widths will be the cube root
#' of absolute weight values, scaled with a sigmoid. The option `"MI"` is meant
#' to be used with weights derived from mutual information (range 0 to +Inf).
#' Widths will be the weights divided by to maximum weight, then scaled with
#' a sigmoid. `"default_scaling"` applies the same transformation as `"MI"`,
#' as a result scaling the any weights to range 0 to 1.
#' `"ranked"` will calculate percentage ranks of the weight, and
#' scale them with a sigmoid. `"percentile"` will chose a set of fixed widths
#' determined by the percentile of a weight value. The highest percentiles will
#' be assigned the largest width, but they are the smallest group, vice versa
#' for the lowest percentiles. This argument can be abbreviated.
#'
#' @param adj_mats A square adjacency matrix or data frame or a list of these.
#'     The data in the matrix is used as edge weights for the network. Row names
#'     and column names specify interacting nodes, and are required.
#' @inheritParams adj_matrix_to_edgelist
#' @param node_attrs Character strings, one or multiple of 'none', 'all' (default), 'group',
#'     'color_group', and 'size'. This argument can be used to choose which
#'     additional attributes should be added to the node table. Selecting 'group'
#'     will append a column to the node table that corresponds to `group_vec`.
#'     Selecting 'color_group' in addition to 'group' will add a column that
#'     contains a color for each node, selected based on the group of this node.
#'     Selecting 'size' will add a column with node sizes, that are based on the
#'     connectivity of the nodes.
#' @param edge_attrs Character strings, one or multiple of 'none, 'all' (default), 'width',
#'     'color'. This argument can be used to choose which additional attributes
#'     should be added to the edge table. Selecting 'width' will add a column to
#'     edge table that contains edge width values. These are determined by scaling
#'     the edge weights to range 0 to 1 using a sigmoid and a method based on
#'     `width_type` (more info in Details section).
#' @param group_vec A vector of character strings that assigns group labels to
#'     the nodes. The order of this vector should match the order of column and
#'     rownames of the input adjacency matrices. If `adj_mats` is a list, a single
#'     group vector can be used if it matches all adjacency matrices.
#'     Alternatively, provide a list of group vectors with one vector for each
#'     adj. matrix in the list. For this information to be added, `node_attrs`
#'     must be 'group' or 'all'.
#' @inheritParams add_colors
#' @inheritParams node_size_connectivity
#' @inheritParams weights_to_color
#' @param colorblind Logical (default `FALSE`), determining if the default colors
#'     should be exchanged for colorblind accessible colors.
#' @param width_type Argument used to convert edge weights into widths for
#'     visualizations that are more easy to interpret. Options are `"default_scaling"`,
#'     `"MI"`, `"cor"`, `"partcor"`, `"ranked"`, and `"percentile"`, or a vector
#'     containing the name of one of the methods for each network.
#'     A detailed description of the options can be found in the Details section.
#' @param arrange_co Logical (default `FALSE`), should nodes be reordered based on
#'     their average connectivity in multiple networks? Requires
#'     the same node names to be present in all networks. Also requires 'size'
#'     column to be present in node tables, so `node_attrs` should be 'all' or
#'     include 'size'.
#'
#' @seealso [addVisAttrs] for adding attributes to an existing network objects.
#'
#'
#' @return This function returns a graphNEL object or a list of these.
#' The nodes and edges of these graph objects will have additional attributes
#' added corresponding with the provided input arguments.
#'
#' @export
#'
#' @examples
#' # Create grouping vector for the nodes of sepsis data
#' proteins <- colnames(sepsis[[1]])
#' groups <- dplyr::case_when(
#'     stringr::str_starts(proteins, "IL") ~ "group A",
#'     stringr::str_starts(proteins, "CCL") ~ "group B",
#'     stringr::str_starts(proteins, "CXCL") ~ "group C",
#'     TRUE ~ "group D")
#'
#' # Create a network with node groups, color, and size, and edge width, and color
#' #   attributes added. We'll use the first network from our sepsis example data
#' adjToNetwork(sepsis[[1]],
#'              node_attrs = "all",
#'              edge_attrs = "all",
#'              width_type = "partcor",
#'              group_vec = groups)
adjToNetwork <- function(adj_mats,
                         directed = FALSE,
                         self_loops = FALSE,
                         node_attrs = c("all", "none", "group",
                                        "color_group", "size"),
                         edge_attrs = c("all", "none", "width", "color"),
                         group_vec = NULL,
                         group_colors = NULL,
                         size_type = c("igraph", "cytoscape", "scaled_only"),
                         arrange_co = FALSE,
                         width_type = NULL,
                         colorblind = FALSE,
                         output_as = c("graphNEL", "igraph", "list"),
                         edge_color_func = NULL) {

    # Check which attributes should be added
    node_attrs <- match.arg(node_attrs, several.ok = TRUE)
    edge_attrs <- match.arg(edge_attrs, several.ok = TRUE)
    output_as <- match.arg(output_as)
    size_type <- match.arg(size_type)

    # Check if input is adjacency matrix or a list of adj matrices
    if(inherits(adj_mats, "data.frame") == TRUE |
       inherits(adj_mats, "matrix") == TRUE) {
        adj_mats <- list(adj_mats)
        # This input does not allow providing a name for the network
        netw_names <- NULL

    } else if (inherits(adj_mats, "list")) {
        # Extract network names to reuse them later
        netw_names <- names(adj_mats)

        for (i in seq_along(adj_mats)) {

            if (!inherits(adj_mats[[i]], "data.frame") &&
                !inherits(adj_mats[[i]], "matrix")) {

                stop("Must provide data.frame, matrix, or list of these as input. \n",
                     "ℹ Element ", i, " of your list is of class: ",
                     class(adj_mats[[i]]),
                     "\n✖ All list elements must be data.frame or matrix.",
                     call. = FALSE)
                }
        }

    } else if (!inherits(adj_mats, "list")) {
        stop("Must provide data.frame, matrix, or list of these as input \n",
             "You provided: ", class(adj_mats), call. = FALSE)
    }


    # Create networks
    networks <- lapply(seq_along(adj_mats), function(i) {
        adj_matrix <- adj_mats[[i]]

        # If the number and column of the adjacency matrix is not equal there may be
        #   missing info and an error with the data input
        if (ncol(adj_matrix) != nrow(adj_matrix)) {
            stop("Adjacency matrix should be a square matrix with equal number of ",
                 "rows and columns. Error occured with adj. matrix nr. ", i,
                 " of the adj. matrix list.", call. = FALSE)
        }
        # node table from adjacency
        node_table <- adj_matrix_to_nodetable(adj_matrix)

        # adjacency to edgelist
        edge_table <- adj_matrix_to_edgelist(adj_matrix,
                                             directed = directed,
                                             self_loops = self_loops)

        return(list("vertices" = node_table, "edges" = edge_table))
    })

    ## Optionally, add additional attributes for visualizations
    networks <- addVisAttrs(network = networks,
                            node_attrs = node_attrs,
                            edge_attrs = edge_attrs,
                            group_vec = group_vec,
                            group_colors = group_colors,
                            size_type = size_type,
                            arrange_co = arrange_co,
                            width_type = width_type,
                            colorblind = colorblind,
                            edge_color_func = edge_color_func)

    ## For a single network the addVisAttrs will remove it from list, the next
    ##    step requires list input
    if (!inherits(networks, "list") || is_network_list(networks)) {
        networks <- list(networks)
    }

    graphNELs <- lapply(networks, function(network) {
        node_table <- network$vertices
        edge_table <- network$edges

        if (output_as == "graphNEL") {
            out <- graphNEL_from_dfs(edge_table = edge_table,
                                  node_table = node_table,
                                  directed = directed)
        } else if (output_as == "igraph") {
            out <- igraph::graph_from_data_frame(edge_table,
                                                 vertices = node_table,
                                                 directed = directed)
        } else {
            out <- network
        }

        return(out)
    })

    names(graphNELs) <- netw_names

    ## To match user input better, unlist the network when there is only one
    ##    in the list
    if (inherits(graphNELs, "list") && length(graphNELs) == 1) {
        graphNELs <- graphNELs[[1]]
    }

    return(graphNELs)
}

#' Add attributes to networks to improve visualizations
#'
#' Takes networks of 3 possible classes and adds edge and node attributes
#' that can be used in network visualization. A list of networks
#' can be provided as input, to add attributes to multiple networks at once.
#'
#' @param network A network or a list of these. Valid classes for the network are
#'     "igraph", "graphNEL", and "list" of two data frames with the names "edges"
#'     and "vertices".
#' @inheritParams adjToNetwork
#'
#' @inheritSection adjToNetwork Details
#'
#' @return
#' Returns the input network(s) with added attributes.
#'
#' @seealso [adjToNetwork] for creating a network from an adjacency matrix and
#' adding node and edge attributes in a single step.
#'
#' @export
#'
#' @examples
#' # Create grouping vector for the nodes of sepsis data
#' proteins <- colnames(sepsis[[1]])
#' groups <- dplyr::case_when(
#'     stringr::str_starts(proteins, "IL") ~ "group A",
#'     stringr::str_starts(proteins, "CCL") ~ "group B",
#'     stringr::str_starts(proteins, "CXCL") ~ "group C",
#'     TRUE ~ "group D")
#'
#' # Create a network without additional attributes. Users that are adding
#' #   attributes an to existing network can skip this step
#' netw <- adjToNetwork(sepsis[[1]],
#'              node_attrs = "none",
#'              edge_attrs = "none")
#'
#' # Add attributes to our network
#' addVisAttrs(netw,
#'             node_attrs = "all",
#'             edge_attrs = "all",
#'             width_type = "partcor",
#'             group_vec = groups)
addVisAttrs <- function(network,
                        node_attrs = c("all", "none", "group",
                                       "color_group", "size"),
                        edge_attrs = c("all", "none", "width", "color"),
                        group_vec = NULL,
                        group_colors = NULL,
                        size_type = c("igraph", "cytoscape", "scaled_only"),
                        arrange_co = FALSE,
                        width_type = NULL,
                        colorblind = FALSE,
                        edge_color_func = NULL) {

    # Check which attributes should be added
    node_attrs <- match.arg(node_attrs, several.ok = TRUE)
    edge_attrs <- match.arg(edge_attrs, several.ok = TRUE)
    size_type <- match.arg(size_type)

    # Due to vectorization of this function we need to ensure the input is list
    if (!inherits(network, "list") || is_network_list(network)) {
        network <- list(network)
    }

    # Check number of matrices for later tests
    n_nets <- length(network)

    network_type_list <- lapply(seq_along(network), function (i) {
        net <- network[[i]]
        if (is(net, "graphNEL")) "graphNEL"
        else if (is(net, "igraph")) "igraph"
        else if (is_network_list(net)) "lists"
        else stop("Input network must be (a list of) graphNEL, igraph, or list containing data ",
                  "frames named 'vertices' and 'edges'. \nℹ Class of your network: ",
                  class(network), call.=FALSE)
    })

    directed_list <- lapply(network, function (net) {
        if (is(net, "graphNEL")) graph::edgemode(net) == "directed"
        else if (is(net, "igraph")) igraph::is_directed(net)
        else if (is_network_list(net)) NA
    })

    graph_attrs_list <- lapply(network, function (net) {
        if (is(net, "graphNEL")) net@graphData
        else if (is(net, "igraph")) igraph::graph_attr(net)
        else if (is_network_list(net)) NA
    })

    network_dfs <- lapply(network, function (net) {
        if (is(net, "graphNEL")) dfs_from_graphNEL(gr_nel = net)
        else if (is(net, "igraph")) dfs_from_igraph(igraph_obj = net)
        else if (is_network_list(net)) net
    })

    # Check if grouping vector is list, if not, turn into list
    if (is.null(group_vec)) {
        group_vec <- lapply(network_dfs, function (x) rep("A", nrow(x$vertices)))
    }

    if (!is.list(group_vec)) {
        group_vec <- list(group_vec)
    } else {
        if (!length(group_vec) == n_nets && !length(group_vec) == 1) {
            stop("Grouping vector list must be of equal length as network list, or length 1",
                 "\nℹ Length of `group_vec` = ", length(group_vec),
                 ", length of `network` = ", n_nets, ".", call.=FALSE)
        }
    }

    if (!is.null(width_type)){
        if (!(length(width_type) == n_nets || length(width_type) == 1)) {
            stop("Length of width type must be 1 or matching with number of networks: ",
                 "\nℹ Length of `width_type` = ", length(width_type),
                 ", length of `networks` = ", n_nets, ".", call.=FALSE)
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


    # Add attributes to the networks
    network_dfs <- lapply(seq_along(network_dfs), function(i) {
        net <- network_dfs[[i]]
        edge_table <- net$edges
        node_table <- net$vertices

        addVisAttrsCore(edge_table = edge_table,
                       node_table = node_table,
                       node_attrs = node_attrs,
                       edge_attrs = edge_attrs,
                       group_vec = group_vec[[
                           if (length(group_vec) == n_nets) i else 1]],
                       group_colors = group_colors,
                       size_type = size_type,
                       width_type = width_type[[
                           if (length(width_type) == n_nets) i else 1]],
                       edge_color_func = edge_color_func)
    })

    # Node ordering by average connectivity
    if (arrange_co) {
        nodes <- lapply(seq_along(network_dfs),
                        function(x) network_dfs[[x]]$vertices)

        nodes <- sort_avg_connectivity(nodes_list = nodes)

        for (i in seq_along(network_dfs)) {
            network_dfs[[i]]$vertices <- nodes[[i]]
        }
    }

    # Convert output into the same class as input
    final_nets <- lapply(seq_along(network_dfs), function(x) {
        network_type <- network_type_list[[x]]
        edge_table <- network_dfs[[x]]$edges
        node_table <- network_dfs[[x]]$vertices
        directed <- directed_list[[x]]
        graph_attrs <- graph_attrs_list[[x]]

        if (network_type == "igraph") {
            igraph_obj <- igraph::graph_from_data_frame(d = edge_table,
                                                        vertices = node_table,
                                                        directed = directed)
            igraph::graph_attr(igraph_obj) <- graph_attrs
            return(igraph_obj)

        } else if (network_type == "graphNEL") {
            nel <- graphNEL_from_dfs(edge_table = edge_table,
                                     node_table = node_table,
                                     directed = directed)
            nel@graphData <- graph_attrs
            return(nel)

        } else if (network_type == "lists") {
            return(list("vertices" = node_table, "edges" = edge_table))
        }
    })

    ## To match user input better, unlist the network when there is only one
    ##    in the list
    if (inherits(final_nets, "list") && length(final_nets) == 1) {
        final_nets <- final_nets[[1]]
    }

    return(final_nets)
}

# Function that adds the visualization attributes to a network
addVisAttrsCore <- function(edge_table,
                           node_table,
                        node_attrs = c("none", "all", "group",
                                       "color_group", "size"),
                        edge_attrs = c("none", "all", "width", "color"),
                        group_vec = NULL,
                        group_colors = NULL,
                        size_type = NULL,
                        width_type = NULL,
                        edge_color_func = NULL) {


    # Adding grouping information

    if ("all" %in% node_attrs ||
        ((!"none" %in% node_attrs) && "group" %in% node_attrs)) {
        if (is.null(group_vec)) {
            stop("Must provide grouping vector:",
                 "\n✖ `group_vec` should not be NULL when `node_attrs` is ",
                 "'all' or 'group'.",
                 call.=FALSE)
        }
        node_table <- group_nodes(node_table, group_vec = group_vec)
    }

    # Add colors for the groups

    if ("all" %in% node_attrs ||
        ((!"none" %in% node_attrs) && "color_group" %in% node_attrs)) {
        node_table <- add_colors(node_table, group_colors = group_colors)
    }


    # Add node size

    if ("all" %in% node_attrs ||
        ((!"none" %in% node_attrs) && "size" %in% node_attrs)) {
        node_table <- node_size_connectivity(node_table = node_table,
                                             edge_table = edge_table,
                                             size_type = size_type)
    }

    # Convert edge weight to edge width

    if ("all" %in% edge_attrs ||
        ((!"none" %in% edge_attrs) && "width" %in% edge_attrs)) {
        edge_table <- edge_weight_to_widths(edge_table, width_type = width_type)
    }

    # Add colour column to edge table

    if ("all" %in% edge_attrs ||
        ((!"none" %in% edge_attrs) && "color" %in% edge_attrs)) {
        edge_table <- weights_to_color(edge_table,
                                       edge_color_func = edge_color_func)
    }

    return(list("vertices" = node_table, "edges" = edge_table))
}
