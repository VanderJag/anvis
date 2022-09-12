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
#' 'node_table'. The 'node_table' data frame has a 'node' column and other
#' columns for the additional attributes that were added. The 'edge_table' has
#' columns 'source' and 'target', and more columns for the added attributes.
#' @export
adjToNetwork <- function(adj_mats,
                         directed = FALSE,
                         self_loops = FALSE,
                         node_attrs = c("none", "all", "group",
                                        "color_group", "size"),
                         edge_attrs = c("none", "all", "width", "color"),
                         group_vec = NULL,
                         group_colors = NULL,
                         size_type = c("igraph", "cytoscape", "scaled_only"),
                         width_type = NULL,
                         colorblind = FALSE,
                         edge_color_func = NULL) {
# TODO width type is vectorized with these options

    # Check which attributes should be added
    node_attrs <- match.arg(node_attrs, several.ok = TRUE)
    edge_attrs <- match.arg(edge_attrs, several.ok = TRUE)

    # Check if input is adjacency matrix or a list of adj matrices
    if(inherits(adj_mats, "data.frame") == TRUE |
       inherits(adj_mats, "matrix") == TRUE) {
        adj_mats <- list(adj_mats)
        # TODO complete the below thing
    # } else if (inherits(adj_mats, "list")) {
    #     df_mat_yn <- inherits(adj_mats, "data.frame") == TRUE || inherits(adj_mats, "matrix")
    #     stop("Must provide data.frame, matrix, or list of these as input \n",
    #          "You provided: ", class(adj_mats), call. = FALSE)
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

    # Optionally, add additional attributes for visualizations
    if (!"none" %in% node_attrs || !"none" %in% edge_attrs) {
        networks <- addVisAttrs(network = networks,
                               node_attrs = node_attrs,
                               edge_attrs = edge_attrs,
                               group_vec = group_vec,
                               group_colors = group_colors,
                               size_type = size_type,
                               width_type = width_type,
                               edge_color_func = edge_color_func)
    }

    graphNELs <- lapply(networks, function(network) {
        node_table <- network$vertices
        edge_table <- network$edges

        graphNEL_from_dfs(edge_table = edge_table,
                          node_table = node_table,
                          directed = directed)
    })

    # TODO if it was one item don't return as list

    return(graphNELs)
}


addVisAttrs <- function(network,
                        node_attrs = c("none", "all", "group",
                                       "color_group", "size"),
                        edge_attrs = c("none", "all", "width", "color"),
                        group_vec = NULL,
                        group_colors = NULL,
                        size_type = NULL,
                        arrange_co = FALSE,

                        width_type = NULL,
                        colorblind = FALSE,
                        edge_color_func = NULL) {

    # TODO implement a check to see what the type of the input is, make sure to convert to list
    # TODO width type is vectorized, see if that is still working, so some input check, check can be found below?

    # Check number of matrices for later tests
    n_mats <- length(network)

    network_type_list <- lapply(network, function (net) {
        # TODO iterate over i and do net <- network[[i]], so i can be used to
        #     tell in the error message where it went wrong
        if (is(net, "graphNEL")) "graphNEL"
        else if (is(net, "igraph")) "igraph"
        else if (is_network_list(net)) "lists"
        else stop("Input network must be graphNEL, igraph, or list containing data ",
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
        else if (is(net, "igraph")) dfs_from_igraph(igraph_obj = network)
        else if (is_network_list(net)) net
    })

    # TODO add tests to the length of group_vec
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


    # Add attributes to the networks
    network_dfs <- lapply(network_dfs, function(net) {
        edge_table <- net$edges
        node_table <- net$vertices

        addVisAttrsCore(edge_table = edge_table,
                       node_table = node_table,
                       node_attrs = node_attrs,
                       edge_attrs = edge_attrs,
                       group_vec = group_vec[[
                           if (length(group_vec) == n_mats) x else 1]],
                       group_colors = group_colors,
                       size_type = size_type,
                       width_type = width_type[[
                           if (length(width_type) == n_mats) x else 1]],
                       edge_color_func = edge_color_func)
    })

    # TODO implement check to see if this part of code is working
    # Node ordering by average connectivity
    if (arrange_co) {
        nodes <- lapply(seq_along(network_dfs),
                        function(x) network_dfs[[x]]$vertices)

        nodes <- sort_avg_connectivity(nodes_list = nodes)

        for (i in seq_along(network_dfs)) {
            network_dfs[[i]]$vertices <- nodes[[i]]
        }
    }

    # TODO if there is only one network return it directly, not as list

    # Convert output into the same class as input
    fin_nets <- lapply(seq_along(network_dfs), function(x) {
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

    return(fin_nets)

}

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

    if ((!"none" %in% node_attrs) &
        ("all" %in% node_attrs | "group" %in% node_attrs)) {
        if (is.null(group_vec)) {
            stop("Must provide grouping vector:",
                 "\n✖ `group_vec` should not be NULL when `node_attrs` is ",
                 "'all' or 'group'.",
                 call.=FALSE)
        }
        node_table <- group_nodes(node_table, group_vec = group_vec)
    }

    # Add colors for the groups

    if ((!"none" %in% node_attrs) &
        ("all" %in% node_attrs | "color_group" %in% node_attrs)) {
        node_table <- add_colors(node_table, group_colors = group_colors)
    }


    # Add node size

    if ((!"none" %in% node_attrs) &
        ("all" %in% node_attrs | "size" %in% node_attrs)) {
        node_table <- node_size_connectivity(node_table = node_table,
                                             edge_table = edge_table,
                                             size_type = size_type)
    }

    # Convert edge weight to edge width

    if ((!"none" %in% edge_attrs) &
        ("all" %in% edge_attrs | "width" %in% edge_attrs)) {
        edge_table <- edge_weight_to_widths(edge_table, width_type = width_type)
    }

    # Add colour column to edge table

    if ((!"none" %in% edge_attrs) &
        ("all" %in% edge_attrs | "color" %in% edge_attrs)) {
        edge_table <- weights_to_color(edge_table,
                                       edge_color_func = edge_color_func)
    }

    return(list("vertices" = node_table, "edges" = edge_table))
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

    adj <- igraph::as_adjacency_matrix(graph = graph,
                                       type = "both",
                                       attr = weight_col,
                                       sparse = F)

    return(adj)
}
