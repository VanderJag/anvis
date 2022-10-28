#' Convert igraph or graphNEL networks to list networks
#'
#' This function takes objects of class "igraph" or "graphNEL" (or a list
#' of these objects), and converts them into a networks represented as lists
#' of data frames.
#'
#' @param networks Objects of class "igraph" or "graphNEL". Can be a single
#'     object or list.
#' @return Returns a list of two dataframes, named "vertices", and "edges". "vertices"
#'     will contain as first column "node" with the node names, additional
#'     columns will be used for further node attributes. The edges data frame
#'     contains as first two columns "source" and "target", and will contain
#'     further columns for edge attributes. Return will be a list of lists
#'     if multiple networks have been provided as input.
#'
#' @export
dfsFromNetwork <- function(networks) {

    # Make sure input is a list, we will iterate later
    if (!inherits(networks, "list")) {
        networks <- list(networks)
    } else {
        # Store network names to add to return
        net_names <- names(networks)
    }

    # Check if all networks are of the required input type graphNEL or igraph
    for (i in seq_along(networks)) {
        net <- networks[[i]]
        if (!(is(net, "graphNEL") || is(net, "igraph"))) {
            stop("Input networks must be graphNEL, or igraph:",
                 "\nℹ Class of your network: ", class(net),
                 "\n Error occured for network nr.: ", i, call.=FALSE)
        }
    }

    # Get networks as data frames
    dfs_list <- lapply(networks, function (net) {

        if (is(net, "graphNEL")) {
            return(dfs_from_graphNEL(net))
        }

        if (is(net, "igraph")) {
            return(dfs_from_igraph(net))
        }
    })

    # If there is a single network take remove it out of list
    if (length(dfs_list) == 1) {
        return(dfs_list[[1]])
    } else {
        # Add orginal network names if there were any
        names(dfs_list) <- net_names

        return(dfs_list)
    }
}
