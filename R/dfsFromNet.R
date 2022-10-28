dfsFromNetwork <- function(networks) {

    # Make sure input is a list, we will iterate later
    if (!inherits(networks, "list")) {
        networks <- list(networks)
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
        return(dfs_list)
    }
}
