# Check if a file with the same base name and extension already exists in the
#     working directory. If it does, keep appending higher numbers to the base
#     name until there is no file with this number present in the folder.
file_sequence <- function(name_base, ext) {
  if (!file.exists(paste0(name_base, ext))) return(name_base)
  i = 2
  repeat {
    seq_next = paste0(name_base, "_", i)
    if (!file.exists(paste0(seq_next, ext))) return(seq_next)
    i = i + 1
  }
}


# Will check whether the file name exist with either of two extensions and will
#   also check whether a network in cytoscape already has the same name.
cyto_file_seq <- function (name_base, ext1, ext2) {
    if (!file.exists(paste0(name_base, ext1)) &&
        !file.exists(paste0(name_base, ext2)) &&
        !(name_base %in% RCy3::getNetworkList())) return(name_base)
    i = 2
    repeat {
        seq_next = paste0(name_base, "_", i)
        if (!file.exists(paste0(seq_next, ext1)) &&
            !file.exists(paste0(seq_next, ext2)) &&
            !(seq_next %in% RCy3::getNetworkList())) return(seq_next)
        i = i + 1
    }
}


# Defaults for NULL values
`%||%` <- function(a, b) if (is.null(a)) b else a

# From gplot package, convert color names like midnightblue to hex
col2hex <- function(cname)
{
  colMat <- col2rgb(cname)
  rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}


# Check if something is a color
are_colors <- function(x) {
  is_color <- sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })

  not_colors <- names(is_color)[!is_color]

  if (!all(is_color)) {
    stop("Must provide valid color names:",
    "\nℹ The following input could not be interpreted as color: ",
    paste0(not_colors, collapse = ", "), ".", call.=FALSE)
  }

  invisible(is_color)
}


# Check if all list elements are named
named_list_check <- function(test_list) {
    list_name <- deparse(substitute(test_list))

    if (!inherits(test_list, "list")) {
        stop("Argument `", list_name, "` must be a list:",
        "\nℹ class(", list_name, "): ", class(test_list), call.=FALSE)
    }

    all_named <- (length(test_list) == sum(names(test_list) != "", na.rm=TRUE))

    if (!all_named) {
        stop("All elements in list `", list_name, "` must be named:",
        "\nℹ names(", list_name, "): ", names(test_list) %>% paste(collapse = ", "),
        "\n✖ Number of non-empty names required: ", length(test_list),
        call.=FALSE)
    }

    return(invisible(all_named))
}


# To create an example directed network from an undirected one
lower_tri_remix <- function(matr) {
    matr[lower.tri(matr)] <- sample(matr[lower.tri(matr)])

    return(matr)
}


# To create graphNEL from edge and node table
graphNEL_from_dfs <- function(edge_table, node_table, directed) {
    igraph_obj <- igraph::graph_from_data_frame(edge_table,
                                                vertices = node_table,
                                                directed = directed)
    nel <- igraph::as_graphnel(igraph_obj)

    return(nel)
}


# To create edge and node table from graphNEL
dfs_from_graphNEL <- function(gr_nel) {
    igraph_obj <- igraph::graph_from_graphnel(gr_nel)

    dfs <- dfs_from_igraph(igraph_obj = igraph_obj)

    return(dfs)
}

dfs_from_igraph <- function(igraph_obj) {
    dfs <- igraph::as_data_frame(igraph_obj, what = "both")

    names(dfs$vertices) <- names(dfs$vertices) %>%
        stringr::str_replace_all("^name$", "node")

    names(dfs$edges) <- names(dfs$edges) %>%
        stringr::str_replace_all("^from$", "source") %>%
        stringr::str_replace_all("^to$", "target")

    return(dfs)
}

is_network_list <- function(obj) {
    if (!inherits(obj, "list")) {
        return(FALSE)
    }

    if (!all(c("vertices", "edges") %in% names(network))) {
        return(FALSE)
    }

    if (!(inherits(obj$vertices, "data.frame") &&
          inherits(obj$edges, "data.frame"))) {
        return(FALSE)
    }

    return(TRUE)
}
