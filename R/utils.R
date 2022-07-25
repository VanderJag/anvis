file_sequence <- function(name_base, ext) {
  if (!file.exists(paste0(name_base, ext))) return(name_base)
  i = 2
  repeat {
    seq_next = paste0(name_base, "_", i)
    if (!file.exists(paste0(seq_next, ext))) return(seq_next)
    i = i + 1
  }
}

file_pair_seq <- function(name_base, ext1, ext2) {
  if (!file.exists(paste0(name_base, ext1)) &&
      !file.exists(paste0(name_base, ext2))) return(name_base)
  i = 2
  repeat {
    seq_next = paste0(name_base, "_", i)
    if (!file.exists(paste0(seq_next, ext1)) &&
        !file.exists(paste0(seq_next, ext2))) return(seq_next)
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

