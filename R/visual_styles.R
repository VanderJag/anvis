#' Select n colors of distinct appearance
#'
#' This function contains a pre-selected collection of colors and will provide
#' the selected number of these. Optionally, custom vectors with colors can be
#' provided. If the number of requested colors exceeds the avaible colors (25),
#' the available colors will be repeated.
#'
#' @param n Single numeric value that determines how many colors are returned.
#' @return A character vector containing the number of requested colors
#'
#' @examples
#' n_distinct_cols(6)
n_distinct_cols <- function(n, colors = NULL) {

  # check correct n is supplied
  if (!is.numeric(n) | length(n) > 1) {
    stop("Must provide a single number to select colors",
         "\nℹ class(n) for your n: ", class(n),
         call.=FALSE)
  }

  # Check if at least one color is selected
  if (n < 1) {
    stop("Must select at least one color",
    "\nℹ argument n is: ", n,
    "\n✖ n must be 1 or greater", call.=FALSE)
  }

  # Round n and convert into integer
  n <- as.integer(round(n, digits = 0))

  # Define colors palettes with distinct colors
  sanjee_cols4 <- as.vector(c("#0073C2", "#EFC000", "#868686", "#CD534C"))
  c16 <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1",
           "skyblue2", "palegreen2", "#FDBF6F", "gray70", "maroon", "orchid1",
           "darkturquoise", "darkorange4", "brown")
  c25 <- c("dodgerblue2", "#E31A1C", # red
           "green4", "#6A3D9A", # purple
           "#FF7F00", # orange
           "black", "gold1", "skyblue2", "#FB9A99", # lt pink
           "palegreen2", "#CAB2D6", # lt purple
           "#FDBF6F", # lt orange
           "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
           "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown")

  # Are manual colors supplied?
  if(is.null(colors)) {
    # If fewer colors are returned we try to select prettier ones
    if (n <= 4) {
      return(sanjee_cols4[1:n])
    } else if (n <= 16) {
      return(c16[1:n])
    } else if (n <= 25) {
      return(c25[1:n])
    } else if (n > 25) {
      return(rep(c25, length.out = n))
    }
    # When manual colors are supplied return a selection of those instead
  } else {

    # Colors must be a character vector
    if (!is.character(colors)) {
      stop("Must provide colors as character vector",
           "\nℹ You provided an object of class: ", class(colors),
           call.=FALSE)
    }

    return(rep(colors, length.out = n))
  }
}
