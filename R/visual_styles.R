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
  c25 <- c("#0073C2", "#EFC000", "#868686", "#CD534C", "#008B00", "#6A3D9A", "#FF7F00", "#000000",
            "#7EC0EE", "#90EE90", "#FDBF6F",  "#B03060",
           "#FF83FA", "#00CED1", "#8B4500", "#A52A2A", "#FB9A99", "#CAB2D6",
           "#EEE685", "#FF1493", "#0000FF", "#36648B", "#00FF00", "#8B8B00",
           "#CDCD00")
  # pie(rep(1, 25), col = c25) # to check the colors

  # Are manual colors supplied?
  if(is.null(colors)) {
      return(rep(c25, length.out = n))

    # When manual colors are supplied return a selection of those instead
  } else {

    # Colors must be a character vector
    if (!is.character(colors)) {
      stop("Must provide colors as character vector",
           "\nℹ You provided an object of class: ", class(colors),
           call.=FALSE)
    }

    # Check if the provided colors are recognized by R
    are_colors(colors)

    # ensure cytoscape can read colors by converting to hex
    colors <- col2hex(colors)

    return(rep(colors, length.out = n))
  }
}
