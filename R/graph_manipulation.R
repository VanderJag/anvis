

# adds a column with the grouping info to a node table, and sorts it
group_nodes <- function(node_table, group_vec = NULL) {

  # TODO Check if grouping vector has the correct length and ensure it's character type

  # If a grouping vector has been provided
  if (!is.null(group_vec)) {
    node_table$Groups <- group_vec
    node_table <- node_table[order(node_table$Groups),]
  } else {
    node_table$Groups <- "A"
  }
}


add_node_pos <- function(node_table, layout = "circle") {
  X = NULL
  Y = NULL
  R = round(nrow(node_table)/10, 0) * (100)

  if (layout == "circle") {
    # Calculate position in circle
    for (i in 0:(nrow(node_table) - 1)) {
      x = R*cos((i*2*3.14159265359)/(nrow(node_table)))
      X <- as.vector(append(X, x))
      y = R*sin((i*2*3.14159265359)/(nrow(node_table)))
      Y <- as.vector(append(Y, y))
    }
  } else {
    # TODO complete error message
    stop("Must select valid network layout. ",
         "\nℹ you selected: ", layout,
         "\n✖ parameter `layout` must be one of ...", call.=FALSE)
  }

  pos <- as.data.frame(cbind(X,Y))
  node_table <- cbind(node_table, pos)

  return(node_table)
}


# TODO create function that automatically infers the type of scaling for width,
#   by checking the range of the weights
pick_width_type <- function() {
  NULL
}


# requires df with a column called Weight
edge_weight_to_widths <- function(edge_table, type) {
  frac = as.vector(c(2, 3, 4, 6, 10, 15, 24, 36))
  n_edges = nrow(edge_table)
  M = NULL
  for (i in 1:length(frac)) {
    f = frac[i]
    mes = NULL
    mes = round((f * n_edges)/100, 0)
    M <- as.vector(append(M, mes))
  }
  diff = (sum(M)) - (nrow(edge_table))
  ifelse(diff == 0, print("perfect!"), M[8] <- M[8] - diff)

  wids <- as.vector(c(10, 8, 4, 2, 1, 0.5, 0.25, 0.25))
  wid = NULL
  for (j in 1:length(M)) {
    times = M[j]
    value = wids[j]
    wid <- as.vector(append(wid, c(rep(value, times))))
  }

  nthroot = function(x,n) {
    (abs(x)^(1/n))*sign(x)
  }

  #1 is partcor, 2 is cor, 3 is MI, 4 is ranked, 5 is percentile.
  ifelse(type == 1, edge_table <- dplyr::mutate(edge_table, width=sigmoid_xB(x=nthroot(abs(Weight), 3), B=3)),
         ifelse(type == 2, edge_table <- dplyr::mutate(edge_table, width=sigmoid_xB(x=abs(Weight), B=3)),
                ifelse(type == 3, edge_table <- dplyr::mutate(edge_table, width=sigmoid_xB(x=(abs(Weight)/max(abs(df))), B=3)),
                       ifelse(type == 4, edge_table <- dplyr::mutate(edge_table, width = sigmoid_xB(x=(Rank(-Weight)/n_edges), B=3)),
                              if(type == 5){
                                wid <- as.data.frame(wid)
                                edge_table <- edge_table[sort(abs(edge_table$Weight), decreasing=T, index.return=T)[[2]],]
                                edge_table <- cbind(edge_table, wid)
                                colnames(edge_table)[5] <- "width"
                              }
                              else{
                                print("type not selected")
                              }))))

  tmp_x <- seq(0, 1, length.out = 1000)
  plot(tmp_x, sigmoid_xB(tmp_x, 3))
  abline(0,1)
  return(edge_table)
}

weights_to_color <- function(edge_table) {

  # If negative numbers are found in the weights use a diverging color palette,
  #   otherwise use a sequential color palette
  if (min(edge_table$Weight) < 0) {
    Stroke <- as.vector(colorspace::diverging_hcl(n=nrow(edge_table), palette = "Blue-Red"))
    edge_table <- edge_table[sort(edge_table$Weight, decreasing=T, index.return=T)[[2]],]
    edge_table <- cbind(edge_table, Stroke)
  } else {
    Stroke <- as.vector(colorspace::sequential_hcl(n=nrow(edge_table), palette = "Reds2"))
    edge_table <- edge_table[sort(abs(edge_table$Weight), decreasing=T, index.return=T)[[2]],]
    edge_table <- cbind(edge_table, Stroke)
  }

  return(edge_table)
}

