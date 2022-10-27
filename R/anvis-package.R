#' anvis: A package for automated network visualizations
#'
#' This package provides functions aimed at facilitating the network
#' visualization workflow. Adjacency matrices and igraph or graphNEL objects
#' can be provided as network inputs. Edge weights of input networks will be
#' used to infer visual properties of the network, such as node size or edge
#' color. These visual properties will be integrated into visualizations that
#' are automatically generated with the igraph or Cytoscape software frameworks.
#'
#' @section Network preparation:
#' * Use `adjToNetwork` to convert an adjacency matrix into a network object,
#'   and to add visual attributes of choice.
#' * Use `addVisAttrs` to modify an existing network object by adding
#'   attributes for visualizations.
#'
#' @section Network Visualization:
#' * `anvis()` can be use to visualize networks with added attributes. It
#'   allows choosing "igraph", "cytoscape", or "network" as output types. The
#'   first two options will create visualizations, the last option will export
#'   the network so it can visualized or processed in different software.
#'
#' @docType package
#' @name anvis-package
#'
#' @importFrom magrittr %>%
NULL
