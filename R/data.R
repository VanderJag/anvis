#' Partial correlation between the levels of 36 proteins measured in blood plasma
#'
#' A dataset containing partial correlations values, that indicate how similar the
#' levels of selected blood plasma proteins behave. The protein levels were
#' measured in a range of hospital patients, which can be assigned to different
#' groups based on the type of their disease.
#'
#' @format ## `sepsis`
#' A list of 12 named elements that relate to different groups of hospital
#' patients. Each list element corresponds to a matrix with
#' 36 rows and 36 columns, representing the 36 proteins of which the concentration
#' was measured. Each cell in the matrix shows the partial correlation between
#' the levels of proteins measured in blood plasma. The matrices are symmetrical
#' over the diagonal.
#'
#' List elements were made by creating subsets of hospital patients, for which
#' partial correlation was estimated separately. The names of the list element
#' and the subgroup of patients they relate to is as follows (note that NSTI
#' stands for necrotizing soft tissue infection):
#'
#' \describe{
#'   \item{Hospital patients}{NSTI patients}
#'   \item{Control group 1}{Non NSTI patients}
#'   \item{Control group 2}{Cellulitis patients}
#'   \item{Surgical control}{Surgical control patients}
#'   \item{Septicshock patients}{NSTI patients who had septicshock}
#'   \item{No-septicshock patients}{NSTI patients who didn't have septicshock}
#'   \item{Mono-microbial patients}{NSTI caused by 1 bacterial species (mono-microbial)}
#'   \item{Poly-microbial patients}{NSTI caused by several species (poly-microbial)}
#'   \item{Mono-microbial septicshock patients}{Mono-microbial patients with septicshock}
#'   \item{Mono-microbial no-septicshock patients}{Mono-microbial patients without septicshock}
#'   \item{Poly-microbial septicshock patients}{Poly-microbial patients with septicshock}
#'   \item{Poly-microbial no-septicshock patients}{Poly-microbial patients without septicshock}
#' }
#'
#' @source For more information on the raw data or a download thereof:
#' \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.f1vhhmgw4}
"sepsis"


#' Mutual information matrix for elements detected in deep sea drilling
#'
#' A dataset containing mutual information (MI) values, that indicate which trace
#' elements show the same trends in the samples of a deep sea drilling experiment.
#' The MI data presented here was generated from element abundance data that has
#' previously been investigated to study the environmental change that
#' occurred late Paleocene-early Eocene.
#'
#' @format ## `paleo_mi`
#' A matrix made of 38 rows and columns. The rows and columns represent
#' several trace elements and depth and age of the ground. The cells of the matrix
#' give mutual information values that give an estimate for the extent to which
#' the abundance of the elements show the same patterns.
#'
#' @source For more information on the raw data or a download thereof:
#' \url{doi:10.1016/j.margeo.2005.02.009}
"paleo_mi"


#' Partial correlation matrix for elements detected in deep sea drilling
#'
#' A dataset containing partial correlation values, that indicate which trace
#' elements show the same trends in the samples of a deep sea drilling experiment.
#' The data presented here was generated from element abundance data that has
#' previously been investigated to study the environmental change that
#' occurred late Paleocene-early Eocene.
#'
#' @format ## `paleo`
#' A matrix made of 38 rows and columns. The rows and columns represent
#' several trace elements and depth and age of the ground. The cells of the matrix
#' give mutual information values that give an estimate for the extent to which
#' the abundance of the elements show the same patterns.
#'
#' @source For more information on the raw data or a download thereof:
#' \url{doi:10.1016/j.margeo.2005.02.009}
"paleo"












