# library(magrittr)       # for %>% operator

# Load functions to create adjacency matrix
source("data-raw/relational_networks.R")

# Read a file from https://datadryad.org/stash/dataset/doi:10.5061/dryad.f1vhhmgw4
#   on protein levels for patients with sepsis
datamatrix <- readr::read_tsv("data-raw/Set01_imputed.txt") %>%
    dplyr::select(-c("IL-1ra", "IL-23_ELISA", "IL_33_ELISA"))

# function to remove information columns later
rm_info <- . %>% dplyr::select(-c(1:12))

# Create subsets for groups of patients
# In data the variable 'Type' indicates the type of infection and NSTI patient had.
#           'Type I': Polymicrobial etiology, 'Type II': Monomicrobial etiology
sub_groups <- list(
    "NSTI patients" = datamatrix %>% dplyr::filter(Case == "NSTI") %>% rm_info(),
    "Non_NSTI patients" = datamatrix %>% dplyr::filter(Case == "Non-NSTI") %>% rm_info(),
    "Cellulitis patients" = datamatrix %>% dplyr::filter(Case == "Cellulitis") %>% rm_info(),
    "Surgical control patients" = datamatrix %>%
        dplyr::filter(Case == "Surgical control") %>% rm_info(),
    "Septicshock NSTI patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Case == "NSTI" && Septicshock == 1) %>% rm_info(),
    "No-septicshock NSTI patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Case == "NSTI" && Septicshock == 0) %>% rm_info(),
    "Mono-microbial NSTI patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Case == "NSTI" && Type == "Type II") %>% rm_info(),
    "Poly-microbial NSTI patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Case == "NSTI" && Type == "Type I") %>% rm_info(),
    "Mono-microbial septicshock patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Type == "Type II" && Septicshock == 1) %>% rm_info(),
    "Mono-microbial no-septicshock patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Type == "Type II" && Septicshock == 0) %>% rm_info(),
    "Poly-microbial septicshock patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Type == "Type I" && Septicshock == 1) %>% rm_info(),
    "Poly-microbial no-septicshock patients" = datamatrix %>% dplyr::rowwise() %>%
        dplyr::filter(Type == "Type I" && Septicshock == 0) %>% rm_info()
)


# Get partial correlations and probability matrix
vis_data <- lapply(sub_groups, function (x) PCLRC.gmm(x,
                                       prob.threshold = 0.95,
                                       Niter=1000,
                                       frac=0.75,
                                       rank.thr=0.3))

sepsis <- lapply(vis_data, function (x) x$CorrMatFiltered)

# Remove additional attributes
rm_attrs <- attributes(sepsis[[1]]) %>% names() %>% {.[!. %in% c("dim", "dimnames")]}
for (i in seq_along(sepsis)) {
    for (at in rm_attrs) attr(sepsis[[i]], at) <- NULL
}



# Visualization to compare with the other dataset
## Create group labels
# idx_ccl <- which(stringr::str_detect(colnames(sepsis[[1]]), "^CCL"))
# idx_il <- which(stringr::str_detect(colnames(sepsis[[1]]), "^IL"))
# idx_cxcl <- which(stringr::str_detect(colnames(sepsis[[1]]), "^CXCL"))
# groups <- rep("A", nrow(sepsis[[1]]))
# groups[idx_il] <- "B"
# groups[idx_ccl] <- "C"
# groups[idx_cxcl] <- "D"
# ## Visualize
# nets <- adjToNetwork(adj_mats = sepsis, width_type = "partcor", node_attrs = "all",
#                      edge_attrs = "all", group_vec = groups, arrange_co = T)
# anvis(nets, igr_grid_names = T, vis_save = T)
# anvis(nets, output_type = "igraph", vis_save = T, igr_grid = c(2,6),
#       vis_export_opts = list(width = 6400, height = 2600),
#       igr_par_opts = list(mar=c(2,4,5,4)),
#       igr_grid_names = T)

usethis::use_data(sepsis, overwrite = TRUE)
