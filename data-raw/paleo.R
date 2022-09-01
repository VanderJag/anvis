# Load functions to create adjacency matrix
source("data-raw/relational_networks.R")

# Read a file from doi:10.1016/j.margeo.2005.02.009
#   on protein levels for patients with sepsis
end_comment <- readLines("data-raw/Elemental_dsdp_48_401_thomas_etal2005.tab") %>%
    stringr::str_detect("^\\*/") %>% which()
datamatrix <- readr::read_tsv("data-raw/Elemental_dsdp_48_401_thomas_etal2005.tab",
                              skip = end_comment) %>%
    dplyr::select(-1)

# Get partial correlations and probability matrix
network_for_visualisation <- PCLRC.gmm(datamatrix,
                                       prob.threshold = 0.95,
                                       Niter=1000,
                                       frac=0.75,
                                       rank.thr=0.3)

# Save just partial correlation values
paleo <- network_for_visualisation$CorrMatFiltered

# Remove additional attributes
rm_attrs <- attributes(paleo) %>% names() %>% {.[!. %in% c("dim", "dimnames")]}
for (at in rm_attrs) attr(paleo, at) <- NULL

# Visualization to compare with the other dataset
## Create group labels
# idx_ccl <- which(stringr::str_detect(colnames(sepsis), "^CCL"))
# idx_il <- which(stringr::str_detect(colnames(sepsis), "^IL"))
# idx_cxcl <- which(stringr::str_detect(colnames(sepsis), "^CXCL"))
# groups <- rep("A", nrow(sepsis))
# groups[idx_il] <- "B"
# groups[idx_ccl] <- "C"
# groups[idx_cxcl] <- "D"
# ## Visualize
# sepsis %>% VisualiseNetwork(node_attrs = "all",
#                             edge_attrs = "all",
#                             group_vec = groups)

usethis::use_data(paleo, overwrite = TRUE)
