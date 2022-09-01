# Load functions to create adjacency matrix
source("data-raw/relational_networks.R")

# Read a file from https://datadryad.org/stash/dataset/doi:10.5061/dryad.f1vhhmgw4
#   on protein levels for patients with sepsis
datamatrix <- readr::read_tsv("data-raw/Set01_imputed.txt") %>%
    dplyr::select(-c(1:12), -c("IL-1ra", "IL-23_ELISA", "IL_33_ELISA"))

# Get partial correlations and probability matrix
network_for_visualisation <- PCLRC.gmm(datamatrix,
                                       prob.threshold = 0.95,
                                       Niter=1000,
                                       frac=0.75,
                                       rank.thr=0.3)

# Save just partial correlation values
sepsis <- network_for_visualisation$CorrMatFiltered

# Remove additional attributes
rm_attrs <- attributes(sepsis) %>% names() %>% {.[!. %in% c("dim", "dimnames")]}
for (at in rm_attrs) attr(sepsis, at) <- NULL

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

usethis::use_data(sepsis, overwrite = TRUE)
