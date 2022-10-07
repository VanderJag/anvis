# -------------------------------------------------------------------------
# When using the PCLRC algorithm please cite the references:
# DOI: 10.1021/acs.jproteome.5b00344
# DOI: 10.1021/acs.jproteome.8b00781

# Find more info and a download for PCLRC on the following page:
# https://www.systemsbiology.nl/download-page/
# -------------------------------------------------------------------------

# Load functions to create adjacency matrix
source("data-raw/relational_networks.R")

library(magrittr)       # for %>% operator


# Read a file from doi:10.1016/j.margeo.2005.02.009
#   on element abundance for a deep sea drilling project
end_comment <- readLines("data-raw/paleo_data_Elemental_dsdp_48_401_thomas_etal2005.tab") %>%
    stringr::str_detect("^\\*/") %>% which()
datamatrix <- readr::read_tsv("data-raw/paleo_data_Elemental_dsdp_48_401_thomas_etal2005.tab",
                              skip = end_comment) %>%
    dplyr::select(-1)

# reorder the elements based on units
# perc <- names(datamatrix) %>% stringr::str_subset("%")
# mass <- names(datamatrix) %>% stringr::str_subset("kg")
# alu <- names(datamatrix) %>% stringr::str_subset("/Al")
# datamatrix <- datamatrix %>% dplyr::select(1, 2, alu, perc, mass)

# Mutual information dataset prep -----------------------------------------

# Get mutual information and probability matrix
network_for_visualisation <- PCLRC.mi(datamatrix)

# Save just mutual information values
paleo_mi <- network_for_visualisation$MiMatFiltered

# Remove additional attributes
rm_attrs <- attributes(paleo_mi) %>% names() %>% {.[!. %in% c("dim", "dimnames")]}
for (at in rm_attrs) attr(paleo_mi, at) <- NULL

usethis::use_data(paleo_mi, overwrite = TRUE)


# Partial correlation datset prep -----------------------------------------

# Get mutual information and probability matrix
network_for_visualisation <- PCLRC.gmm(datamatrix,
                                       prob.threshold = 0.98,
                                       Niter=1000,
                                       frac=0.75,
                                       rank.thr=0.3)

# Save just mutual information values
paleo <- network_for_visualisation$CorrMatFiltered

# Remove additional attributes
rm_attrs <- attributes(paleo) %>% names() %>% {.[!. %in% c("dim", "dimnames")]}
for (at in rm_attrs) attr(paleo, at) <- NULL

usethis::use_data(paleo, overwrite = TRUE)


