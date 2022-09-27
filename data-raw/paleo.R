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
#   on protein levels for patients with sepsis
end_comment <- readLines("data-raw/paleo_data_Elemental_dsdp_48_401_thomas_etal2005.tab") %>%
    stringr::str_detect("^\\*/") %>% which()
datamatrix <- readr::read_tsv("data-raw/paleo_data_Elemental_dsdp_48_401_thomas_etal2005.tab",
                              skip = end_comment) %>%
    dplyr::select(-1)



# Get partial correlations and probability matrix
network_for_visualisation <- PCLRC.mi(datamatrix)

# Save just partial correlation values
paleo <- network_for_visualisation$MiMatFiltered

# Remove additional attributes
rm_attrs <- attributes(paleo) %>% names() %>% {.[!. %in% c("dim", "dimnames")]}
for (at in rm_attrs) attr(paleo, at) <- NULL

usethis::use_data(paleo, overwrite = TRUE)
