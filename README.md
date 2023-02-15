# anvis

This repository contains the source files for `anvis`: Automated Network 
VISualization. The package is aimed at facilitating interpretation and 
comparison of networks by visualizing derived network properties.

Below a demonstration of the type of visualization that `anvis` would 
generate by default: 

![A network visualization generated by anvis.](vignettes/igraph_images/saved_image.svg)

### Installation and use

For installation of the package run:

```
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
devtools::install_github("VanderJag/anvis", build_vignettes = TRUE)
```

This command for installation of the package will also build the vignette. Open this
document using `vignette("Vignette", package = "anvis")`, to find an instruction on 
how to use the package and to find additional tips.

Use the following for installation once the package is available on 
Bioconductor. 

```
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("anvis")
```
