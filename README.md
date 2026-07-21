# AgriDataTools

## Overview
An enterprise-grade, CRAN-compliant R package engineered for advanced agricultural analytics, plant breeding research, and quantitative genetics. It streamlines complete processing matrices from variance decomposition (ANOVA) to dynamic post-hoc mean performance evaluations, multivariate spaces (PCA), and hierarchical divergence networks.

---

## Technical Features Array
*   **Experimental Variance Partitioning:** High-precision automated structures for complete randomized block layouts.
*   **Post-Hoc Isolation Engines:** Local integration of Fisher's LSD, Tukey's HSD, and Scheffe's mathematical contrasts.
*   **Multivariate Coordinates Mapping:** Exact extraction of Principal Component eigenvalues, rotation loadings, and individual genotype spaces.
*   **Agglomerative Tree Clustering:** Distance network calculations utilizing Euclidean grid layouts and Ward's Minimum Variance method.
*   **Integrated Unified Graphics:** Advanced publication-grade visualization suite supporting standard analytical models, residual checks, modern bar rankings, PCA biplot matrices, and colored cluster trees.

---

## Installation
You can install the development version of AgriDataTools directly into your R environment using the local file path matrix:

```r
# Verify documentation and structural prerequisites are installed
install.packages(c("devtools", "ggplot2", "ggrepel", "ggdendro"))

# Compile and mount the package directly into R session
devtools::install("D:/RStudio/AgriDataTools")