# Enhanced Meta-Analysis Script - Package Installation
cat("Installing required packages for Enhanced Meta-Analysis Script...\n")

required_packages <- c(
  "meta", "metafor", "dplyr", "ggplot2", "gridExtra",
  "RColorBrewer", "viridis", "readr", "stringr", "purrr",
  "forestplot", "grid", "ggrepel", "patchwork", "scales", "pacman"
)

optional_packages <- c("Cairo")

install_if_missing <- function(packages, optional = FALSE) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Installing", pkg, "...\n"))
      install.packages(pkg, dependencies = TRUE)
    }
  }
}

install_if_missing(required_packages)
install_if_missing(optional_packages, optional = TRUE)

cat("\n✓ Package installation complete!\n")
