# COMPARE RESULTS

# Install missing packages

if (!requireNamespace("performance", quietly = TRUE)) {
  install.packages("performance")
}
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}
if (!requireNamespace("see", quietly = TRUE)) {
  install.packages("see")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

# Load Libraries

library(broom)
library(performance)
library(see)
library(tidyverse)

# Load model results

modWt <-readRDS("modWt.RDS")
modWtSex <-readRDS("modWtSex.RDS")
modWtAge <-readRDS("modWtAge.RDS")
modWtAgeSex <-readRDS("modWtAgeSex.RDS")

# Compare model results
comp <- compare_performance(modWt, modWtAge, modWtSex, modWtAgeSex, rank=TRUE)
comp

# Export 
readr::write_csv2(comp, "modelComparison.csv")

plot(comp)
ggsave("graphModelComparison.png")


