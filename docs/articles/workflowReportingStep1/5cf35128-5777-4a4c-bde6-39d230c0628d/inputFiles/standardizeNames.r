# Standardize variable names

# Load data
remData<-readr::read_csv2("remData.csv")


# Install package if not already available
if (!requireNamespace("janitor", quietly = TRUE)) {
  install.packages("janitor")
}

# Load packages
library(tidyverse)
library(janitor)

# Standardize names
remData<-remData %>% 
  janitor::clean_names()

names(remData)

# Save data with standardized names
readr::write_csv2(remData, "remDataStd.csv")
