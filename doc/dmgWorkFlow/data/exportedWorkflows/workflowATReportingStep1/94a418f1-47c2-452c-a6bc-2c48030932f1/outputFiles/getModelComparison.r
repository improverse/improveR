# COMPARE RESULTS

# Install missing packages

if (!requireNamespace("gtsummary", quietly = TRUE)) {
  install.packages("gtsummary")
}
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom")
}

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}
if (!requireNamespace("webshot", quietly = TRUE)) {
  install.packages("webshot")
}
if (!requireNamespace("performance", quietly = TRUE)) {
  install.packages("performance")
}
# Load Libraries

library(broom)
library(performance)
#library(see)
library(tidyverse)
library(gtsummary)
library(webshot)
webshot::install_phantomjs()

# Set theme
theme_gtsummary_journal(
  journal = c("jama"),
  set_theme = TRUE
)

# Load model results

modWt <-readRDS("modWt.RDS")
modWtSex <-readRDS("modWtSex.RDS")
modWtAge <-readRDS("modWtAge.RDS")
modWtAgeSex <-readRDS("modWtAgeSex.RDS")

tblModWt <- modWt %>% 
  tbl_regression() %>% 
  add_significance_stars() %>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )
# tblModWt

tblModWtSex <- modWtSex %>% 
  tbl_regression() %>% 
  # add_glance_table() %>% 
  add_significance_stars() %>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )
# tblModWt

tblModWtAge <- modWtAge %>% 
  tbl_regression() %>% 
  # add_glance_table() %>% 
  add_significance_stars() %>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )
# tblModWtAge

tblModWtAgeSex <- modWtAgeSex %>% 
  tbl_regression() %>% 
  # add_glance_table() %>% 
  add_significance_stars() %>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma)
  )

tbl_sideby <- tbl_merge(
  tbls = list(tblModWt, tblModWtSex, tblModWtAge, tblModWtAgeSex),
  tab_spanner = c(
    "**Model 1:** wt", 
    "**Model 2:** wt + sex",
    "**Model 3:** wt + age",
    "**Model 4:** wt + age + sex"
    )
  ) %>% 
  modify_table_body(~.x |> dplyr::arrange(row_type == "glance_statistic"))

tbl_sideby
gt::gtsave(tbl_sideby %>% as_gt(), filename = "tableResultsCompared.html")
gt::gtsave(tbl_sideby %>% as_gt(),  filename = "tableResultsCompared.rtf")
webshot::webshot("tableResultsCompared.html", "tableResultsCompared.png", vwidth = 1000, vheight = 800)


