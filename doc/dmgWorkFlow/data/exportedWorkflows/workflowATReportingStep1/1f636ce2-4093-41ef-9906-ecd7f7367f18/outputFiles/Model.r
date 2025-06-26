# MODEL DURATION ~ WT

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
if (!requireNamespace("webshot", quietly = TRUE)) {
  install.packages("webshot")
}

# Load libraries

library(broom)
library(performance)
library(see)
library(tidyverse)
library(gtsummary, quietly=TRUE)
library(webshot)
webshot::install_phantomjs()

# Load file

remDataSubject<-readr::read_csv2("remDataSubject.csv")

# Run model

modWt <- lm(durationSec ~ wt, data=remDataSubject) 
tabModWtEstimates<-modWt %>% broom::tidy()

#readr::write_csv2(tabModWtEstimates,"tabModWtEstimates.csv")

tabmodWtEstimates<-gtsummary::tbl_regression(modWt, intercept=TRUE)%>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma))

gt::gtsave(as_gt(tabmodWtEstimates), filename = "tabmodWtEstimates.html")
webshot::webshot("tabmodWtEstimates.html", "tabmodWtEstimates.png", vwidth = 1000, vheight = 800)


# Export
saveRDS(modWt, "modWt.RDS")

performance::model_performance(modWt)
performance::check_model(modWt, base_size=7)

# Export

ggsave("graphPerformanceModWt.png")
