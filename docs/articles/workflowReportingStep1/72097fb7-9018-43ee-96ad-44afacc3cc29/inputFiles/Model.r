# MODEL duration ~ wt + age


# Intall missing packages

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
if (!requireNamespace("broom.helpers", quietly = TRUE)) {
  install.packages("broom.helpers")
}

# Load libraries

library(broom)
library(performance)
library(see)
library(tidyverse)
library(gtsummary, quietly=TRUE)
library(webshot)
options(download.file.extra = "--no-check-certificate")
webshot::install_phantomjs()

# Load file

remDataSubject<-readr::read_csv2("remDataSubject.csv")

# Run model

modWtAge <- lm(durationSec ~ wt + age, data=remDataSubject) 
tabModWtAgeEstimates<-modWtAge %>% broom::tidy()
tabModWtAgeEstimates

# readr::write_csv2(tabModWtAgeEstimates,"tabModWtAgeEstimates.csv")

tabmodWtAgeEstimates<-gtsummary::tbl_regression(modWtAge, intercept=TRUE)%>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma))

gt::gtsave(as_gt(tabmodWtAgeEstimates), filename = "tabmodWtAgeEstimates.html")
#webshot::webshot("tabmodWtAgeEstimates.html", "tabmodWtAgeEstimates.png", vwidth = 1000, vheight = 800)


# Export
saveRDS(modWtAge, "modWtAge.RDS")

performance::model_performance(modWtAge)
performance::check_model(modWtAge, base_size=7)

# Export

ggsave("graphPerformanceModWtAge.png")
