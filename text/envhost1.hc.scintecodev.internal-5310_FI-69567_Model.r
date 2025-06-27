# MODEL DURATION ~ WT


# Load libraries

library(broom)
library(performance)
library(see)
library(tidyverse)
library(gtsummary, quietly=TRUE)
library(webshot)
Sys.setenv(OPENSSL_CONF="/dev/null")
if (!webshot::is_phantomjs_installed()) {
  options(download.file.extra="--no-check-certificate")
  webshot::install_phantomjs()
}

# Load file

remDataSubject<-readr::read_csv2("remDataSubject.csv")

# Run model

modWtSex <- lm(durationSec ~ wt + sex, data=remDataSubject) 
tabModWtSexEstimates<-modWtSex %>% broom::tidy()

#readr::write_csv2(tabModWtSexEstimates,"tabModWtSexEstimates.csv")

tabmodWtSexEstimates<-gtsummary::tbl_regression(modWtSex, intercept=TRUE)%>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma))

gt::gtsave(as_gt(tabmodWtSexEstimates), filename = "tabmodWtSexEstimates.html")
webshot::webshot("tabmodWtSexEstimates.html", "tabmodWtSexEstimates.png", vwidth = 1000, vheight = 800)


# Export
saveRDS(modWtSex, "modWtSex.RDS")

performance::model_performance(modWtSex)
performance::check_model(modWtSex, base_size=7)

# Export

ggsave("graphPerformanceModWtSex.png")

