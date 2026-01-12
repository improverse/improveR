# MODEL duration ~ wt + age + sex

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


Sys.setenv(OPENSSL_CONF="/dev/null")
if (!webshot::is_phantomjs_installed()) {
  options(download.file.extra="--no-check-certificate")
  webshot::install_phantomjs()
}

#options(download.file.extra = "--no-check-certificate")
#webshot::install_phantomjs()

# Load file

remDataSubject<-readr::read_csv2("remDataSubject.csv")

# Run model

modWtAgeSex <- lm(durationSec ~ wt + age + sex, data=remDataSubject) 
tabModWtAgeSexEstimates <-modWtAgeSex %>% broom::tidy()

#readr::write_csv2(tabModWtAgeSexEstimates,"tabModWtAgeSexEstimates.csv")

tabmodWtAgeSexEstimates<-gtsummary::tbl_regression(modWtAgeSex, intercept=TRUE)%>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma))

gt::gtsave(as_gt(tabmodWtAgeSexEstimates), filename = "tabmodWtAgeSexEstimates.html")
webshot::webshot("tabmodWtAgeSexEstimates.html", "tabmodWtAgeSexEstimates.png", vwidth = 1000, vheight = 800)


# Export
saveRDS(modWtAgeSex, "modWtAgeSex.RDS")

performance::model_performance(modWtAgeSex)
performance::check_model(modWtAgeSex, base_size=7)

# Export

ggsave("graphPerformanceModWtAgeSex.png")
