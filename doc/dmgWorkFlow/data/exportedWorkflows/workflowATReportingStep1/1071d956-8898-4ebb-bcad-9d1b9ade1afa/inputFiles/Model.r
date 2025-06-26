# MODEL duration ~ wt + sex

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
library(webshot)
library(gtsummary, quietly=TRUE)
webshot::install_phantomjs()
# Load file

remDataSubject<-readr::read_csv2("remDataSubject.csv")

# Run model

modWtSex <- lm(durationSec ~ wt + sex, data=remDataSubject) 
tabModWtSexEstimates<-modWtSex %>% broom::tidy()

#readr::write_csv2(tabModWtSexEstimates,"tabModWtSexEstimates.csv")


tabmodWtSexEstimates<-gtsummary::tbl_regression(modWtSex)%>% 
  add_glance_table(
    label = list(sigma = "\U03C3"),
    include = c(r.squared, AIC, sigma))

gt::gtsave(as_gt(tabmodWtSexEstimates), filename = "tabmodWtSexEstimates.html")
webshot::webshot("tabmodWtSexEstimates.html", "tabmodWtSexEstimates.png", vwidth = 1000, vheight = 800)


# Export
saveRDS(modWtSex, "modWtSex.RDS")
performance::model_performance(modWtSex)


# Export

performance::check_model(modWtSex, theme=ggplot2::theme_classic(base_size=5)+theme(axis.tile=element_text(size=2)))
ggsave("graphPerformanceModWtSex.png")
