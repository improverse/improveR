# CREATE CROSS-SECTION SUBJECT LEVEL DATA

# Load libraries
library(tidyverse)

# Load data
remData<-readr::read_csv2("remDataQuality.csv")
names(remData)

# Get unique cross-section subject data
remDataSubject <- remData %>%
distinct(
    subject, age, sex, ht, wt, bsa, lbm
)

# Export
readr::write_csv2(remDataSubject, "remDataSubject.csv")

# GET DURATION OF INTEREST

# identify location of first threshold value
threshold_min <- 0.5

# Keep only period before conc falls below threshold_min for the first time
remDataDuration <- remData %>%
  select(subject, time, timeSec, conc, post_amt_index) %>% 
  filter(post_amt_index > 0) %>% 
  mutate(
    conc_min
    =ifelse(conc<threshold_min, 1, NA), 
    .after=conc  # 0.5 ng/mL is the cut-off value for relevant concentration
  )  %>%
  group_by(subject) %>%
  fill(conc_min, .direction="down") %>%
  ungroup() %>% 
  filter(is.na(conc_min)) %>% 
  select(-conc_min)




# Calculate duration
remDataDuration<-remDataDuration %>%   
  group_by(subject) %>%
  mutate(
    timeSec_min=min(timeSec),
    timeSec_max=max(timeSec),
    durationSec=max(timeSec)-min(timeSec)
  ) %>%
  ungroup()

remDataDurationSubject<-remDataDuration %>% 
  distinct(subject, durationSec)

# Combine subject level data
nrow(remDataSubject)
nrow(remDataDurationSubject)

remDataSubject<-remDataSubject %>% 
  left_join(remDataDurationSubject, by="subject")

# Export
readr::write_csv2(remDataSubject, "remDataSubject.csv")
readr::write_csv2(remDataDuration, "remDataDuration.csv")
