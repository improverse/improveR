#Load packages
library(tidyverse)


#Install missing packages
if (!requireNamespace("visdat", quietly = TRUE)) {
  devtools::install_github("ropensci/visdat")
}
packageVersion("visdat")

if (!requireNamespace("naniar", quietly = TRUE)) {
  install.packages("naniar")
}
if (!requireNamespace("reactablefmtr", quietly = TRUE)) {
  install.packages("reactablefmtr")
}

install.packages("htmlwidgets")

library(visdat)
library(naniar)
library(reactable)
library(reactablefmtr)
library(htmlwidgets)


#Set default theme
theme_default <- theme_minimal()+
  theme(
    plot.title.position = "panel",
    plot.subtitle = element_text(color="grey30"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust=0, color="grey30", size=6),
    axis.text=element_text(size=6),
    axis.text.y=element_text(size=5),
    legend.box.margin = ggplot2::margin(l=0, unit="cm"),
    legend.margin = ggplot2::margin(l=0, unit="cm"),
    legend.justification = "left",
    legend.position = "top",
    legend.key.height = unit(0.2, "cm"),
    legend.key.width = unit(0.2, "cm"),
    legend.text = element_text(hjust = 1, color = "grey30", face = "italic", size = rel(.8), margin=ggplot2::margin(l=0, unit="cm")),
    legend.box.just = "left",
    legend.key.size = unit(8, "pt"),  # Set legend key size to match text size legend.key.size = unit(8, "pt"),  # Set legend key size to match text size 8
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=6, hjust=0, color="grey30"),
    legend.title=element_blank(),
    strip.text = element_text(hjust = 0, size=8)
  )

theme_set(theme_default)


#Load data
remDataStd<-readr::read_csv2("remDataStd.csv")

#Get dimensions of dataset
names(remDataStd)
dim(remDataStd)

#Check for missing data
naniar::miss_var_summary(remDataStd)

#Plot position of missing data
remDataStd %>%
group_by(subject) %>%
data_vis_miss() %>% 
ggplot(aes(x=rows, y=variable, fill=valueType))+
labs(
  title = "Missing Data per Subject",
  subtitle=str_wrap("Missing data is non-randomly distributed. For every subject there are two observations with missing conc values. The first missing conc value occurs with an individual's first observation. The second after the end of the compound's injection.", width=100),
  caption=
    "Source: Pinheiro J, Bates D, R Core Team (2025). nlme: Linear and Nonlinear Mixed Effects Models. R package version 3.1-168, https://CRAN.R-project.org/package=nlme;\nPinheiro, J. C. and Bates, D. M. (2000). Mixed-Effects Models in S and S-PLUS, Springer, New York.; ",
  y= "Variables",
  x = "Observation index"
  ) +
geom_raster()+
scale_fill_manual(values=c("TRUE"="red", "FALSE"="lightgrey"), labels=c("TRUE"="missing", "FALSE"="present"))+
scale_y_discrete(guide = guide_axis(n.dodge = 2))+
facet_wrap(vars(subject), labeller = labeller(subject = label_both))

##Save plot
ggsave("missingData.png", dpi=300)

  
#Investigate position of missing data 
## create two new indicators
## subject_row_index: row number nested within subjects
## post_amt_index: row number nested within subjects; starts with one at
## first observation after injection finished (before NA).
remDataStd <- remDataStd %>%
  group_by(subject) %>%
  mutate(
    subject_row_index=row_number(), .after=id
  ) %>%
  mutate(
    post_amt_index=ifelse(amt==0, 1, 0),
  ) %>%
  mutate(
    post_amt_index=cumsum(post_amt_index)
  ) %>%
  ungroup()

# Convert time to seconds
convert_to_seconds <- function(x) {
  mins <- floor(x)
  secs <- round((x - mins) * 100)  # multiply by 100 to treat decimal as seconds
  mins * 60 + secs
}

remDataStd <- remDataStd %>%
  mutate(timeSec=convert_to_seconds(time), .after=time)

#Export data with newly added variables indicating missing observations
readr::write_csv2(remDataStd, "remDataQuality.csv") 

#Create table displaying rows with missing conc values and their subject_row_index and post_amt_index
concNA <- remDataStd %>%
filter(is.na(conc)) %>%
select(
  subject, subject_row_index, post_amt_index
)




#Create GRAPH showing frequency of missing data locations
concNA

concNA %>% 
  ggplot()+
  labs(
    title = "Frequency of locations of missing data in conc column",
    subtitle=str_wrap("Missing conc data is either located at subject_row_index 1 or at post_amt_index 1. There are no other locations with missing conc observations in the data. This means
    that concentration data is missing at the moment of when the injection started (subject_row_index==1), and at the first observation after the injection ended (post_amt_index==1). Locations with
                      no missing data not displayed.", width=100),
    caption="Source: Pinheiro J, Bates D, R Core Team (2025). nlme: Linear and Nonlinear Mixed Effects Models. R package version 3.1-168, https://CRAN.R-project.org/package=nlme;\nPinheiro, J. C. and Bates, D. M. (2000). Mixed-Effects Models in S and S-PLUS, Springer, New York.;\nGraph: Scienteco",
    y= "Number of missing data")+
  geom_bar(
    aes(x=as.factor(1),
        fill=as.factor(post_amt_index)), 
    stat="count")+
  scale_y_continuous(position="right")+
  scale_fill_manual(values=c("0"="dodgerblue", "1"="green"))+
  facet_grid(rows=vars(subject_row_index), cols=vars(post_amt_index),
             labeller=labeller(.rows=label_both, .cols=label_both),
             switch="y")+
  theme(
    strip.text.y.left = element_text(angle = 0),
    axis.title.x = element_blank(),
    axis.title.y=element_text(color="grey50", size=6, hjust=1, vjust=0),
    axis.text.x=element_blank(),
    legend.position="none")
  
##Save plot
ggsave("missingDataLocationFrequency.png", dpi=300)
  
