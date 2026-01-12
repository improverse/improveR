# EDA OF EXPLANATORY VARIABLES

# Check if packages are available, if not install them
# if (!requireNamespace("summarytools", quietly = TRUE)) {
#   install.packages("summarytools")
# }
if (!requireNamespace("reactable", quietly = TRUE)) {
  install.packages("reactable")
}

if (!requireNamespace("ggh4x", quietly = TRUE)) {
  install.packages("ggh4x")
}
if (!requireNamespace("smd", quietly = TRUE)) {
  install.packages("smd")
}
#if (!requireNamespace("webshot", quietly = TRUE)) {
#  install.packages("webshot")
#}

 if (!requireNamespace("webshot2", quietly = TRUE)) {
   install.packages("webshot2")
 }


# Load libraries
library(tidyverse)
library(gtsummary, quietly=TRUE)
library(smd)
library(ggh4x)
library(gt)
library(webshot2)

#Temporarily set the option to bypass the SSL certificate check
options(download.file.extra = "--no-check-certificate")
#webshot::install_phantomjs()
#Sys.setenv(PHANTOMJS_PATH = "/root/bin/phantomjs")

# Load data
remDataSubject <-readr::read_csv2("remDataSubject.csv")
nrow(remDataSubject)
names(remDataSubject)


## add age groups
remDataSubject <- remDataSubject %>%
  mutate(
    ageGroup=case_when(
      age >= 20 & age <=40 ~ "young",
      age >= 40 & age <=65 ~ "middle aged",
      age > 65 ~ "elderly",
      TRUE ~ NA_character_
    ), .after="age"
  ) %>% 
  mutate(ageGroup=factor(ageGroup, levels=c("young", "middle aged", "elderly")))
levels(remDataSubject$ageGroup)


# Overall summary statistics
tableStatsAll<-remDataSubject %>% 
  select(-subject) %>% 
  select(durationSec, everything()) %>% 
  gtsummary::tbl_summary(
    missing_text="missing"
  ) %>% 
  add_n() %>% 
  # bold_labels() %>% 
  modify_caption(caption="**Table: Summary Statistics of Remifentanil Dataset**") %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("Pinheiro J, Bates D, R Core Team (2025); Pinheiro, J. Bates D (2000)")) %>% 
  gt::tab_row_group(
    label="Dependent Variable",
    row=1
  ) %>% 
  gt::tab_options(row_group.default_label="Explanatory Variable") %>% 
  gt::tab_style(
    style    = cell_text(style = "italic", weight="bold"),
    locations = cells_row_groups()
  )

tableStatsAll
gt::gtsave(tableStatsAll, filename = "tableStatsAll.rtf")
gt::gtsave(tableStatsAll, filename = "tableStatsAll.html")
#gt::gtsave(tableStatsAll, filename = "tableStatsAll.png")

# Issue - location of phantom.js installation not found when running script in R batch mode and not interactively in RStudio
#webshot::webshot("tableStatsAll.html", "tableStatsAll.png", vwidth = 1000, vheight = 800)

#webshot::webshot("tableStatsAll.html", "tableStatsAll.png", 
#                 vwidth = 1000, vheight = 800, 
#                 phantomjs = "/root/bin/phantomjs")



# Per sex

## Summary statistics by sex
tableStatsSex<-remDataSubject %>% 
  select(-subject) %>% 
  select(durationSec, everything()) %>% 
  gtsummary::tbl_summary(by=c(sex)) %>% 
  add_overall() %>% 
  add_n() %>% 
  add_difference() %>% 
  bold_labels() %>% 
  modify_caption(caption="**Table: Summary Statistics of Remifentanil Dataset by Sex**") %>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("Pinheiro J, Bates D, R Core Team (2025); Pinheiro, J. Bates D (2000)")) %>% 
  tab_row_group(
    label="Dependent Variable",
    row=1
  ) %>% 
  tab_options(row_group.default_label="Explanatory Variable") %>% 
  tab_style(
    style    = cell_text(style = "italic", weight="bold"),
    locations = cells_row_groups()
  )

tableStatsSex
gt::gtsave(tableStatsSex, filename = "tableStatsSex.rtf")
gt::gtsave(tableStatsSex, filename = "tableStatsSex.html")
#webshot::webshot("tableStatsSex.html", "tableStatsSex.png", vwidth = 1000, vheight = 800)

## Plot
remDataSubjectLong <- remDataSubject %>%
select(
    age,
    sex,
    ht,
    wt,
    bsa,
    lbm,
    durationSec
) %>%
pivot_longer(
    cols=-sex,
    names_to="var",
    values_to = "value"
)

remDataSubjectLong %>%
ggplot()+
labs(
  title="Distribution of Values (Box-Whisker-Raincloud Plot)"
)+
gghalves::geom_half_point_panel(na.rm=T, side="r", 
aes(y=value, x = var, color=sex)) +
gghalves::geom_half_boxplot(
    aes(y=value, x = var),
    na.rm=T, side="l", outlier.color="transparent") +
gghalves::geom_half_violin(
    aes(y=value, x = var),
    na.rm=T, side="r", fill="transparent") +
# scale_y_continuous(limits=c(0,NA))+
scale_color_manual(values=c("Female"="red", "Male"="blue"))+
facet_wrap(vars(var), scales="free")+
# facet_grid(rows = vars(var),
#            cols = vars(sex),
#            scales="free",
#            axes="all")+
coord_flip() +
theme_minimal() +
theme(
    axis.text.y = element_blank(),
    axis.title.y= element_blank(),
    axis.title.x=element_text(size=6, hjust=0, color="grey30"),
    legend.position="top",
    legend.justification="left"
)


# Per age group

## Summary statistics by age group
tableStatsAge <-remDataSubject %>% 
  select(-subject) %>% 
  select(durationSec, everything()) %>% 
  gtsummary::tbl_summary(by=c(ageGroup)) %>% 
  add_overall() %>% 
  add_n() %>% 
  bold_labels() %>% 
  modify_caption(caption="**Table: Summary Statistics of Remifentanil dataset by Age Group**")%>% 
  as_gt() %>% 
  gt::tab_source_note(gt::md("Data: Pinheiro J, Bates D, R Core Team (2025); Pinheiro, J. Bates D (2000)")) %>% 
  tab_row_group(
  label="Dependent Variable",
  row=1
) %>% 
  tab_options(row_group.default_label="Explanatory Variable") %>% 
  tab_style(
    style    = cell_text(style = "italic", weight="bold"),
    locations = cells_row_groups()
  )

tableStatsAge
gt::gtsave(tableStatsAge, filename = "tableStatsAge.rtf")
gt::gtsave(tableStatsAge, filename = "tableStatsAge.html")

#webshot::webshot("tableStatsAge.html", "tableStatsAge.png", vwidth = 1000, vheight = 800)


## Plot
remDataSubject %>%
  select(
    durationSec,
    ageGroup,
    sex,
    ht,
    wt,
    bsa,
    lbm
  ) %>%
  pivot_longer(
    cols=-c(ageGroup, sex),
    names_to="var",
    values_to = "value"
  ) %>%
  # mutate(ageGroup=fct(ageGroup, levels=c("young", "middle aged", "elderly"))) %>% 
  mutate(var=fct(var, levels=c("durationSec","wt", "ht", "bsa", "lbm"))) %>% 
  ggplot()+
  labs(
    title="Distribution of Values per Sex and Age Group (Box-Whisker-Raincloud Plot)",
    subtitle="Note different scaleson the y-axis.",
    caption="Pinheiro J, Bates D, R Core Team (2025); Pinheiro, J. Bates D (2000)"
  )+
  gghalves::geom_half_point_panel(
    na.rm=T, 
    side="r",
    position = position_nudge(x = -0.075), # Nudge points down by 0.5 units
    aes(y=value, x = sex, color=sex)) +
  gghalves::geom_half_boxplot(
    aes(y=value, x = sex),
    na.rm=T, side="l", outlier.color="transparent") +
  gghalves::geom_half_violin(
    aes(y=value, x = sex),
    na.rm=T, side="r", fill="transparent") +
  # scale_y_continuous(limits=c(0,NA))+
  scale_color_manual(values=c("Female"="darkred", "Male"="dodgerblue"))+
  scale_y_continuous(position="left")+
  scale_x_discrete(position="top")+
  facet_grid2(
    # ageGroup ~ var, 
    axes="margins",
    switch="y",
    var ~ ageGroup,
             drop=T,
    labeller=labeller(
      var=c(durationSec="Duration (sec)", wt="WT (kg)", ht="HT (cm)", lbm="LBM", bsa="BSA"),
      ageGroup=\(x) str_to_upper(x)
      ),
    scales="free")+
  theme_minimal()+
  theme(
    axis.title=element_blank(),
    legend.position = "none",
    strip.text.x=element_text(face="bold", hjust=0),
    strip.placement = "outside",
    strip.text.y=element_text(face="bold", hjust=1)
  )
  
ggsave("graphDistributionSexAge.png")

