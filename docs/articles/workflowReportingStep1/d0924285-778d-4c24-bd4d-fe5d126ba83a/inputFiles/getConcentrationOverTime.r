# OUTCOME OF INTEREST


# Load libraries


library(tidyverse)


# Set theme

theme_plot <-theme_minimal() +
  theme(
    legend.title=element_blank(),
    plot.caption = element_text(color="grey50"),
    axis.title.x=element_text(hjust=1, color="grey30", size=8.8),
    axis.title.y=element_text(hjust=1, color="grey30", size=8.8),
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    legend.position = "top",
    legend.justification = "left")

theme_set(theme_plot)


# Load data

remData<-readr::read_csv2("remDataQuality.csv")

txtCaption<-"Pinheiro J, Bates D, R Core Team (2025); Pinheiro, J. Bates D (2000)"

# Plot total
remData %>% 
  mutate(
    phase=ifelse(post_amt_index==0, "injection", "post-injection")
  ) %>%
ggplot(aes(x=timeSec, y=conc, color=phase, group=1)) +
  geom_smooth(se=FALSE) +
  geom_point() +
  labs(title = "Concentration of Remifentanil over Time",
       caption=txtCaption,
       x = "Time (seconds)",
       y = "Concentration (ng/mL)") +
  scale_color_manual(values=c("injection"="#E76F51", "post-injection"="#2A9D8F"))+
  scale_x_continuous(labels=scales::label_comma(), expand=expansion(mult=0))

ggsave("graphConcentrationDuration.png")


# Plot per subject
remData %>% 
  mutate(
    phase=ifelse(post_amt_index==0, "injection", "post-injection")
  ) %>%
ggplot(aes(x=timeSec, y=conc, color=phase, group=1)) +
  geom_point() +
  geom_smooth(se=FALSE, size=.8) +
  labs(title = "Concentration of Remifentanil over Time per Subject",
       caption=txtCaption,
       x = "Time (seconds)",
       y = "Concentration (ng/mL)") +
  scale_color_manual(values=c("injection"="#E76F51", "post-injection"="#2A9D8F"))+
  scale_x_continuous(labels=scales::label_number(
    scale    = 1e-3,    # divide values by 1,000
    suffix   = "K",     # append “K”
    accuracy = 1
  ))+
  facet_wrap(
    vars(subject),
    labeller=labeller(subject=label_both)
  )


ggsave("graphConcentrationDurationSubject.png", 
       scale=2,
       width=10,
       height=20, 
       units="cm")
