# Load packages
library(ggstream)
library(ggtext)
library(ragg)
library(readxl)
library(showtext)
library(tidyverse)

#Data cleaning and wrangling
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')
ceerope <- read_excel("country--field_structure_of_parliament.xls") %>%
           rename(country = Country)
ceerope$subregion <- "Central and Eastern Europe"
ceerope$country <- gsub("Czech Republic", "Czechia", ceerope$country)


arewereallyfree <- ceerope %>%
                   select(country, subregion) %>%
                   right_join(freedom) %>%
                   drop_na(subregion) 

no <- arewereallyfree %>%
      mutate(Status = case_when(Status == "F" ~ "Free",
                                Status == "PF" ~ "Partially Free",
                                Status == "NF" ~ "Not Free")) %>%
      mutate(Status = factor(Status, levels = c("Free", "Partially Free", "Not Free"))) %>%
      group_by(year, Status) %>% 
      summarise(total_countries = n())

#fonts
font_add(family = "regular", "Barlow Semi Condensed-Regular.ttf")
font_add(family = "semibold", "BarlowSemiCondensed-SemiBold.ttf")
showtext_auto() 

#colours
sadness <- c("#1C658C", "#398AB9", "#D8D2CB")


#plot
n <- no %>% ggplot(aes(year, total_countries, fill = Status)) +
       geom_stream(type = "proportional") +
       scale_fill_manual(values = sadness) +
       coord_cartesian(expand=FALSE) +
       guides(fill = guide_legend(title = "Country Status")) +
       xlab("") + ylab("Proportion of Central and Eastern European Countries") + xlim(1995, 2020) +
       labs(title = "Proportion of free countries in Central and Eastern Europe",
           subtitle = "The proportion of <span style='color:#1C658C'>free countries</span> has slightly fallen off over there in recent years :(", 
           caption= "Lucas Couto | #TidyTuesday | Twitter: @lucas_coutoz | Data: Freedom House and IPU") +
       theme(
         #Title, Subtitle, Caption
         plot.title = element_markdown(family = "semibold", hjust = 0.5, vjust = 0.5, size = 60, color = "black"),
         plot.title.position = "plot",
         plot.subtitle = element_markdown(family="semibold", size = 45, hjust = 0.5, color = "black"),
         plot.caption = element_text(family="semibold", size = 35, color = "black", hjust = 1),
         plot.caption.position = "plot",
         #Panel and Background
         panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         plot.background = element_rect(fill = "#EEEEEE", linetype = 'blank'),
         panel.background = element_rect(fill = "#EEEEEE"),
         plot.margin = ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
         #Axes
         axis.title = element_text(size = 60, family = "semibold", color = "black"),
         axis.text.y = element_text(size = 40, family = "regular", color = "black"),
         axis.text.x = element_text(size = 40, family = "regular", color = "black"),
         axis.ticks = element_blank(),
         axis.line = element_blank(),
         #Legend
         legend.position = "top",
         legend.text = element_text(family = "regular",size=40),
         legend.background = element_rect(fill = "#EEEEEE"),
         legend.key = element_rect(fill = "#EEEEEE"),
         #Plus
         text = element_text(family = "regular", size = 45)
       )
  
ggsave("CEE.png",
       plot = n,
       device = agg_png(width = 10, height = 8, units = "in", res = 300))