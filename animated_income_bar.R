" The code came from https://github.com/amrrs/animated_bar_charts_in_R 
but I made a ton of changes to fit it to my needs for this project "

rm(list=ls())

library(tidyverse)
library(gganimate)

income_df <- read_csv("data/Incomeproject1.csv")

#One state said KS, turning to KA (assuming its Kansas)
income_df$State[income_df$State=="KA"] <- "KS"

#Getting a column of State Names
income_df$State_Name <- income_df$State
income_df$State_Name <- state.name[match(income_df$State_Name, state.abb)]

# This function made NAs because DC is in dataset
income_df$State_Name[is.na(income_df$State_Name)] <- "Washington DC"


income_df <- income_df %>%
  group_by(Year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Income),
         Value_rel = Income/Income[rank==1],
         Value_lbl = paste0(" ",round(Income/1e9))) %>%
  group_by(State_Name) %>% 
  filter(rank <=10) %>%
  ungroup()

# Animation
anim <- ggplot(income_df, aes(rank, group = State_Name, 
                fill = as.factor(State_Name), color = as.factor(State_Name))) +
  geom_tile(aes(y = Income/2,
                height = Income,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(State_Name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Income,label = Income, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
         axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
       plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(Year, transition_length = 8, state_length = 4) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Income by State Per Year : {closest_state}',  
       subtitle  =  "Top 10 States",
       caption  = "Income in USD | Data Source: Incomeproject.csv") 


# For MP4
animate(anim, 800, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4
anim_save("Income_Video.mp4", animation = for_mp4 )

