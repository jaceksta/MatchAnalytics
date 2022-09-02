library(tidyverse)
library(worldfootballR)
library(ggrepel)


FCBBMG <- fotmob_get_match_details("3903571")

shots <- FCBBMG[[15]][[1]]

colnames(shots)

YannSommer <- shots%>%
  filter(event_type == "AttemptSaved" & team_id == 9823 & is_blocked == F)


ggplot(data = YannSommer, aes(x=on_goal_shot$x, y=on_goal_shot$y, label = player_name))+
  geom_point(aes(size = expected_goals_on_target), alpha = 1/3, color = "#a00028")+
  geom_text_repel()+
  geom_segment(aes(x = 1.75, y = 0, xend=1.75, yend=0.5), size = 2)+
  geom_segment(aes(x = 0.25, y = 0, xend=0.25, yend=0.5), size = 2)+
  geom_segment(aes(x = 0.25, y = 0.5, xend=1.75, yend=0.5), size = 2)+
  xlim(0.25, 1.75)+
  ylim(0,0.5)+
  xlab("")+
  ylab("")+
  theme(panel.background = element_rect(fill = "#F7F7F7"), plot.background = element_rect(fill = "#F7F7F7"))+
  theme(legend.position = "none")+
  labs(
    title = "Wszystkie obronione strzały Yanna Sommera w meczu z Bayernem",
    subtitle = "źródło: fotmob.com",
    caption = "Autor: Jacek Staszak"
  )+
  theme(plot.title = element_text(face = "bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(panel.border = element_blank())+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


library(ggsoccer)




FCBBMG_US <- understat_match_shots("https://understat.com/match/19375")

FCBBMG_US <- FCBBMG_US[1:33,]

YS_US <- FCBBMG_US%>%
  filter(result == "SavedShot")


ggplot(YS_US) +
  annotate_pitch(colour = "white",
                 fill   = "#486F38",
                 limits = FALSE) +
  geom_point(aes(x = 100*X, y = 100 - (Y*100), size = xG),
             fill = "#a00028", 
             shape = 21) +
  theme_pitch() +
  theme(panel.background = element_rect(fill = "#486F38")) +
  coord_flip(xlim = c(49, 101),
             ylim = c(-12, 112)) +
  ggtitle("Skąd strzelali piłkarze Bayernu",
          "strzały obronione przez Yanna Sommera")+
  theme(legend.position = "none")+
  labs(caption = "Źródło: understat.com, Autor: Jacek Staszak")
