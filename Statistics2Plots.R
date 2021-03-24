library(readr)
wnba <- read_csv("Data/wnba.csv")
library(dplyr)
library(ggplot2)
set.seed(1)
under_12 <- wnba %>% 
  filter(Games_Played<=12) %>% 
  sample_n(1)

btw_13_22 <- wnba %>% 
  filter(Games_Played > 12 & Games_Played<=22) %>% 
  sample_n(2)

over_22 <- wnba %>% 
  filter(Games_Played > 22) %>% 
  sample_n(7)

combined <- bind_rows(under_12, btw_13_22, over_22)
mean(combined$PTS)









ggplot(data=wnba, aes(x=Exp_ordinal, fill = Exp_ordinal))+
  geom_bar()+
  coord_flip()
  theme(legend.position ="none")
  
  #proportion (use ..)
  ggplot(data = wnba,
         aes(x = Pos, 
             y = ..prop.., 
             group = 1)) +
    geom_bar() + 
    theme(legend.position = "none")+
    labs(x = "Position",
         y = "Percentage")
  
  ## stacked bar graph with proportions##   
  library(stringr)
  
  pos_prop <- wnba %>% 
    group_by(Pos) %>% 
    summarize(Prop = n() / nrow(wnba))
  
  ggplot(data = pos_prop, 
         aes(x = "", y = Prop, fill = Pos)) + 
    geom_bar(stat = "identity", width = 0.25) +
    coord_flip() +
    geom_text(aes(label = str_c(round(Prop * 100), "%")), 
              position = position_stack(vjust = 0.5)) + 
    labs(x = NULL, 
         y = NULL, 
         fill = NULL, 
         title = "Player Distribution by Position") + 
    theme_classic() + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  pos_prop <- wnba %>% 
    group_by(Pos) %>% 
    summarize(Prop = n() / nrow(wnba))
  
  ggplot(data = pos_prop, 
         aes(x = "", 
             y = Prop, 
             fill = Pos)) + 
    geom_bar(stat = "identity") +
    coord_polar(theta = "y")  
 ## stacked bar graph with proportions## 
  exp_prop <- 
    wnba %>% 
    group_by(Exp_ordinal) %>% 
    summarize(Prop = n() / nrow(wnba))
  ggplot(data = exp_prop, 
         aes(x = "", y = Prop, fill = Exp_ordinal)) + 
    geom_bar(stat = "identity", width = 0.25) +
    coord_polar(theta = "y") +
    geom_text(aes(label = str_c(round(Prop * 100), "%")), 
              position = position_stack(vjust = 0.5)) + 
    labs(x = NULL, 
         y = NULL, 
         fill = NULL, 
         title = "Player Distribution by Experience Level") + 
    theme_classic() + 
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())  

#histograms 
pts_binwidth <- (max(wnba$PTS) - min(wnba$PTS)) / 10
  
ggplot(data = wnba, 
         aes(x = PTS)) +
    geom_histogram(boundary = min(wnba$PTS), 
                   binwidth = pts_binwidth) +
    scale_x_continuous(breaks = seq(0, 600, by = 50))

wnba <- wnba %>% 
  mutate(games_categories = cut(Games_Played, breaks = 10, dig.lab = 4))

wnba %>% 
  group_by(games_categories) %>% 
  summarize(Freq = n())
games_binwidth <- (max(wnba$Games_Played) - min(wnba$Games_Played)) / 10
ggplot(data = wnba, 
       aes(x = Games_Played)) +
  geom_histogram(boundary = min(wnba$Games_Played), 
                 binwidth = games_binwidth) +
  scale_x_continuous(breaks = seq(0, 35, by = 5))

ggplot(data=wnba, aes(x=AST))+
  geom_histogram()

ggplot(data = wnba, 
       aes(x = BMI)) +
  geom_histogram(bins = 15)

##  grouped frequency distribution table
wnba <- wnba %>% 
  mutate(points_categories = cut(PTS, breaks = 10, dig.lab = 4))
wnba %>% 
  group_by(points_categories) %>% 
  summarize(Freq = n())
##specify two arguments to geom_histogram(): (1) boundary and (2) binwidth
pts_binwidth <- (max(wnba$PTS) - min(wnba$PTS)) / 10

ggplot(data = wnba, 
       aes(x = PTS)) +
  geom_histogram(boundary = min(wnba$PTS), 
                 binwidth = pts_binwidth)
