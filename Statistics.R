library(readr)
wnba <- read_csv("Data/wnba.csv")
library(dplyr)
# wnba <- readr("~/R_files/DataQueset_learning/Statistics/Data/wnba.csv")
set.seed(1)
wnba <- wnba %>%
  mutate(pts_game=PTS/Games_Played)
total_points_estimates<-wnba%>%
  group_by(Pos) %>%
  sample_n(10) %>%
  summarise(mean_pts_game=mean(pts_game), mean_pts_season=mean(PTS))%>%
  arrange(Pos)
total_points_parameter<-wnba%>%
  group_by(Pos) %>%
  summarise(mean_pts_game=mean(pts_game), mean_pts_season=mean(PTS))%>%
  arrange(Pos)

wnba %>% 
  mutate(games_stratum = cut(Games_Played, breaks = 3)) %>%
  group_by(games_stratum) %>% 
  summarize(n = n()) %>% 
  mutate(percentage = n / sum(n) * 100) %>% 
  arrange(desc(percentage))

set.seed(1)
under_12<- wnba%>%
  filter(Games_Played<=12)%>%
  sample_n(1)
btw_13_22<- wnba%>%
  filter(Games_Played>12 & Games_Played<=22)%>%
  sample_n(2)
over_22<- wnba%>%
  filter(Games_Played>22)%>%
  sample_n(7)
combined <-bind_rows(under_12, btw_13_22, over_22)
mean(combined$PTS)

## for identifying class of each column
library(purrr)
list_class <- map(wnba, class)

print(list_class)
df_class <- map_df(wnba, class)
#taking games played into account-generate a function and iterate it 100 times
library(purrr)
library(tibble)
library(ggplot2)
set.seed(1)
sample_mean<-function(x){
  under_12 <- wnba %>% 
    filter(Games_Played <= 12) %>% 
    sample_n(1)
  btw_13_22 <- wnba %>% 
    filter(Games_Played > 12 & Games_Played <= 22) %>% 
    sample_n(2)
  over_22 <- wnba %>% 
    filter(Games_Played > 22) %>% 
    sample_n(7)
  
  combined <- bind_rows(under_12, btw_13_22, over_22)
  mean(combined$PTS)
}
sample_number<- 1:100
mean_points_season<- map_dbl(sample_number, sample_mean)
df<-tibble(sample_number, mean_points_season)
ggplot(data=df)+
  aes(x=sample_number, y=mean_points_season) +
  geom_point()+
  geom_hline(yintercept=mean(wnba$PTS) ,color="blue")+
  ylim(80,320)
#
wnba <- wnba %>%
  mutate(minutes_stratum = cut(MIN, breaks = 3))
set.seed(1)

sample_mean <- function(x) {
  sample <- wnba %>% 
    group_by(minutes_stratum) %>% 
    sample_frac(.07)
  
  mean(sample$PTS)
}

sample_number <- 1:100

mean_points_season <- map_dbl(sample_number, sample_mean)

df <- tibble(sample_number, mean_points_season)
ggplot(data = df) + 
  aes(x = sample_number, y = mean_points_season) +
  geom_point() +
  geom_hline(yintercept = mean(wnba$PTS), color = "blue") +
  ylim(80, 320)
library(ggplot2)

set.seed(10)
clusters <- unique(wnba$Team)%>%
  sample(size=4)
sample<-wnba%>%
  filter(Team %in% clusters)
sampling_error_height <- mean(wnba$Height) - mean(sample$Height)
sampling_error_age <- mean(wnba$Age) - mean(sample$Age)
sampling_error_games <- mean(wnba$Games_Played) - mean(sample$Games_Played)
sampling_error_points <- mean(wnba$PTS) - mean(sample$PTS)
library(tidyr)
glimpse(wnba)
str(wnba)

wnba%>%
  group_by(Height) %>%
  summarise(Freq=n())%>%
  arrange(desc(Height))
wnba%>%
  group_by(Height) %>%
  summarise(Freq=n())%>%
  arrange(desc(Freq))
age_descending <- wnba%>%
  group_by(Age) %>%
  summarise(Freq=n())%>%
  arrange(desc(Age))
age_ascending <- wnba %>%
  group_by(Age) %>%
  summarize(Freq = n())

#stratifying by height
wnba <- wnba%>%
  mutate(height_labels = case_when(
    Height<=170 ~"short",
    Height>170 & Height<=180 ~"medium",
    Height>180 ~"tall")
    ) 

wnba%>%
  select(Height, height_labels)%>%
  head(10)
wnba%>%
  group_by(height_labels)%>%
  summarise(freq=n())%>%
  arrange(desc(height_labels))
#ordinal or define as a factor
height_levels <- c("short", "medium", "tall")

wnba %>% 
  group_by(Height_labels) %>% 
  summarize(Freq = n()) %>% 
  arrange(factor(Height_labels, 
                 levels = height_levels))
age_25 <- wnba%>%
  filter(Age==25)%>%
  summarize(Freq=n())%>%
  mutate(Prop = Freq/nrow(wnba))%>%
  mutate(Percentage = Freq/nrow(wnba)*100)
age_23_or_under <- wnba%>%
  filter(Age<=23)%>%
  summarize(Freq=n())%>%
  mutate(Prop = Freq/nrow(wnba))%>%
  mutate(Percentage = Freq/nrow(wnba)*100)
age_30_or_older <- wnba%>%
  filter(Age>=30)%>%
  summarize(Freq=n())%>%
  mutate(Prop = Freq/nrow(wnba))%>%
  mutate(Percentage = Freq/nrow(wnba)*100)
wnba %>% 
  mutate(cume_dist_age = cume_dist(Age)) %>%
  select(Name, Age, cume_dist_age) %>%
  head (n=15)
age_upper_quartile <- quantile(wnba$Age, probs=0.75)
wnba_age_percentiles<-wnba %>%
  mutate(cume_dist_age = cume_dist(Age))%>%
  select(Name, Age, cume_dist_age)%>%
  arrange(Age)
#make categories and count frequency
library(tidyr)
wnba%>%
  mutate(weight_categories = 
           cut(Weight, breaks = 10, dig.lab = 4))%>%
  group_by(weight_categories)%>%
  summarise(Freq = n())%>%
  drop_na()

#The ( character indicates that the starting point is not included, while the ] the endpoint is included.
wnba%>%
  mutate(weight_categories = 
           cut(Weight, breaks = 10, dig.lab = 4))%>%
  group_by(weight_categories)%>%
  summarise(Freq = n())%>%
  drop_na()


wnba <- wnba %>% 
  mutate(points_categories = cut(PTS, breaks = 10, dig.lab = 4))

pts_freq_table <- wnba %>% 
  group_by(PTS) %>% 
  summarize(Freq = n())

pts_grouped_freq_table <- wnba %>% 
  group_by(points_categories) %>% 
  summarize(Freq = n()) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100) %>% 
  arrange(desc(points_categories))

wnba <- wnba %>% 
  mutate(min_categories = cut(MIN, breaks = 20, dig.lab = 4))

wnba %>% 
  group_by(min_categories) %>% 
  summarize(Freq = n())
wnba <- wnba %>% 
  mutate(min_categories = 
           cut(MIN, 
               breaks = c(0, 150, 300, 450, 600, 750, 900, 1050), 
               dig.lab = 4))

min_grouped_freq_table <- wnba %>% 
  group_by(min_categories) %>% 
  summarize(Freq = n()) %>% 
  mutate(Percentage = Freq / nrow(wnba) * 100)
