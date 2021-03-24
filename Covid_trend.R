library(readr)
library(dplyr)
covid19 <- read_csv("Data/covid19.csv")
covid_df_all_states <- covid19 %>% 
  group_by(Province_State) %>% 
  filter(Province_State == "All States")
covid_df <- covid19 %>% 
   select(-Province_State)

covid_df_all_states_daily <- covid_df_all_states %>% 
  select(Date, Country_Region, active, hospitalizedCurr, 
         daily_tested, daily_positive)

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% 
  summarise(tested = sum(daily_tested),
            positive = sum(daily_positive),
            activesum = sum(active),
            hospitalized = sum(hospitalizedCurr)) %>% 
  arrange(desc(tested))

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$activesum
hospitalized_cases <- covid_top_10$hospitalized
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries
ratio <- positive_cases/tested_cases
positive_tested_top_3 <- ratio[c(7,1,5)]
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
covid_mat <- rbind(united_kingdom, united_states, turkey)
vector_col <- c("Ratio", "tested", "positive", "active", "hospitalized") 
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized") 
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)
df <- c(covid_df, covid_df_all_states, covid_df_all_states_daily,covid_top_10)
mat <- c(covid_mat)
vectors <- c(vector_col, countries)
data_structure_list <- c(df, mat, vectors)
covid_analysis_list <- c(question, answer, data_structure_list)
