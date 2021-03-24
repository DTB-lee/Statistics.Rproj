library(readr)
library(dplyr)
book_review <- read_csv(file = "Data/book_reviews.csv", col_names = TRUE)
dim(book_review)
colnames(book_review)
glimpse(book_review)
book_review1 <- book_review %>% 
  filter(!is.na(state))
book_review1 <- book_review %>% 
  filter(!is.na(review))
summary(book_review1)
unique(book_review1$state)

book_review1 <- book_review1 %>% 
  mutate(abstate = if_else(state == "New York", "NY", state)) %>% 
  mutate(abstate=if_else(state == "Florida", "FL", abstate)) %>% 
  mutate(abstate=if_else(state == "Texas", "TX", abstate)) %>% 
  mutate(abstate=if_else(state == "California", "CA", abstate))

unique(book_review1$abstate)
unique(book_review1$review)
book_review1 <- book_review1 %>% 
  mutate(review_num =case_when(review=="Poor" ~ 1,
                   review=="Fair" ~ 2,
                   review=="Good" ~ 3,
                   review=="Great" ~ 4,
                   review=="Excellent" ~ 5))   
book_review1 <- book_review1 %>% 
  mutate(is_high_review = if_else(review_num>=4, TRUE, FALSE))

Book_total <- book_review1 %>% 
  group_by(book) %>% 
  summarise(No_sold=n())

unique(book_review1$price)

book_price <- book_review1 %>% 
  group_by(book, price) %>% 
  summarise(price_book=n())

Total_perbook <- book_price %>% 
  mutate(total=price*price_book) %>% 
  arrange(desc(total))
