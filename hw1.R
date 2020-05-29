library(tidyverse)
library(readr)

watch.table <- read_csv("watch_table.csv")
user.table <- read_csv("user_table.csv")
drama.table <- read_csv("drama_table.csv")
#1
full.table <- watch.table %>% left_join(user.table, by = "user_id") %>% left_join(drama.table , by = "drama_id" )
#2
full.table %>% group_by(gender) %>% select( gender) %>% summarize(nmuber = n() )
#3 analysis
full.table %>% group_by(user_name , age ,location) %>% 
  filter(device == "Android") %>% 
  summarize(male = table(unique(gender))[1],
            female = table(unique(gender))[2])
#4 analysis
full.table %>% 
  filter(location == "Taipei" & gender == "male" ) %>% 
  group_by(user_name, device , age ) %>%  summarize(gender = unique(gender))

#second
air_bnb <- read_csv("AB_NYC_2019.csv")

#1
air_bnb %>% filter( neighbourhood_group == "Manhattan" ) %>% 
  ggplot( air_bnb , mapping = aes(x = latitude , y = longitude)) + geom_point(size = 0.1, color = "#56B4E9")+
  theme(axis.text.y = element_text(size = 10 , vjust = 0.5, hjust = 0.5))+
  theme(axis.line = element_line(size=0.2, colour = "black"))
#2
air_bnb %>% 
  filter(neighbourhood_group == "Manhattan"  & number_of_reviews >=400) %>%
  group_by(neighbourhood) %>%
  summarise(sum_reviews = sum(number_of_reviews)) %>%
ggplot(air_bnb, mapping = aes(x = neighbourhood , y = sum_reviews)) + geom_bar(stat="identity" , fill = "#56B4E9" , alpha = 0.7) + 
  geom_text(stat="identity",aes(label=sum_reviews),vjust=2, color=I("#000000"),size=3)+
  theme(axis.text.x = element_text(size = 10 , vjust = 0.5, hjust = 0.5 , angle = 30))+
  theme(axis.text.y = element_text(size = 10 , vjust = 0.5, hjust = 0.5))+
  theme(axis.line = element_line(size=0.2, colour = "black"))

#3
air_bnb %>% 
  filter(neighbourhood_group == "Manhattan"  & number_of_reviews >=400) %>%
  group_by(neighbourhood) %>%
  summarise(sum_reviews = sum(number_of_reviews)) %>%
  select(neighbourhood,sum_reviews) %>% 
  filter(rank(desc(sum_reviews)) == 1)

#4
Harlem <- air_bnb %>% 
  filter(neighbourhood == "Harlem")
table(is.na(Harlem))
Harlem <- na.omit(Harlem)
table(is.na(Harlem))

ggplot(Harlem , mapping = aes(x = number_of_reviews , fill = room_type)) + geom_density(alpha = 0.7)+
  theme(axis.text.x = element_text(size = 10 , vjust = 0.5, hjust = 0.5 , angle = 30))+
  theme(axis.text.y = element_text(size = 10 , vjust = 0.5, hjust = 0.5))+
  theme(axis.line = element_line(size=0.2, colour = "black"))

ggplot(Harlem , aes(x = number_of_reviews, y = price, color = room_type, shape = room_type)) +
  geom_point(size=1.5)

