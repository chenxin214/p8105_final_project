## Questions:

# First tidy the data.
# Use library(plotly) to visulizing the US obesity and overweight rate geographically (on a map), including the average amount of obesity and overweight from 2011 to 2019.
# Visulizing the obesity and overweight rate trend for all the states, and indicates the states with the most and the least changes from 2011 to 2019.
# Find and create tables for the state with the highest average obesity and overweight: 
# Visulizing the states obesity and overweight response in terms of its income (break_out_category_id == "CAT6"), education level (break_out_category_id == "CAT5"), race/ethnicity ((break_out_category_id == "CAT4"), age((break_out_category_id == "CAT3"), gender ((break_out_category_id == "CAT2").

########################################
library(tidyverse)
library(httr)
library(plotly)
library(TSA)
library(viridis)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
# also install shiny if you want to develop interactive mapping applications
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(kableExtra)
library(ggplot2)
library(maptools)

#install.packages("maptools")
#install.packages("mapdata")
#install.packages("mapproj")



## Data import:
load_df =
  GET("https://chronicdata.cdc.gov/resource/fqb7-mgjf.csv",
      query = list("$limit" = 50000)) %>% 
  content("parsed") %>% 
  janitor::clean_names()

### Variable : notation
#year: 2011 - 2019
#locationabbr: state abbreviation, 56 states
#locationdesc : state name, 56 states
#response: Normal Weight (BMI 18.5-24.9); Obese (BMI 30.0 - 99.8); Overweight (BMI 25.0-29.9); Underweight (BMI 12.0-18.4); Underweight (bmi 12.0-18.4); Normal Weight (bmi 18.5-24.9); Obese (bmi 30.0 - 99.8); Overweight (bmi 25.0-29.9)
#break_out_category: Race/Ethnicity; Age Group; Household Income; Education Attained; Gender; Overall  
#break_out: Male; College graduate; White, non-Hispanic;
#          `$35,000-$49,999`; `$25,000-$34,999`; `$15,000-$24,999`;
#           35-44; 25-34; 18-24; 55-64; 
#           (Other); Overall; Some post-H.S.

#sample_size
#data_value: crude prevalence, unit %, such as 14.7 means 14.7%
#confidence_limit_low
#confidence_limit_high
#display_order: I don't know
#geo_location
#break_out_category_id: 6 types
#breakout_id: such as SEX1, SEX2
#location_id: WTF!!! out-of-order!!!! hope it useless
#response_id: 4 types


#load_df<-read.csv("https://chronicdata.cdc.gov/resource/fqb7-mgjf.csv",
#                  header = T,
#                  nrows=50000)

# Tidy data

# total population in each state in each year
df.tot<-load_df %>%
  group_by(year,locationdesc) %>%
  summarise(state_sum=sum(sample_size))

# total 'obese' and 'overweight' amount in each state in each year
df.overweight<-
  load_df %>%
  filter(response %in% c("Obese (BMI 30.0 - 99.8)","Overweight (BMI 25.0-29.9)",
                         "Obese (bmi 30.0 - 99.8)","Overweight (bmi 25.0-29.9)")) %>%
  group_by(year,locationdesc) %>%
  summarise(state_overweight_sum=sum(sample_size))


df.rate<-left_join(df.tot, df.overweight,
                   by = c("year" , "locationdesc")) %>%
  mutate(over_rate=state_overweight_sum/state_sum)



# Use library(plotly) to visulizing the US obesity and overweight rate geographically (on a map), including the average amount of obesity and overweight from 2011 to 2019.
df.rate1<- df.rate %>%
  group_by(locationdesc)  %>%
  mutate(mean_amount=mean(state_overweight_sum))

df.rate1<-df.rate1[-which(df.rate1$locationdesc %in% c("All States and DC (median) **","All States, DC and Territories (median) **","Virgin Islands") ),]
df.rate1$region<-tolower(df.rate1$locationdesc)

states = map_data("state")
# merge the datasets
states = merge(states, df.rate1, by="region", all.x=T)


map_plot1 = ggplot(states[which(states$year=="2011"),], 
                   aes(x = long, y = lat, group = group, fill = over_rate,
                       text = paste0("State: ",locationdesc, "</br></br>Over weight rate: ", round(over_rate,3)))) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(name = "obesity and \noverweight rate", low = "#ffe8ee", high = "#c81f49", guide = "colorbar", 
                      na.value="#eeeeee") +
  labs(title="Obesity and overweight rate in US 2011") +
  coord_map()
ggplotly(map_plot1, tooltip = "text")

map_plot2 = ggplot(states[which(states$year=="2019"),], 
                   aes(x = long, y = lat, group = group, fill = over_rate,
                       text = paste0("State: ",locationdesc, "</br></br>Over weight rate: ", round(over_rate,3)))) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(name = "obesity and \noverweight rate", low = "#ffe8ee", high = "#c81f49", guide = "colorbar", 
                      na.value="#eeeeee") +
  labs(title="Obesity and overweight rate in US 2019") +
  coord_map()
ggplotly(map_plot2, tooltip = "text")

map_plot3 = ggplot(states[which(states$year=="2019"),], 
                   aes(x = long, y = lat, group = group, fill = mean_amount,
                       text = paste0("State: ",locationdesc, "</br></br>Average amount of obesity and overweight: ", round(mean_amount,3)))) + 
  geom_polygon(color = "white") +
  scale_fill_gradient(name = "Average amount of \nobesity and overweight", low = "#ffe8ee", high = "#c81f49", guide = "colorbar", 
                      na.value="#eeeeee") +
  labs(title="Average amount of obesity and overweight in US 2011-2019") +
  coord_map()
ggplotly(map_plot3, tooltip = "text")






# Visulizing the obesity and overweight rate trend for all the states, and indicates the states with the most and the least changes from 2011 to 2019.
ggplot(data=df.rate[-which(df.rate$locationdesc %in% c("All States and DC (median) **","All States, DC and Territories (median) **","Virgin Islands") ),], 
       aes(year, y=over_rate)) +
  geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc),size=1.5)+
  facet_wrap(~ locationdesc)+
  theme(legend.position = "None",
        #text = element_text(family='Kai'),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = seq(2011,2019,1))+
  xlab("Year")+ylab("Obesity and overweight rate")+
  ggtitle("Obesity and overweight rate trend for all the states")

ggplot(data=df.rate[-which(df.rate$locationdesc %in% c("All States and DC (median) **","All States, DC and Territories (median) **","Virgin Islands") ),], 
       aes(year, y=over_rate)) +
  geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc),size=1.5)+
  # facet_wrap(~ locationdesc)+
  theme(legend.position = "bottom",
        #text = element_text(family='Kai'),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = seq(2011,2019,1))+
  xlab("Year")+ylab("Obesity and overweight rate")+labs(color="")+
  ggtitle("Obesity and overweight rate trend for all the states")

df.rate2011<-df.rate %>%
  filter(year %in% c(2011)) 

df.rate2019<-df.rate %>%
  filter(year %in% c(2019)) 

df.rate20112019<-left_join(df.rate2011, df.rate2019,
                           by = c("locationdesc")) %>%
  mutate(change=over_rate.y-over_rate.x)

na.omit(df.rate20112019[-which(df.rate$locationdesc %in% c("All States and DC (median) **","All States, DC and Territories (median) **","Virgin Islands") ),]) %>% 
  filter(abs(change)==max(abs(change)))

na.omit(df.rate20112019[-which(df.rate$locationdesc %in% c("All States and DC (median) **","All States, DC and Territories (median) **","Virgin Islands") ),]) %>% 
  filter(abs(change)==min(abs(change)))



# Find and create tables for the state with the highest average obesity and overweight: 
df.highest<-df.rate %>% 
  group_by(locationdesc) %>%
  summarise(mean_overweight=mean(state_overweight_sum)) %>%
  filter(mean_overweight == max(mean_overweight))


kable(df.highest, row.names= T, escape= F) %>%
  kable_styling(bootstrap_options = "striped", full_width= F)


# Visulizing the states obesity and overweight response in terms of its 
# income (break_out_category_id == "CAT6"), 
# education level (break_out_category_id == "CAT5"), 
# race/ethnicity ((break_out_category_id == "CAT4"), 
# age((break_out_category_id == "CAT3"), 
# gender ((break_out_category_id == "CAT2").


new.load_df<-load_df[-which(load_df$locationdesc %in% c("All States and DC (median) **","All States, DC and Territories (median) **","Virgin Islands") ),]
new.load_df<-new.load_df[which(new.load_df$locationdesc=="Nebraska"),]
new.load_df$new.response<-substr(new.load_df$response, 1, regexpr("\\(", new.load_df$response)-1)


ggplot(data=new.load_df[which(new.load_df$break_out_category_id == "CAT6"),], 
       aes(x=new.response, y=breakout_id)) +
  #geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc,size=sample_size))+
  facet_wrap(~ locationdesc)+
  theme(#legend.position = "None",
    #text = element_text(family='Kai'),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Response")+ylab("Income")+
  ggtitle("Bubble plot of response v. income sample size")


ggplot(data=new.load_df[which(new.load_df$break_out_category_id == "CAT5"),], 
       aes(x=new.response, y=breakout_id)) +
  #geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc,size=sample_size))+
  facet_wrap(~ locationdesc)+
  theme(#legend.position = "None",
    #text = element_text(family='Kai'),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Response")+ylab("Education level ")+
  ggtitle("Bubble plot of response v. education level sample size")


df4<-new.load_df[which(new.load_df$break_out_category_id == "CAT4"),]
df4$breakout_id<-gsub("[0]","",df4$breakout_id)

ggplot(data=df4, 
       aes(x=new.response, y=breakout_id)) +
  #geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc,size=sample_size))+
  facet_wrap(~ locationdesc)+
  theme(#legend.position = "None",
    #text = element_text(family='Kai'),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Response")+ylab("Race/ethnicity ")+
  ggtitle("Bubble plot of response v. race/ethnicity sample size")



ggplot(data=new.load_df[which(new.load_df$break_out_category_id == "CAT3"),], 
       aes(x=new.response, y=breakout_id)) +
  #geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc,size=sample_size))+
  facet_wrap(~ locationdesc)+
  theme(#legend.position = "None",
    #text = element_text(family='Kai'),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Response")+ylab("Age")+
  ggtitle("Bubble plot of response v. age sample size")


ggplot(data=new.load_df[which(new.load_df$break_out_category_id == "CAT2"),], 
       aes(x=new.response, y=breakout_id)) +
  #geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc,size=sample_size))+
  facet_wrap(~ locationdesc)+
  theme(#legend.position = "None",
    #text = element_text(family='Kai'),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Response")+ylab("Gender")+
  ggtitle("Bubble plot of response v. gender sample size")


########################
tidy_df = 
  load_df %>% 
  filter(is.na(data_value_footnote)) %>% 
  dplyr::select(-class, -topic, -question, -data_value_unit, -data_value_type, -data_source, -data_value_footnote_symbol, -data_value_footnote, -question_id, -class_id, -topic_id, -states, -counties)

income_df = 
  tidy_df %>% 
  filter(break_out_category_id == "CAT6") %>%
  dplyr::select(year, locationabbr, locationdesc, response, break_out, sample_size, data_value, confidence_limit_low, confidence_limit_high, response_id) %>% 
  group_by(break_out, response) %>%
  mutate(
    sample_sum  = sum(sample_size),
    average_value = weighted.mean(data_value, sample_size)
  ) 

a= income_df %>% filter(locationdesc == "Nebraska") 
ggplot(data=a, aes(x=response, y=break_out)) +
  #geom_line(aes(color=locationdesc))+
  geom_point(aes(color=locationdesc,size=sample_size))+
  facet_wrap(~ locationdesc)+
  theme(#legend.position = "None",
    #text = element_text(family='Kai'),
    axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Response")+ylab("Income")+
  ggtitle("Bubble plot of response v. income sample size")

###############################################
# output tidy_df for regression
write_csv(tidy_df, "./data/tidy_df")
