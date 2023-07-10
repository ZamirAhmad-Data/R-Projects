
# Project - Airbnb 2021
# Introduction: 

# Introduction:
#   
#The objective of this project is to analyze Airbnb listings across ten different cities, namely Barcelona, Amsterdam, 
#Budapest, London, Vienna, Lisbon, Rome, Berlin, Athens, and Paris. 
#The analysis aims to identify the factors that affect the prices of Airbnb listings.
# Another goal of the project was to examine the differences in Airbnb listings across all cities, 
#focusing on various variables such as average listing price, average number of bedrooms, 
# and whether the host was a business or an individual and other variables. 

# In the first part of the analysis, I compared various metrics across the ten cities, 
#including the number of listings, median and average prices, price variability, average number of bedrooms, 
#person capacity, guest satisfaction, and cleanliness rating. 
#I also analyzed the proportion of listings offering shared rooms, private rooms, and entire apartments/houses, 
#as well as the proportion of listings offered by businesses, hosts with multiple offerings, 
#and hosts with only one offering. Additionally, I calculated descriptive statistics for location variables, 
#such as distance from listings to the city center and metro stations. 
#The dataset also includes an attraction index and a restaurant index, 
#which measures the number of reviews on Google for nearby attractions and restaurants, 
#respectively, and divides them by the distance between the listings and the attraction/restaurant. 
#The higher the index, the more popular the attraction/restaurant. 
#Based on the assumption that listings closer to popular attractions/restaurants are more expensive, 
#I compared the attraction/restaurant indices across the ten cities. 
#Note that only attractions and restaurants with more than ten reviews are included in the dataset.

# In part two of the analysis, I examined the relationship between listing prices 
#and other variables in the dataset to identify any factors that could 
# predict listing prices. I also built a multiple regression model to determine 
#how well the selected variables could explain 
# the variation in listing prices, which is the dependent variable.
# 
# Finally, in part three, I compared the average, median, and standard deviation 
#of listing prices across all ten cities on weekdays and weekends. 
#This analysis aimed to identify whether listing prices differed between weekdays 
#and weekends.

# Overall, the analysis provides valuable insights into the factors that affect Airbnb listing prices 
# in various cities and can be used to inform pricing strategies for Airbnb hosts. 





  
# The meaning of the columns:
# 
# For each city two files are provided: data for weekday and weekend offers
# 
# The columns are as following:
#   
# realSum: the full price of accommodation for two people and two nights in EUR
# room_type: the type of the accommodation 
# room_shared: dummy variable for shared rooms
# room_private: dummy variable for private rooms
# person_capacity: the maximum number of guests 
# host_is_superhost: dummy variable for superhost status
# multi: dummy variable if the listing belongs to hosts with 2-4 offers
# biz: dummy variable if the listing belongs to hosts with more than 4 offers
# cleanliness_rating: cleanliness rating
# guest_satisfaction_overall: overall rating of the listing
# bedrooms: number of bedrooms (0 for studios)
# dist: distance from city centre in km
# metro_dist: distance from nearest metro station in km
# attr_index: attraction index of the listing location
# attr_index_norm: normalised attraction index (0-100)
# rest_index: restaurant index of the listing location
# rest_index_norm: normalised restaurant index (0-100)
# lng: longitude of the listing location
# lat: latitude of the listing location
# 
# Other important Notes: 
#   
# The offers were collected four to six weeks in advance of the travel dates, 
# and the collected prices refer to the full amount due for the accommodation, 
# including the reservation fee and cleaning fee.
# 
# The next variable related to professionalisation is the superhost status that is 
# granted to hosts providing high-quality service.

#Source:https://zenodo.org/record/4446043#.ZCdKRezMJhF


#Part I 


# Dataset = Weekdays

packages <- c("DescTools","dplyr","ggplot2","readr","stringr","tidyr","visdat")
lapply(packages,require,character.only=T)

amsterdam = read.csv('amsterdam_weekdays.csv')
amsterdam$City = rep('Amsterdam',nrow(amsterdam))
amsterdam = amsterdam[, c('City','realSum',names(amsterdam)[-c(1,2)])]
amsterdam$City.1 = NULL



athens = read.csv('athens_weekdays.csv')
athens$City = rep('Athens',nrow(athens))
athens = athens[, c('City','realSum',names(athens)[-c(1,2)])]
athens$City.1 = NULL


barcelona = read.csv('athens_weekdays.csv')
barcelona$City = rep('Barcelona',nrow(barcelona))
barcelona = barcelona[, c('City','realSum',names(barcelona)[-c(1,2)])]
barcelona$City.1 = NULL


berlin = read.csv('berlin_weekdays.csv')
berlin$City = rep('Berlin',nrow(berlin))
berlin = berlin[, c('City','realSum',names(berlin)[-c(1,2)])]
berlin$City.1 = NULL

budapest = read.csv('budapest_weekdays.csv')
budapest$City = rep('Budapest',nrow(budapest))
budapest = budapest[, c('City','realSum',names(budapest)[-c(1,2)])]
budapest$City.1 = NULL



lisbon = read.csv('lisbon_weekdays.csv')
lisbon$City = rep('Lisbon',nrow(lisbon))
lisbon = lisbon[, c('City','realSum',names(lisbon)[-c(1,2)])]
lisbon$City.1 = NULL


london = read.csv('london_weekdays.csv')
london$City = rep('London',nrow(london))
london = london[, c('City','realSum',names(london)[-c(1,2)])]
london$City.1 = NULL



paris = read.csv('paris_weekdays.csv')
paris$City = rep('Paris',nrow(paris))
paris = paris[, c('City','realSum',names(paris)[-c(1,2)])]
paris$City.1 = NULL


rome = read.csv('rome_weekdays.csv')
rome$City = rep('Rome',nrow(rome))
rome = rome[, c('City','realSum',names(rome)[-c(1,2)])]
rome$City.1 = NULL


vienna = read.csv('vienna_weekdays.csv')
vienna$City = rep('Vienna',nrow(vienna ))
vienna  = vienna [, c('City','realSum',names(vienna)[-c(1,2)])]
vienna $City.1 = NULL


data = rbind(amsterdam,athens,barcelona,berlin,budapest,lisbon,london,paris,rome,vienna)

theme_s = theme( text = element_text(family='serif',size=14),
                 rect = element_blank(),
                 panel.grid = element_blank(),
                 title = element_text(color='#8b0000'),
                 axis.line = element_line(color='black'))


data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum))

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum)) %>% 
ggplot(aes(y=reorder(City,-n),x=n)) + 
  geom_bar(stat='identity') + 
  theme_s + 
  labs(title='Number of Listings per City',
       y= '',
       x='Count')


#According to the graph, London has the highest number of listings among the ten cities, with 4614 listings. 
#Rome comes second with 4492 listings, followed by Paris with 3130 listings.


data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum))

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum)) %>% 
  ggplot(aes(y=reorder(City,-median_listing_price),x=median_listing_price)) + 
           geom_bar(stat='identity') + 
  theme_s + 
  labs(title = 'Comparison of Median Listing Prices Across Cities' ,
       y='',
       x='Listing Price in EUR')


# Based on the data, Amsterdam has the highest median listing price of 430 Euros for a two-person, two-night stay. Paris comes in second with a median price of 319 Euros, 
# while London is the third most expensive city in the dataset with a median listing price of 256 Euros.

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum))

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum)) %>% 
  ggplot(aes(y=reorder(City,-sd),x=sd)) + 
  geom_bar(stat='identity') + 
  theme_s + 
  labs(title = "Range of Variability in Listing Prices by City",
       y='',
       x='Listing Price in EUR')

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum)) %>% arrange(desc(sd))

#According to the data, London has the highest variability in listing prices, 
#with a spread of 508 Euros. This indicates that there is a significant range of 
#prices among the listings in London. Vienna ranks second with a spread of 455 Euros, 
#while Amsterdam closely follows with a spread of 417 Euros.

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum),
                                      avg_price = mean(realSum)) %>% arrange(desc(avg_price))

data %>% group_by(City) %>% summarize(n= n(), median_listing_price = median(realSum), sd = sd(realSum),
avg_price = mean(realSum)) %>% arrange(desc(avg_price)) %>% 

  ggplot(aes(y=reorder(City,-avg_price),x=avg_price)) + 
  geom_bar(stat='identity')  + 
  theme_s + 
  labs(title = "Distribution of Average Listing Prices Across Cities (in EUR)",
       y='',
       x='Listing Price in Euro')


# Amsterdam had the highest average listing price by city at 545 Euro's. Pairs ranked second at 398 Euro's 
# and London ranks in third place at 360 Euro's. Amsterdam is the most expensive city 
# to book an airbnb.On the other side, Budapest, Athens and Barcelona are more affordable for 
# people with a budget. 



ggplot(data,aes(y=City,x=realSum)) + 
  geom_boxplot(alpha=0.3,coef=1.5) + 
  theme_s + 
  labs(title = 'Distribution listing prices by City',
       y='',
       x='Price in Euro')
  


ggplot(data,aes(y=City,x=realSum)) + 
  geom_boxplot(alpha=0.3,coef=1.5) + 
  theme_s + 
  labs(title = "Variability in Listing Prices by City: A Boxplot Analysis",
       y='',
       x='Price in Euro') + 
  xlim(0,4000)


data %>% group_by(City) %>% 
  summarize( min = min(realSum),
             q1 = quantile(realSum,probs=0.25),
             median = median(realSum),
             q3 = quantile(realSum,probs=0.75),
             max = max(realSum),
             n = n(),
             iqr = IQR(realSum),
             mean= mean(realSum)) %>% arrange(desc(iqr))
  

#The boxplot visualizes the distribution of listing prices across different cities. 
#To make the boxes more visible, the x-axis is limited to a range of 0 to 4000, 
#but all cities still have a considerable number of outliers. 
#A whisker of 1.5, which is an industry standard, is used to identify outliers.

#Among all cities, Amsterdam has the highest interquartile range (IQR), 
#indicating the largest spread in the 50% middle values. 
#London and Paris follow with IQRs of 268 and 223 Euros, respectively. 
#This suggests that there is some variation in the middle values of listing prices across cities.

#Some listings are worth over 18,000 Euros, indicating a few extremely expensive options. 
#The graph shows that Paris and London have the highest numbers of outliers or 
#listings that are much more expensive than the average listing in the city. 
#Notably, all outliers are on the upper end of the distribution, 
#and there are no listings that are much less costly than the average.




data %>% group_by(City) %>% summarize(guest_satis = mean(guest_satisfaction_overall),
clean_rate = mean(cleanliness_rating)) %>%
  ggplot(aes(x=reorder(City,-guest_satis),y=guest_satis)) + 
  geom_bar(stat='identity') +
  ylim(0,96) + 
  theme_s + 
  labs(title='Average Guest Satisfaction Rating by City',
       y='Guest Satisfaction Rating',
       x='') +
  theme(axis.text.x= element_text(angle=90))






data %>% group_by(City) %>% summarize(guest_satis = mean(guest_satisfaction_overall),
clean_rate = mean(cleanliness_rating)) %>% 
  ggplot(aes(x=reorder(City,-clean_rate),y=clean_rate)) + 
  geom_bar(stat='identity') +
  theme_s + 
  labs(title="Comparison of Cleanliness Ratings Across Cities",
       y='Cleanliness Rating',
       x='') + 
  theme(axis.text.x = element_text(angle=90))
 

data %>% group_by(City) %>% summarize(guest_satis = mean(guest_satisfaction_overall),
clean_rate = mean(cleanliness_rating)) %>%  arrange(desc(clean_rate)) 

#Notes: Among all cities, Athens and Barcelona rank first in terms of overall guest satisfaction rate 
# and cleanliness rate, with rates of 91.1 and 9.64, respectively. 
#On the other hand, London has the lowest guest satisfaction rate and the lowest cleanliness rate.


data$room_shared = ifelse(data$room_shared == 'True',1, 
                          ifelse(data$room_shared=='False',0,data$room_shared))
data$room_shared = as.integer(data$room_shared)

data$room_private = ifelse(data$room_private=='True',1, 
                           ifelse(data$room_private=='False',0,data$room_private))
data$room_private = as.integer(data$room_private)


data$home_entire = ifelse(data$room_type=='Private room' | data$room_type =='Shared room',0,
                           ifelse(data$room_type=='Entire home/apt',1,data$room_type))

data$home_entire = as.integer(data$home_entire)

prop = data %>% group_by(City) %>% summarize(prop_shared = sum(room_shared)/n()*100,
                                      prop_private = sum(room_private)/n()*100,
                                      prop_home = sum(home_entire)/n()*100)

data %>% group_by(City) %>% summarize(prop_shared = sum(room_shared)/n()*100,
                                      prop_private = sum(room_private)/n()*100,
                                      prop_home = sum(home_entire)/n()*100) %>%
  
  
  
  ggplot(aes(x=reorder(City,-prop_shared),y=prop_shared)) +
        geom_bar(stat='identity') + 
  labs(title = "Proportion of Shared Rooms as a Percentage of Listings by City",
       y= 'Proportion Room Shared in %',
       x='City') + 
  theme_s + 
  theme(axis.text.x= element_text(angle=90))
#The graph depicts the percentage of listings that provide shared rooms, 
#and it reveals that Berlin has the highest proportion of shared rooms (2.73%) among all the cities. 
#It is noteworthy that only a small fraction of the listings offer shared rooms.


prop

ggplot(prop,aes(x=reorder(City,-prop_private),y=prop_private)) +
  geom_bar(stat='identity') + 
  labs(title = 'Proportion of Private Rooms as a Percentage of Listings by City',
       y= 'Proportion Room private in %',
       x='') + 
  theme_s + 
  theme(axis.text.x= element_text(angle=90))


#Notes: This graph presents the distribution of listings that provide private rooms in different cities. 
#The data indicates that Berlin has the highest proportion of listings that offer private rooms, 
#accounting for 60.4% of all listings, followed by London with 56.9%.
#The majority of listings (meaning more than 50% of the listings) 
#in Amsterdam,Berlin and London offer Private rooms.





ggplot(prop,aes(x=reorder(City,-prop_home),y=prop_home)) +
  geom_bar(stat='identity') + 
  labs(title = ' Proportion of Entire Homes as a Percentage of Listings by City',
       y= 'Proportion Entire Home in %',
       x='') + 
  theme_s + 
  theme(axis.text.x= element_text(angle=90))

# These notes provide an overview of the types of listings offered in different cities. 
# The majority of listings in Barcelona, Athens, Budapest, Vienna, Paris, Lisbon, and Rome offer entire homes. 
# On the other hand, Berlin, London, and Amsterdam have the lowest share of listings that offer entire homes. 
# This information can be useful for travelers who have a specific preference for the type of accommodation 
# they are looking for.

install.packages("kableExtra")
library(kableExtra)
installed.packages('knitr')
library(knitr)

kable(prop, format = 'html',
      caption= 'Descriptive statistics of dummy explanatory variables (share of listings (%))') %>%
  kable_styling(bootstrap_options='striped',full_width=T)







professional=data %>% group_by(City) %>% summarize(prop_multi = sum(multi)/n()*100,
                                      prop_biz = sum(biz)/n()*100,
                                      prop_single= 100-prop_multi-prop_biz)


professional=professional[,c('City','prop_single','prop_multi','prop_biz')]

professional

ggplot(professional,aes(x=reorder(City,-prop_single),y=prop_single)) + 
         geom_bar(stat='identity') + 
  labs(title = 'Hosts with One Listing as a Percentage of Total Listings by City ',
       y='Share of hosts with one listing in %',
       x='') + 
  theme_s + 
  theme(axis.text.x= element_text(angle=90))


#The graph shows the distribution of listings based on the number of listings offered by a host. 
#It is evident that the majority of listings in Amsterdam, Berlin, and Paris are offered 
#by hosts with only one listing, with Amsterdam having the highest proportion at 57.7%. 
#Berlin follows closely with 54%, and Paris comes in third at 51.7%. 
#This suggests that many hosts in these cities may be casual or occasional renters rather 
#than professional hosts with multiple listings.

professional

ggplot(professional,aes(x=reorder(City,-prop_multi),y=prop_multi)) + 
  geom_bar(stat='identity') + 
  labs(title = 'Proportion of Airbnb Listings Offered by Hosts with Multiple Listings in Each City',
       y='Percentage of Listings Offered by Hosts with Multiple Listings (%)',
       x='') + 
  theme_s + 
  theme(axis.text.x= element_text(angle=90))

#The chart depicts the distribution of listings offered by hosts with multiple
#(i.e., hosts with two to four listings) across several cities. 
#According to the data, Rome had the highest percentage of such listings at 38.7%, 
#with Amsterdam (30.8%) and Budapest (29.8%) following closely behind. 
#This indicates that a significant proportion of listings in these cities are 
#likely to be offered by professional hosts rather than individuals.

professional
ggplot(professional,aes(x=reorder(City,-prop_biz),y=prop_biz)) + 
  geom_bar(stat='identity') + 
  labs(title = 'Proportion of Listings offered by Business by City ',
       y='Proportion of Listings offered by Business in %',
       x='') + 
  theme_s + 
  theme(axis.text.x= element_text(angle=90))

# Notes: The graph illustrates the percentage of lists that are offered
# by businesses(i.e, hosts with more than 4 listings) across several cities.
# The majority of listings in Lisbon are offered by businesses (58.9%).


kable(professional,format='html',
      caption = 'Descriptive statistics of dummy explanatory variables (share of listings (%))') %>%
  kable_styling(bootstrap_options = 'striped',full_width = T)






data$host_is_superhost = ifelse(data$host_is_superhost == 'True',1,
                                ifelse(data$host_is_superhost == 'False',0,
                                       data$host_is_superhost))

data$host_is_superhost = as.integer(data$host_is_superhost)


superhost=data %>% group_by(City) %>% summarize(Yes = sum(host_is_superhost)/n()*100,
                                      No = 100-Yes)


superhost <- data %>% 
  group_by(City) %>% 
  summarize(Yes = sum(host_is_superhost)/n()*100,
            No = 100-Yes)
superhost

ggplot(superhost, aes(x = reorder(City,-Yes),y=Yes)) + 
       geom_bar(stat='identity') + 
  labs(title= 'Proportion of Superhosts (in %) ',
       y= 'Share of Superhost in %',
       x= '') + 
  theme_s + 
  theme(axis.text.x=element_text(angle=90))


# This graph illustrates the proportion of superhosts by city. 
# Athens and Barcelona have the highest percentage of superhosts, both at 43.1%. 
# Superhosts are hosts who have earned a fabulous reputation within the Airbnb community.







bedroom_summary <- data %>% 
  group_by(City) %>% 
  summarize(average_bedrooms = mean(bedrooms),
            sd_bedroom = sd(bedrooms))


ggplot(bedroom_summary, aes(x = City, y = average_bedrooms)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = average_bedrooms - sd_bedroom, ymax = average_bedrooms + sd_bedroom),
                width = 0.2, color = "black", size = 0.7) +
  labs(title = "Average Number of Bedrooms by City",
       x = "City", y = "Average Number of Bedrooms") +
  theme_s + 
  theme(axis.text.x=element_text(angle=90))



#This graph presents the average number of bedrooms per listing in different cities. 
#The majority of listings have around 1 to 1.28 bedrooms with a standard deviation 
#ranging between 0.74 to 0.55. 
#The error bars indicate the lower and upper bounds of the number of bedrooms. 
#For instance, about 68% of the listings in Amsterdam have between 0.58 to 2.02 bedrooms. 
#By using three standard deviations, we can identify the range of the number of bedrooms 
#that fall within 99.7% of the confidence interval. 
#Amsterdam, Athens, and Barcelona have the highest average number of bedrooms per listing at 1.28, 
#while Paris has the lowest average at 0.96.


person_summary = data %>% group_by(City) %>% summarize(avg_person = mean(person_capacity),
                                                       sd = sd(person_capacity))
person_summary

ggplot(person_summary,aes(x=City,y=avg_person)) + 
  geom_bar(stat = 'identity',fill='steelblue') + 
  geom_errorbar(aes(ymax= avg_person + sd,ymin= avg_person - sd),width= 0.2, color='black',
                size=0.7) + 
  labs(title= 'Average Person Capacity by City',
       y='Average Person capacity',
       x='City') + 
  theme_s + 
  theme(axis.text.x=element_text(angle=90))


#The graph depicts the average maximum number of guests permitted per listing in various cities. 
#Athens and Barcelona have the highest average person capacity per listing at 3.71. 
#This implies that the maximum number of people allowed in the listing is 3.71. 
#The number of rooms can provide an estimate of the number of entire houses/apartments 
#available in the listing and can also indicate the size of the listings. 
#Bigger houses/apartments can accommodate more guests than smaller ones. 
#The error bars represent the standard deviation of the maximum number of guests permitted. 


location=data %>% group_by(City) %>% summarize( avg_dist = mean(dist),
                    sd_dist = sd(dist),
                    avg_metro = mean(metro_dist),
                    sd_metro = sd(metro_dist),
                    avg_attr = mean(attr_index_norm),
                    sd_attr = sd(attr_index_norm),
                    avg_rest = mean(rest_index_norm),
                    sd_rest = sd(rest_index_norm))

location


ggplot(location,aes(x=City,y=avg_dist)) + 
  geom_bar(stat='identity',fill='steelblue') +
  geom_errorbar(aes(ymax=avg_dist+sd_dist,ymin=avg_dist-sd_dist),size=0.7,
                width=0.2,color='black') + 
  labs(title='Average distance to City Centre by City',
       y='Average Distance in km',
       x='City')  +
  theme_s


# The graph presents data on the average distance between Airbnb listings 
#and the city center for different cities. It shows that Athens and Barcelona 
#have the shortest average distance (1.78 km) to the city center, 
#while London has the longest average distance (5.33 km) among all the cities in the dataset



ggplot(location,aes(x=City,y=avg_metro)) + 
  geom_bar(stat='identity',fill='steelblue') +
  geom_errorbar(aes(ymax=avg_metro+sd_metro,ymin=avg_metro-sd_metro),size=0.7,
                width=0.2,color='black') + 
  labs(title='Average distance to Metro by City',
       y='Average Distance in km',
       x='City') + 
  theme_s

#The graph presents the average distance between the Airbnb listings and the nearest metro station 
#across different cities. Paris has the shortest average distance of 0.22 km, 
#which is the closest among all cities in the dataset. 
#On the other hand, Amsterdam has the longest average distance of 1.09 km.



ggplot(location,aes(x=City,y=avg_attr)) + 
  geom_bar(stat='identity',fill='steelblue') +
  geom_errorbar(aes(ymax=avg_attr+sd_attr,ymin=avg_attr-sd_attr),size=0.7,
                width=0.2,color='black') + 
  labs(title=' Closest distance to the most popular attractions',
       y='Attraction Index',
       x='City')

#This graph shows the relative attraction index of attractions in different cities. 
#Attractions with higher indices have more reviews and are closer to the listings, 
#indicating a higher level of popularity. 
#London has the highest average normalized attraction index at 20.6,
#which suggests that listings in London are closer to popular attractions 
#compared to other cities in the dataset.



ggplot(location,aes(x=City,y=avg_rest)) + 
  geom_bar(stat='identity',fill='steelblue') +
  geom_errorbar(aes(ymax=avg_rest+sd_rest,ymin=avg_rest-sd_rest),size=0.7,
                width=0.2,color='black') + 
  labs(title="Proximity to Popular Attractions by City",
       y=' Normalized Attraction Index',
       x='City') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   theme_s

#This graph displays the relative attraction index of restaurants in various cities, 
#where higher indices indicate greater popularity based on the number of reviews 
#and proximity to listings. Paris has the highest average normalized attraction index at 47.9, 
#indicating that listings in Paris are closer to popular restaurants compared to other cities in the dataset.
#For guests who value proximity to popular restaurants, 
#Paris would be the ideal city to rent an Airbnb listing 
#as it has a significantly higher average normalized attraction index for restaurants 
#compared to other cities in the dataset.

kable(location,format='html',
      caption = 'Descriptive statistics for location variables (various units)') %>%
  kable_styling(bootstrap_options = 'striped',full_width = F)




str(data)
data %>% group_by(room_type) %>% summarize(median = median(realSum),
                                             max = max(realSum),
                                             min= min(realSum),
                                             mean=mean(realSum),
                                             q1 = quantile(realSum,0.25),
                                             q3 = quantile(realSum,0.75),
                                             iqr= IQR(realSum))

ggplot(data,aes(x=room_type,y=realSum,)) + 
  geom_boxplot() +
  ylim(0,3500) + 
  labs(title= 'Distribution of Listing Prices by Room type (in Euro)',
       x='Room type',
       y='Listing Price in Eur') + 
  theme_s


#The box plot displays the pricing distribution of Airbnb listings based on the type of room. 
#The median price for an entire home/apartment is the highest among all the types, which is 220 Euros. 
#The median price for private rooms is 159 Euros and the median price for a shared room is 122 Euro. 
#The interquartile range of homes is the largest among all types, 
#indicating that the middle 50% of the home listings' prices have more spread with a value of 185 Euros. 
#It's worth noting that all types of rooms have a considerable number of outliers, 
#which represent listings that are much more expensive than the average. 
#It is also worth mentioning that listings above 3500 Euros have been removed from the analysis. 


Mode(data$realSum)

ggplot(data,aes(x=realSum)) + 
  geom_histogram(bins=100, fill = "steelblue") + 
  geom_vline(aes(xintercept=median(realSum)), color="red", linetype="dashed", size=1) +
  annotate("text", x=250, y=600, label=paste("Median Price:", round(median(data$realSum),2)), color="red", size=4) +
  xlim(0, 1000) + 
  labs(title= 'Distribution of Listing Prices ',
       y='Frequency',
       x='Listing Price in Euro') +
  theme_s

#The graph displays the distribution of Airbnb listing prices, 
#which is skewed to the right, indicating that some listings have a much higher cost than the average. 
#The median price of listings is 196 Euros, while the average is 262.99 Euros. 
#The highest-priced listing is 18,545 Euros, and the lowest-priced listing is 37.13 Euros. 
#The interquartile range, which is the range between the 25th and 75th percentiles, is 159.77 Euros. 
#The mode, or most frequently occurring value, is 115.9984 Euros.


data$realSum %>% summary()

data %>% mutate(log_realSum = log(realSum)) %>% 
  ggplot(aes(x=log_realSum)) + 
  geom_histogram() + 
  labs(title = 'Distribution of Logarithmic Listing Prices',
       y='Frequency',
       x='Log Listing Price') +
  theme_s

#The histogram illustrates the distribution of the logarithm of listing prices. 
#The shape of the distribution is slightly right-skewed, but overall 
#it appears to be relatively symmetrical with a few outliers.


#Part 2 

data=data %>% mutate(log_realSum = log(realSum))


cor(data$log_realSum,data$dist)


ggplot(data,aes(x=dist,y=log_realSum)) + 
         geom_jitter(alpha=0.4) + 
  labs(title='Scatterplot of Log-Transformed Listing Price vs Distance to City Centre',
       y='Log Listing Price',
       x='Distance to City Centre in km')


ggplot(data,aes(x=dist,y=realSum)) + 
  geom_jitter(alpha=0.4) + 
  ylim(0,3500)


df=data %>% filter(realSum <800)

cor(df$realSum,df$dist)




#The scatterplot shows the relationship between the log of listing price and distance from the city center. 
#The correlation coefficient between the two variables is -0.03, which indicates a weak negative relationship. 
#However, based on the expectation that listings closer to the city center would cost more, 
#a moderate negative relationship was anticipated. Although the dataset contains a large number of 
#listings with prices significantly higher than the average, 
#filtering out listings priced under 1000, 800, and 500 Euro's, 
#after computing the log listing price, did not result in a significant change in the correlation coefficient.




cor(data$metro_dist,data$log_realSum)

ggplot(data,aes(x=metro_dist,y=log_realSum)) + 
  geom_jitter(alpha=0.4) + 
  labs(title='Relationship between Metro Proximity and Listing Price (Log Scale)',
       y='Log Listing Price',
       x='Distance to the Metro in km') + 
  theme_s


df=data %>% filter(realSum <800)
cor(df$realSum,df$metro_dist)


# These notes describe a scatterplot that displays the relationship between the
# logarithm of the listing price and the distance to the nearest metro station.
# The correlation coefficient between these two variables is -0.07, indicating
# a weak negative relationship. The scatterplot suggests that properties closer
# to the metro station tend to have slightly higher prices than those that are further
# away, but the relationship is not strong.

cor(data$attr_index,data$log_realSum)

ggplot(data,aes(x=attr_index,y=log_realSum)) + 
  geom_jitter(alpha=0.4) + 
  labs(title='Relationship between Log Listing Price and Attraction Index',
              y='Log Listing Price',
              x='Attraction index') +
  theme_s

# These notes describe a moderate positive correlation coefficient of 0.36
# between the logarithm of price of a listing and its relative attraction index,
# indicating that as the index of attraction increases, so does the price of the listing.
# However, it is important to note that the relationship is not strong and therefore
# we cannot conclude with certainty that listings closer to popular attractions cost more.


ggplot(data,aes(x=rest_index,y=log_realSum)) + 
  geom_jitter(alpha=0.4) + 
  labs(title='Relationship between Log Listing Price and Restaurant Index',
       y='Log Listing Price',
       x='Restaurant index') +
  theme_s

cor(data$rest_index,data$realSum)


# This means that there is a slight tendency for listings closer to popular restaurants 
# to have higher prices, but the relationship is not strong.



cor(data$log_realSum,data$attr_index_norm)

ggplot(data,aes(x=attr_index_norm,y=log_realSum)) + 
  geom_jitter(alpha=0.3) + 
  labs(title='Log Listing Price vs Normalized Attraction Index',
       y='Log Listing Price',
       x='Attraction index normalized') +
  theme_s

# The correlation coefficient between the logarithm of the listing price and 
# the normalized attraction index is 0.49, indicating a moderate positive relationship. 
# The normalized attraction index is based on a benchmark of the highest attraction in the index, 
# and a higher index value indicates a closer proximity to more popular attractions. 
# Therefore, the moderate positive relationship suggests that listings closer to 
# popular attractions tend to have higher prices.





ggplot(data,aes(x=rest_index_norm,y=log_realSum)) + 
  geom_jitter(alpha=0.3) + 
  labs(title='Log listing price vs Normalized Restaurant Index ',
       y='Log Listing Price',
       x='Restaurant index normalized') + 
  theme_s


cor(data$log_realSum,data$rest_index_norm)


# The correlation coefficient between the logarithm of the 
# listing price and the normalized restaurant index is 0.32, 
# indicating a moderate positive relationship. 
# This suggests that listings located closer to restaurants 
# that have a higher ranking in the restaurant index relative 
# to the benchmark are likely to be priced higher. 
# However, it's important to note that this relationship is not applicable to 
# all listings and there may be other factors that influence the price of a listing. 


cor(data$log_realSum,data$cleanliness_rating)

ggplot(data,aes(x=cleanliness_rating,y=log_realSum))+
  geom_jitter(alpha=0.2) + 
  labs(title='Log Listing Price vs Cleanliness Rating',
       y='Log Price',
       x= 'Cleanliness rating') +
  theme_s



# The graph displays the correlation between the logarithm of the listing price 
# and the cleanliness rating. Although the modified graph shows that listings 
# with a higher cleanliness rating tend to have higher prices, 
# the correlation coefficient of -0.007 indicates that there is no significant 
# relationship between the two variables.


cor(data$log_realSum,data$guest_satisfaction_overall)

ggplot(data,aes(x=as.integer(guest_satisfaction_overall),y=log_realSum))+
  geom_jitter(alpha=0.2) + 
  labs(title= 'Relationship between Guest Satisfaction and Log Listing Price in Euro',
       y='Log Price',
       x= 'Guest Satisfaction rating') +
  theme_s

# The graph displays the correlation between the logarithm of the listing price 
# and the overall guest satisfaction rating. However, the correlation coefficient 
# is nearly zero, indicating that there is no discernible relationship 
# between the two variables.


numeric_cols = sapply(data,is.numeric)
numeric_data = data[,numeric_cols]

numeric_data=numeric_data %>% select(-c(lng,lat))

cor_matrix = cor(numeric_data, use = 'pairwise.complete.obs')
cor_df = as.data.frame(as.table(cor_matrix))
names(cor_df) = c('Variable1','Variable2','Correlation')

ggplot(cor_df, aes(Variable1, Variable2, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title='Airbnb 2021 correlation matrix1')




# The correlation matrix shows that there are no significant strong relationships between most of the variables, 
# except for the pair of variables realSum and log_realSum.



model1 =lm(realSum ~ attr_index_norm + rest_index_norm + person_capacity + 
            (attr_index_norm)^2 +  (rest_index_norm)^2 + (person_capacity)^2 ,data = data)


model2 =lm(realSum ~ attr_index_norm + rest_index_norm + person_capacity,data=data) 
model2

summary_output = summary(model1)
r_squared1 = summary_output$r.squared
print(paste('Model 1 R-squared:',r_squared1))


summary_output1=summary(model2)
r_squared=summary_output1$r.squared
print(paste('Model 2 R-squared:',r_squared))








# Both of the regression models have low R-squared values, 
# indicating that the independent variables have limited explanatory power 
# on the dependent variable. Model 2 has an intercept that is negative, 
# which is an unrealistic scenario since listings cannot have a 
# price less than zero. The coefficient for the normalized attraction index is 11.15, 
# which implies that a one-unit increase in the index, assuming that the other variables 
# remain constant, leads to an increase in the listing price by an average of 11.15 Euros. 
# On the other hand, the coefficient for the normalized restaurant index is -0.2117, 
# indicating that a one-unit increase in the index, while holding other variables constant, 
# results in a decrease in the listing price by 0.2117 Euros. 
# Additionally, the coefficient for person capacity is 45.3366, 
# which suggests that an increase in the capacity of one person, 
# while holding other variables constant, results in an average increase of 45.3336 Euros 
# in the listing price. These specific variables were chosen due to their 
# relatively high linear correlation with the dependent variable, 
# and the independence of the explanatory variables 
# to each other to isolate their unique effects on the dependent variable.



#Part 3 
# Cities on Weekends



amsterdam_weekends = read.csv('amsterdam_weekends.csv')
amsterdam_weekends$City = rep('Amsterdam',nrow(amsterdam_weekends))
amsterdam_weekends = amsterdam_weekends[,c('City','realSum',names(amsterdam_weekends)[-c(1,2)])]
amsterdam_weekends$City.1 = NULL


athens_weekends = read.csv('athens_weekends.csv')
athens_weekends$City = rep('Athens',nrow(athens_weekends))
athens_weekends = athens_weekends[,c('City','realSum',names(athens_weekends)[-c(1,2)])]
athens_weekends$City.1 = NULL


barcelona_weekends = read.csv('barcelona_weekends.csv')
barcelona_weekends$City = rep('Barcelona',nrow(barcelona_weekends))
barcelona_weekends = barcelona_weekends[,c('City','realSum',names(barcelona_weekends)[-c(1,2)])]
barcelona_weekends$City.1 = NULL



berlin_weekends = read.csv('berlin_weekends.csv')
berlin_weekends$City = rep('Berlin',nrow(berlin_weekends))
berlin_weekends = berlin_weekends[,c('City','realSum',names(berlin_weekends)[-c(1,2)])]
berlin_weekends$City.1 = NULL


budapest_weekends = read.csv('budapest_weekends.csv')
budapest_weekends$City = rep('Budapest',nrow(budapest_weekends))
budapest_weekends = budapest_weekends[,c('City','realSum',names(budapest_weekends)[-c(1,2)])]
budapest_weekends$City.1 = NULL

lisbon_weekends = read.csv('lisbon_weekends.csv')
lisbon_weekends$City = rep('Lisbon',nrow(lisbon_weekends))
lisbon_weekends = lisbon_weekends[,c('City','realSum',names(lisbon_weekends)[-c(1,2)])]
lisbon_weekends$City.1 = NULL



london_weekends = read.csv('london_weekends.csv')
london_weekends$City = rep('London',nrow(london_weekends))
london_weekends = london_weekends[,c('City','realSum',names(london_weekends)[-c(1,2)])]
london_weekends$City.1 = NULL


paris_weekends = read.csv('paris_weekends.csv')
paris_weekends$City = rep('Paris',nrow(paris_weekends))
paris_weekends = paris_weekends[,c('City','realSum',names(paris_weekends)[-c(1,2)])]
paris_weekends$City.1 = NULL




rome_weekends = read.csv('rome_weekends.csv')
rome_weekends$City = rep('Rome',nrow(rome_weekends))
rome_weekends = rome_weekends[,c('City','realSum',names(rome_weekends)[-c(1,2)])]
rome_weekends$City.1 = NULL



vienna_weekends = read.csv('vienna_weekends.csv')
vienna_weekends$City = rep('Vienna',nrow(vienna_weekends))
vienna_weekends = vienna_weekends[,c('City','realSum',names(vienna_weekends)[-c(1,2)])]
vienna_weekends$City.1 = NULL



data2 = rbind(amsterdam_weekends,athens_weekends,barcelona_weekends,
              berlin_weekends,budapest_weekends,lisbon_weekends,london_weekends,
              paris_weekends,rome_weekends,vienna_weekends)


data2 %>% group_by(City) %>% summarize(n = n(),
                                       median = median(realSum),
                                       avg = mean(realSum),
                                       sd = sd(realSum)) 


table1=data %>% group_by(City) %>% summarize(n = n(),
                                       median = median(realSum),
                                       avg = mean(realSum),
                                       sd = sd(realSum)) 





table2 <- data2 %>% 
  group_by(City) %>% 
  summarize(n = n(),
            median = median(realSum),
            avg = mean(realSum),
            sd = sd(realSum)) 





diff=table1 %>% inner_join(table2,by = 'City',suffix= c('_weekdays','_weekends')) %>% 
  mutate(diff_median = median_weekends - median_weekdays,
         diff_avg = avg_weekends - avg_weekdays,
         diff_sd = sd_weekends - sd_weekdays) %>% select(City,diff_median,diff_avg,diff_sd)
diff


diff %>% mutate(color = ifelse(diff_median < 0, 'orange','blue')) %>% 
ggplot(aes(x=City,y=diff_median,fill=color)) + 
  geom_bar(stat='identity') + 
  scale_fill_manual('Change',values= c('blue','orange'),
                    labels = c('Positve','Negative')) + 
  theme(axis.text.x = element_text(angle=90)) + 
  labs(title = 'Analyzing the Median Listing Price Difference between Weekends and Weekdays by City') + 
  theme_s

# This graph compares the median listing prices between weekends and weekdays for different cities.
# The data shows that Barcelona has the highest increase in median prices on weekends, with a difference
# of 77 Euro's between weekends and weekdays. Paris, on the other hand, has a smaller difference in median
# prices between the two time periods. In general, the median listing prices tend to be higher on weekends
# compared to weekdays.


diff %>% mutate(color = ifelse(diff_avg < 0, 'orange','blue')) %>% 
  ggplot(aes(x=City,y=diff_avg,fill=color)) +
           geom_bar(stat='identity') + 
           scale_fill_manual('Change',values = c('blue','orange'),
                             labels = c('Positve','Negative')) + 
          theme(axis.text.x= element_text(angle=90)) + 
  labs(title = 'Analyzing the Average Listing Price Difference between Weekends and Weekdays by City') +
  theme_s

# This graph shows the average changes in listing prices between weekends and 
# weekdays for various cities. According to the data, 
# Barcelona had the largest difference in average prices 
# with a weekend price increase of 144 Euros. Athens and Paris, 
# on the other hand, had cheaper average prices on weekends. 
# Overall, the graph indicates that there are differences in 
# average listing prices between weekends and weekdays for different cities.


diff



diff %>% mutate(color = ifelse(diff_sd < 0, 'orange','blue')) %>%
      ggplot(aes(x=City,y=diff_sd,fill=color)) + 
               geom_bar(stat='identity') +
              scale_fill_manual(name='Change',
                                values = c('blue','orange'),
                                labels = c('Positive','Negative')) + 
      theme(axis.text.x = element_text(angle=90)) + 
  labs(title='Weekend vs. Weekday: Analyzing Changes in Listing Price Variation by City')

# This graph compares the range in variability in listing prices between listings on 
# weekends and weekdays by city. Listings in Athens have the highest change in the range
# of variability across all other cities. This implies that most listings in Athens on the 
# weekends are clustered close around the mean. In comparison, on a weekend, 68% of the 
#  median listings fall between 207 and 48.2 Euro listings. In comparison, on  weekends, 68% of the 
#  median listings fall between -210.71 and around 522 Euro. Overall, the data suggests less variability 
# between prices on weekends.



# This graph displays the difference in variability in listing prices between 
# weekends and weekdays for various cities. Athens shows the largest change 
# in variability, suggesting that on weekends, most listings are concentrated 
# around the mean price. For example, on a weekend, 68% of the median listings 
# in Athens fall within the range of 207 to 48.2 Euros, while in comparison,
# 68% of median listings on weekdays across all cities fall within 
# the range of -238.78 to 496 Euros. Overall, the data indicates that there is 
# less variability in prices on weekends.


diff=diff %>% select(-c(cum_sum,color))

diff1=kable(diff, format = 'html',
      caption = "Difference in descriptive statistics for prices between 
      Weekends and Weekdays (in EUR)")  %>% 
  kable_styling(bootstrap_options = 'striped', full_width = T)

diff1

table1=kable(table1, format = "html", 
             caption = "Number of listings and descriptive statistics for prices(in EUR) on Weekdays in 2021") %>%
  kable_styling(bootstrap_options = "striped", full_width = T)





table2=kable(table2, format='html',
             caption = "Number of listings and descriptive statistics for prices (in EUR) on Weekends in 2021") %>%
  kable_styling(bootstrap_options = 'striped',full_width = T)



#Conlusion
# In conclusion, my analysis did not reveal any strong predictors of listing price, 
# as the variables in the dataset had poor explanatory power. 
# However, based on my findings, it is necessary to include other important 
# variables such as the size of the house, the median price of houses in the area
# and the market value of the apartment/house other variables. 
# Additionally, my analysis showed that listings tend to 
# be more expensive on weekends than on weekdays in most cities, 
# and there is less variability in listing prices on weekends compared to weekdays.







