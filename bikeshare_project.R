#load the necessary packages

library(ggplot2)
library(tidyverse)
library(lubridate)

#Read your file into their respective variable

ny = read.csv('new-york-city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

#Question 1
#What is the most common day of the week traveled in across the three states

#Extract the weekday from the Start.Time
wash1 <- mutate(wash, weekday = (weekdays(as.POSIXlt (wash$Start.Time))))

ny1 <- mutate(ny, weekday = (weekdays(as.POSIXlt (ny$Start.Time))))

chi1 <- mutate(chi, weekday = (weekdays(as.POSIXlt (chi$Start.Time))))


#Get the summary of weekday
chi$Weekday <- format(as.Date(chi$Start.Time), "%A")
chi.day <- chi$Weekday
ny$Weekday <- format(as.Date(ny$Start.Time), "%A")
ny.day <- ny$Weekday
wash$Weekday <- format(as.Date(wash$Start.Time), "%A")
wash.day <- wash$Weekday

#visualize the result

ggplot(data = day_all, aes(weekday)) + geom_bar(aes(fill = state), position = "dodge")

table(day_all)

#The graph shows that the most traveled day in New-York City is Wednesday with a total travel is 52087, and Wednesday in Washington with a total travel of 48156 and Tuesday in Chicago with total travel is 45912.#


#Question 2
#What is the most common Start Station in Chicago

chi_start_station <- (chi$Start.Station)
stations <- table(chi_start_station) %>%
  as.data.frame() %>%
  arrange(desc(Freq))

station <- head(stations, 1)
paste("The most common start Station is", station$chi_start_station)

station_plot_5 <- head(stations, 5)
qplot(x=chi_start_station, y= Freq, data=station_plot_5, color=I('Black'),fill=I('red'))+
ggtitle("Scattered plot of Chicago top 5 Station") +
labs(x = "Stations")+
labs(y = "Frequency")

#Question 3
#What are the counts of each user type in Washington

wash_User.Type <- table(wash$User.Type) %>%
  as.data.frame()
ggplot(wash, aes(User.Type))+
  geom_histogram(stat = "count", binwidth=30, fill='red', color='blue') +
  ggtitle("Gragh of User type in Washington")
wash_User.Type


#The graph indicates that Users who are Subscribers are the highest using the service with a total count of 220,786, while Users who are Customers have a total count of 79,214.
