#import the required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

setwd('C:/Users/NILADRI/AA MOLLIKDA/PROG/R/DATA ANALYSIS PROJECT/Exploratory Data Analysis in R')

power_con <- read.csv('Tetuan City power consumption.csv')

head(power_con)

#function to clean up the dataframe
clean_data_frame <-function(dataframe){
  #dateTimeParts is the list containing the parts of month, day and year+time
  dateTimeParts <- strsplit(dataframe$DateTime,'/')
  
  #creating new columns for day, month
  dataframe$Day <- sapply(dateTimeParts,'[',2)
  dataframe$Month <- sapply(dateTimeParts,'[',1)
  
  #dateTimePartsYearandTime is the string containing year+time together separated by ' '
  dateTimePartsYearandTime <- sapply(dateTimeParts,'[',3)
  
  #dateTimePartsYearandTimeParts is the list that we get after splitting dateTimePartsYearandTime (in the previous 2 lines) with space (' ') as delimiter. This will separate year and time.
  dateTimePartsYearandTimeParts <- strsplit(dateTimePartsYearandTime,' ')
  
  
  dataframe$Year <- sapply(dateTimePartsYearandTimeParts,'[',1)
  dataframe$Time <- sapply(dateTimePartsYearandTimeParts,'[',2)
  
  df2 <- data.frame(dataframe[,c('DateTime','Year',	'Month','Day','Time','Temperature','Humidity','Wind.Speed','general.diffuse.flows','diffuse.flows','Zone.1.Power.Consumption','Zone.2..Power.Consumption','Zone.3..Power.Consumption'
  )])
  
  #writing the resultant dataframe to a file
  write.csv(df2,file = "output Tetuan City power consumption.csv", row.names = F)
  
  #returning the resultant dataframe
  return(df2)
}

cleaned_power_con <- clean_data_frame(power_con)

head(cleaned_power_con)
tail(cleaned_power_con)

#summary of dataframe
summary_stats = summary(cleaned_power_con)
print(summary_stats)

summary(cleaned_power_con$Temperature)
summary(cleaned_power_con$Humidity)
summary(cleaned_power_con$Wind.Speed)

#Correlation coefficients
cor(cleaned_power_con$Temperature, cleaned_power_con$Humidity, method = c('spearman'))

####################################################

#function to aggregate power consumption zones
func_sum_aggregation_zones <- function(dataframe,  PowerConsumption){
  sum_zone_pc <- aggregate(data = dataframe, PowerConsumption~Month, FUN = sum)
  sum_zone_pc$Month <- as.integer(sum_zone_pc$Month)
  sum_zone_pc$MonthName <- month.name[sum_zone_pc$Month]
  sum_zone_pc <- sum_zone_pc[order(sum_zone_pc$Month),]
  return (sum_zone_pc)
}

#create sum aggregation table for Zone 1 Power Consumption
sum_zone1_pc <- func_sum_aggregation_zones(cleaned_power_con, cleaned_power_con$'Zone.1.Power.Consumption')
#rename columns
names(sum_zone1_pc)[names(sum_zone1_pc) == 'PowerConsumption'] <- 'Zone1_PC'
print(sum_zone1_pc)

ggplot(data = sum_zone1_pc, aes(x = month.name, y = Zone1_PC)) + 
  geom_bar(stat = 'identity', fill = 'lightgreen', color = 'black') + 
  scale_y_continuous(labels = label_number(scale = 1e-6))+
  labs(x = 'Months', y = 'Power consumption (in millions)', title = 'Zone 1 Power Consumption') +
  theme(plot.title = element_text(hjust = 0.5))

#create sum aggregation table for Zone 2 Power Consumption
sum_zone2_pc <- func_sum_aggregation_zones(cleaned_power_con, cleaned_power_con$'Zone.2..Power.Consumption')
#rename columns
names(sum_zone2_pc)[names(sum_zone2_pc) == 'PowerConsumption'] <- 'Zone2_PC'
print(sum_zone2_pc)

ggplot(data = sum_zone2_pc, aes(x = month.name, y = Zone2_PC)) + 
  geom_bar(stat = 'identity', fill = 'orange', color = 'black') + 
  scale_y_continuous(labels = label_number(scale = 1e-6)) +
  labs(x = 'Months', y = 'Power consumption (in millions)', title = 'Zone 2 Power Consumption') +
  theme(plot.title = element_text(hjust = 0.5))


#create sum aggregation table for Zone 3 Power Consumption
sum_zone3_pc <- func_sum_aggregation_zones(cleaned_power_con, cleaned_power_con$'Zone.3..Power.Consumption')
#rename columns
names(sum_zone3_pc)[names(sum_zone3_pc) == 'PowerConsumption'] <- 'Zone3_PC'
print(sum_zone3_pc)

ggplot(data = sum_zone3_pc, aes(x = month.name, y = Zone3_PC)) + 
  geom_bar(stat = 'identity',  fill = 'skyblue', color = 'black') + 
  scale_y_continuous(labels = label_number(scale = 1e-6)) + 
  labs(x = 'Months', y = 'Power consumption (in millions)', title = 'Zone 3 Power Consumption') +
  theme(plot.title = element_text(hjust = 0.5))

########################################################################

#function to calculate average wind speed, humidity and temperature
func_avg_weather <- function(dataframe,  column){
  avg_zone_pc <- aggregate(data = dataframe, column~Month, FUN = mean)
  avg_zone_pc$Month <- as.integer(avg_zone_pc$Month)
  avg_zone_pc$MonthName <- month.name[avg_zone_pc$Month]
  avg_zone_pc <- avg_zone_pc[order(avg_zone_pc$Month),]
  return (avg_zone_pc)
}

#average temperature, humidity and power consumption
avg_wind_speed <- func_avg_weather(cleaned_power_con, cleaned_power_con$Wind.Speed)
names(avg_wind_speed)[names(avg_wind_speed) == 'column'] <- 'AvgWindSpeed'
avg_wind_speed

avg_temp <- func_avg_weather(cleaned_power_con, cleaned_power_con$Temperature)
names(avg_temp)[names(avg_temp) == 'column'] <- 'AvgTemperature'
avg_temp

avg_humidity <- func_avg_weather(cleaned_power_con, cleaned_power_con$Humidity)
names(avg_humidity)[names(avg_humidity) == 'column'] <- 'AvgHumidity'
avg_humidity

#creating a list of the aggregated dataframes
dfs <- list(sum_zone1_pc, sum_zone2_pc, sum_zone3_pc,
            avg_temp, avg_humidity, avg_wind_speed)
#merging dataframes by Reduce function
aggregated_df <- Reduce(function(x,y) merge(x, y, by = c('Month', 'MonthName')), dfs)

print(aggregated_df)

##########################################################################

#display line chart of wind speed over zone 2 power consumption
ggplot(data = aggregated_df, aes(x = month.name, y = Zone1_PC/1000000)) + 
  geom_bar(stat = 'identity',  fill = 'skyblue', color = 'black') + 
labs(x = 'Months', y = 'Power consumption (in million)', title = 'Zone 1 Power Consumption') +
  geom_line(aes(y = AvgWindSpeed, group = 1, color = 'AvgWindSpeed'),size = 1.2) +
  geom_line(aes(y = AvgTemperature, group = 1, color = 'Average Temp'), size = 1.2) +
  geom_line(aes(y = AvgHumidity, group = 1, color = 'Average Humidity'), size = 1.2)+
  theme(plot.title = element_text(hjust = 0.5))


#display line chart of wind speed over zone 2 power consumption
ggplot(data = aggregated_df, aes(x = month.name, y = Zone2_PC/1000000)) + 
  geom_bar(stat = 'identity',  fill = 'lightgreen', color = 'black') + 
  labs(x = 'Months', y = 'Power consumption (in million)', title = 'Zone 2 Power Consumption') +
  geom_line(aes(y = AvgWindSpeed, group = 1, color = 'AvgWindSpeed'),size = 1.2) +
  geom_line(aes(y = AvgTemperature, group = 1, color = 'Average Temp'), size = 1.2) +
  geom_line(aes(y = AvgHumidity, group = 1, color = 'Average Humidity'), size = 1.2)

  theme(plot.title = element_text(hjust = 0.5))

#display line chart of wind speed over zone 3 power consumption
ggplot(data = aggregated_df, aes(x = month.name, y = Zone1_PC/1000000)) + 
  geom_bar(stat = 'identity',  fill = 'orange', color = 'black') + 
  labs(x = 'Months', y = 'Power consumption (in million)', title = 'Zone 3 Power Consumption') +
geom_line(aes(y = AvgWindSpeed, group = 1, color = 'AvgWindSpeed'),size = 1.2) +
  geom_line(aes(y = AvgTemperature, group = 1, color = 'Average Temp'), size = 1.2) +
  geom_line(aes(y = AvgHumidity, group = 1, color = 'Average Humidity'), size = 1.2) +
  theme(plot.title = element_text(hjust = 0.5))

