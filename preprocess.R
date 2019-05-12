library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(markdown)
library(shinythemes)
library(shinycssloaders)
library(plyr)
library(lubridate)
library(dplyr)
library(data.table)

	# Daily_folder <- "Raw Files/Daily/"
	# Daily_file_list <- list.files(path = Daily_folder, pattern = "*.csv")
	# Daily_data <- do.call("rbind", lapply(Daily_file_list, 
	#                                       function(x)
	#                                          read.csv(paste(Daily_folder, x, sep=''), 
	#                                                  stringsAsFactors = FALSE)))
	# Daily_data_subset = subset(Daily_data, select = -c(State.Code, County.Code, Defining.Site, Number.of.Sites.Reporting))
	# write.csv(Daily_data_subset,file = "Daily_Subset.csv", rowname)

####################################################################################################################################################################################################

#Daily files processing


dir.create("Daily")
setwd("Daily")
dir.create("Map")
setwd("..")

years <- c(1990:2018)
for (i in years)
{
	file_name = paste0("Raw Files/Daily/daily_aqi_by_county_",i,".csv")
	write_file_name = paste0("Daily/daily_aqi_by_county_",i,".csv")
	#print(file_name)
	Daily_data_subset = read.csv(file_name)
	#Type_of_Days_Abv <- mapvalues(Daily_data_subset$Category, c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"), c("G", "M", "US", "U", "VU", "H"))
	Daily_data_subset <- subset(Daily_data_subset, select = -c(State.Code, County.Code, Defining.Site, Number.of.Sites.Reporting))
	#Daily_data_subset$Category <- Type_of_Days_Abv
	write.csv(Daily_data_subset, write_file_name, row.names = F)
}


#####################################################################################################################################################################################################

#Hourly files processing

dir.create("Hourly")
setwd("Hourly")
dir.create("CO")
dir.create("NO2")
dir.create("SO2")
dir.create("Wind")
dir.create("Temp")
dir.create("PM2_5")
dir.create("PM10")
dir.create("Ozone")
setwd("..")
#hourly <- c(hourly_42101_2018, hourly_42401_2018, hourly_42602_2018, hourly_44201_2018, hourly_81102_2018, hourly_88101_2018, hourly_TEMP_2018, hourly_WIND_2018)
#pollutants <- c(CO, SO2, NO2, Ozone, PM10, PM2_5, Temp, Wind)

df_CO = read.csv("Raw Files/Hourly/hourly_42101_2018.csv")
df_SO2 = read.csv("Raw Files/Hourly/hourly_42401_2018.csv")
df_NO2 = read.csv("Raw Files/Hourly/hourly_42602_2018.csv")
df_Ozone = read.csv("Raw Files/Hourly/hourly_44201_2018.csv")
df_PM10 = read.csv("Raw Files/Hourly/hourly_81102_2018.csv")
df_PM2_5 = read.csv("Raw Files/Hourly/hourly_88101_2018.csv")
df_Wind = read.csv("Raw Files/Hourly/hourly_WIND_2018.csv")
df_Temp = read.csv("Raw Files/Hourly/hourly_TEMP_2018.csv")

df_CO_ = subset(df_CO, select = c(State.Name, County.Name, Date.Local))
df_SO2_ = subset(df_SO2, select = c(State.Name, County.Name, Date.Local))
df_NO2_ = subset(df_NO2, select = c(State.Name, County.Name, Date.Local))
df_Ozone_ = subset(df_Ozone, select = c(State.Name, County.Name, Date.Local))
df_PM10_ = subset(df_PM10, select = c(State.Name, County.Name, Date.Local))
df_PM2_5_ = subset(df_PM2_5, select = c(State.Name, County.Name, Date.Local))
df_Wind_ = subset(df_Wind, select = c(State.Name, County.Name, Date.Local))
df_Temp_ = subset(df_Temp, select = c(State.Name, County.Name, Date.Local))

df_CO_ = distinct(df_CO_)
df_SO2_ = distinct(df_SO2_)
df_NO2_ = distinct(df_NO2_)
df_Ozone_ = distinct(df_Ozone_)
df_PM10_ = distinct(df_PM10_)
df_PM2_5_ = distinct(df_PM2_5_)
df_Wind_ = distinct(df_Wind_)
df_Temp_ = distinct(df_Temp_)

df_CO_$Gas = rep('CO', nrow(df_CO_))
df_SO2_$Gas = rep('SO2', nrow(df_SO2_))
df_NO2_$Gas = rep('NO2', nrow(df_NO2_))
df_Ozone_$Gas = rep('Ozone', nrow(df_Ozone_))
df_PM10_$Gas = rep('PM10', nrow(df_PM10_))
df_PM2_5_$Gas = rep('PM2.5', nrow(df_PM2_5_))
df_Wind_$Gas = rep('Wind', nrow(df_Wind_))
df_Temp_$Gas = rep('Temp', nrow(df_Temp_))

# write.csv(df_CO, "Hourly/CO_Dates.csv", row.names = F)
# write.csv(df_SO2, "Hourly/SO2_Dates.csv", row.names = F)
# write.csv(df_NO2, "Hourly/NO2_Dates.csv", row.names = F)
# write.csv(df_Ozone, "Hourly/Ozone_Dates.csv", row.names = F)
# write.csv(df_PM10, "Hourly/PM10_Dates.csv", row.names = F)
# write.csv(df_PM2_5, "Hourly/PM2_5_Dates.csv", row.names = F)
# write.csv(df_Wind, "Hourly/Wind_Dates.csv", row.names = F)
# write.csv(df_Temp, "Hourly/Temp_Dates.csv", row.names = F)

All_Gas_Dates = rbind(df_CO_, df_SO2_, df_NO2_, df_Ozone_, df_PM10_, df_PM2_5_, df_Wind_, df_Temp_) 
write.csv(All_Gas_Dates, "Hourly/All_Gas_Dates.csv", row.names = F)

months = c(1:12)


df_CO_temp = subset(df_CO, Sample.Measurement > 0, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_CO_temp)[colnames(df_CO_temp) == "Sample.Measurement"] <- "PPM"
df_CO_temp$m_m3 = round(((df_CO_temp$PPM * 28.01) / 24.45), digits = 2)
for (i in months)
{
	if (i < 10)
	{
	file_name = paste0("Hourly/CO/CO_0",i,".csv")
	} else {
	file_name = paste0("Hourly/CO/CO_",i,".csv")
	}
	df = subset(df_CO_temp, month(as.Date(df_CO_temp$Date.Local)) == i)
	write.csv(df, file_name, row.names = F)
}


df_SO2_temp = subset(df_SO2, Sample.Measurement > 0, select = c(State.Name, County.Name, Date.Local,Time.Local, Sample.Measurement))
colnames(df_SO2_temp)[colnames(df_SO2_temp) == "Sample.Measurement"] <- "PPM"
df_SO2_temp$PPM = df_SO2_temp$PPM/1000
df_SO2_temp$m_m3 = (df_SO2_temp$PPM * 64.066) / 24.45
for(i in months)
{
	if (i < 10)
	{
	file_name = paste0("Hourly/SO2/SO2_0",i,".csv")
	} else {
	file_name = paste0("Hourly/SO2/SO2_",i,".csv")
	}
	df = subset(df_SO2_temp, month(as.Date(df_SO2_temp$Date.Local)) == i)
	write.csv(df, file_name, row.names = F)
}


df_NO2_temp = subset(df_NO2, Sample.Measurement > 0, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_NO2_temp)[colnames(df_NO2_temp) == "Sample.Measurement"] <- "PPM"
df_NO2_temp$PPM = df_NO2_temp$PPM/1000
df_NO2_temp$m_m3 = (df_NO2_temp$PPM * 46.0055) / 24.45
for(i in months)
{
  if (i < 10)
  {
    file_name = paste0("Hourly/NO2/NO2_0",i,".csv")
  } else {
    file_name = paste0("Hourly/NO2/NO2_",i,".csv")
  }
  df = subset(df_NO2_temp, month(as.Date(df_NO2_temp$Date.Local)) == i)
  write.csv(df, file_name, row.names = F)
}


df_Ozone_temp = subset(df_Ozone, Sample.Measurement > 0, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_Ozone_temp)[colnames(df_Ozone_temp) == "Sample.Measurement"] <- "PPM"
df_Ozone_temp$m_m3 = (df_Ozone_temp$PPM * 48) / 24.45
for(i in months)
{
  if (i < 10)
  {
    file_name = paste0("Hourly/Ozone/Ozone_0",i,".csv")
  } else {
    file_name = paste0("Hourly/Ozone/Ozone_",i,".csv")
  }
  df = subset(df_Ozone_temp, month(as.Date(df_Ozone_temp$Date.Local)) == i)
  write.csv(df, file_name, row.names = F)
}


df_PM10_temp = subset(df_PM10, Sample.Measurement > 0, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_PM10_temp)[colnames(df_PM10_temp) == "Sample.Measurement"] <- "m_m3"
df_PM10_temp$PPM = (df_PM10_temp$m_m3 * 24.45) / 52.66
for(i in months)
{
  if (i < 10)
  {
    file_name = paste0("Hourly/PM10/PM10_0",i,".csv")
  } else {
    file_name = paste0("Hourly/PM10/PM10_",i,".csv")
  }
  df = subset(df_PM10_temp, month(as.Date(df_PM10_temp$Date.Local)) == i)
  write.csv(df, file_name, row.names = F)
}


df_PM2_5_temp = subset(df_PM2_5, Sample.Measurement > 0, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_PM2_5_temp)[colnames(df_PM2_5_temp) == "Sample.Measurement"] <- "m_m3"
df_PM2_5_temp$PPM = (df_PM2_5_temp$m_m3 * 24.45) / 52.66
for(i in months)
{
  if (i < 10)
  {
    file_name = paste0("Hourly/PM2_5/PM2_5_0",i,".csv")
  } else {
    file_name = paste0("Hourly/PM2_5/PM2_5_",i,".csv")
  }
  df = subset(df_PM2_5_temp, month(as.Date(df_PM2_5_temp$Date.Local)) == i)
  write.csv(df, file_name, row.names = F)
}


df_Wind_temp = subset(df_Wind, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_Wind_temp)[colnames(df_Wind_temp) == "Sample.Measurement"] <- "MPH"
df_Wind_temp$MPH = df_Wind_temp$MPH * 1.15077945
df_Wind_temp$KPH = df_Wind_temp$MPH * 1.60934
for(i in months)
{
  if (i < 10)
  {
    file_name = paste0("Hourly/Wind/Wind_0",i,".csv")
  } else {
    file_name = paste0("Hourly/Wind/Wind_",i,".csv")
  }
  df = subset(df_Wind_temp, month(as.Date(df_Wind_temp$Date.Local)) == i)
  write.csv(df, file_name, row.names = F)
}


df_Temp_temp = subset(df_Temp, select = c(State.Name, County.Name, Date.Local, Time.Local, Sample.Measurement))
colnames(df_Temp_temp)[colnames(df_Temp_temp) == "Sample.Measurement"] <- "F"
df_Temp_temp$C = (df_Temp_temp$F - 32) * (5 / 9)
for(i in months)
{
  if (i < 10)
  {
    file_name = paste0("Hourly/Temp/Temp_0",i,".csv")
  } else {
    file_name = paste0("Hourly/Temp/Temp_",i,".csv")
  }
  df = subset(df_Temp_temp, month(as.Date(df_Temp_temp$Date.Local)) == i)
  write.csv(df, file_name, row.names = F)
}

########################################################################################################################################################

#Month_test <- mapvalues(Daily_data_subset$Category, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))

dir.create("Hong_Kong")
setwd("Hong_Kong")
dir.create("Daily")
dir.create("Hourly")
dir.create("Monthly")
dir.create("Map")
setwd("..")


df_HK_CO = read.csv("Raw Files/Hong Kong/CO.csv")
df_HK_CO$Date = substr(df_HK_CO$local, 1, 10)
df_HK_CO$Time = substr(df_HK_CO$local, 12, 16)
df_HK_CO_Hourly = subset(df_HK_CO, select = c(location, value, Date, Time))
colnames(df_HK_CO_Hourly)[colnames(df_HK_CO_Hourly) == "value"] <- "ug_m3"
df_HK_CO_Hourly$PPB = (df_HK_CO_Hourly$ug_m3 * 24.45) / 28.01
write.csv(df_HK_CO_Hourly, "Hong_Kong/Hourly/HK_CO_Hourly.csv", row.names = F)



##df_HK_CO$PPB = (df_HK_CO$ug_m3 * 24.45) / 28.01
##df_HK_CO_Daily = as.data.table(df_HK_CO)[, mean(ug_m3), by = .(location, city, Date)]
df_HK_CO_Daily = subset(df_HK_CO, select = c(location, value, Date, Time))
df_HK_CO_Daily = as.data.table(df_HK_CO_Daily)[, mean(value), by = .(location, Date)]
colnames(df_HK_CO_Daily)[colnames(df_HK_CO_Daily) == "V1"] <- "ug_m3"
df_HK_CO_Daily$PPB = (df_HK_CO_Daily$ug_m3 * 24.45) / 28.01
write.csv(df_HK_CO_Daily, "Hong_Kong/Daily/HK_CO_Daily.csv", row.names = F)

df_HK_CO_Monthly = subset(df_HK_CO, select = c(location, value, Date, Time))
df_HK_CO_Monthly = as.data.table(df_HK_CO_Monthly)[, mean(value), by = .(location, month(as.Date(Date)))]
colnames(df_HK_CO_Monthly)[colnames(df_HK_CO_Monthly) == "V1"] <- "ug_m3"
Month_CO <- mapvalues(df_HK_CO_Monthly$month, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))
df_HK_CO_Monthly$PPB = (df_HK_CO_Monthly$ug_m3 * 24.45) / 28.01
df_HK_CO_Monthly$Date = Month_CO
write.csv(df_HK_CO_Monthly, "Hong_Kong/Monthly/HK_CO_Monthly.csv", row.names = F)


df_HK_NO2 = read.csv("Raw Files/Hong Kong/NO2.csv")
df_HK_Lat_Log = subset(df_HK_NO2, select = c(location, latitude, longitude))
df_HK_Lat_Log = unique(df_HK_Lat_Log)
write.csv(df_HK_Lat_Log, "Hong_Kong/Map/HK_Location.csv", row.names = F)
df_HK_NO2$Date = substr(df_HK_NO2$local, 1, 10)
df_HK_NO2$Time = substr(df_HK_NO2$local, 12, 16)
df_HK_NO2_Hourly = subset(df_HK_NO2, select = c(location, value, Date, Time))
colnames(df_HK_NO2_Hourly)[colnames(df_HK_NO2_Hourly) == "value"] <- "ug_m3"
df_HK_NO2_Hourly$PPB = (df_HK_NO2_Hourly$ug_m3 * 24.45) / 46.0055
write.csv(df_HK_NO2_Hourly, "Hong_Kong/Hourly/HK_NO2_Hourly.csv", row.names = F)

##df_HK_CO$PPB = (df_HK_CO$ug_m3 * 24.45) / 28.01
##df_HK_CO_Daily = as.data.table(df_HK_CO)[, mean(ug_m3), by = .(location, city, Date)]
df_HK_NO2_Daily = subset(df_HK_NO2, select = c(location, value, Date, Time))
df_HK_NO2_Daily = as.data.table(df_HK_NO2_Daily)[, mean(value), by = .(location, Date)]
colnames(df_HK_NO2_Daily)[colnames(df_HK_NO2_Daily) == "V1"] <- "ug_m3"
df_HK_NO2_Daily$PPB = (df_HK_NO2_Daily$ug_m3 * 24.45) / 46.0055
write.csv(df_HK_NO2_Daily, "Hong_Kong/Daily/HK_NO2_Daily.csv", row.names = F)

df_HK_NO2_Monthly = subset(df_HK_NO2, select = c(location, value, Date, Time))
df_HK_NO2_Monthly = as.data.table(df_HK_NO2_Monthly)[, mean(value), by = .(location, month(as.Date(Date)))]
colnames(df_HK_NO2_Monthly)[colnames(df_HK_NO2_Monthly) == "V1"] <- "ug_m3"
Month_NO2 <- mapvalues(df_HK_NO2_Monthly$month, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))
df_HK_NO2_Monthly$PPB = (df_HK_NO2_Monthly$ug_m3 * 24.45) / 46.0055
df_HK_NO2_Monthly$Date = Month_NO2
write.csv(df_HK_NO2_Monthly, "Hong_Kong/Monthly/HK_NO2_Monthly.csv", row.names = F)





df_HK_O3 = read.csv("Raw Files/Hong Kong/O3.csv")
df_HK_O3$Date = substr(df_HK_O3$local, 1, 10)
df_HK_O3$Time = substr(df_HK_O3$local, 12, 16)
df_HK_O3_Hourly = subset(df_HK_O3, select = c(location, value, Date, Time))
colnames(df_HK_O3_Hourly)[colnames(df_HK_O3_Hourly) == "value"] <- "ug_m3"
df_HK_O3_Hourly$PPB = (df_HK_O3_Hourly$ug_m3 * 24.45) / 48
write.csv(df_HK_O3_Hourly, "Hong_Kong/Hourly/HK_O3_Hourly.csv", row.names = F)

##df_HK_CO$PPB = (df_HK_CO$ug_m3 * 24.45) / 28.01
##df_HK_CO_Daily = as.data.table(df_HK_CO)[, mean(ug_m3), by = .(location, city, Date)]
df_HK_O3_Daily = subset(df_HK_O3, select = c(location, value, Date, Time))
df_HK_O3_Daily = as.data.table(df_HK_O3_Daily)[, mean(value), by = .(location, Date)]
colnames(df_HK_O3_Daily)[colnames(df_HK_O3_Daily) == "V1"] <- "ug_m3"
df_HK_O3_Daily$PPB = (df_HK_O3_Daily$ug_m3 * 24.45) / 48
write.csv(df_HK_O3_Daily, "Hong_Kong/Daily/HK_O3_Daily.csv", row.names = F)

df_HK_O3_Monthly = subset(df_HK_O3, select = c(location, value, Date, Time))
df_HK_O3_Monthly = as.data.table(df_HK_O3_Monthly)[, mean(value), by = .(location, month(as.Date(Date)))]
colnames(df_HK_O3_Monthly)[colnames(df_HK_O3_Monthly) == "V1"] <- "ug_m3"
Month_O3 <- mapvalues(df_HK_O3_Monthly$month, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))
df_HK_O3_Monthly$PPB = (df_HK_O3_Monthly$ug_m3 * 24.45) / 48
df_HK_O3_Monthly$Date = Month_O3
write.csv(df_HK_O3_Monthly, "Hong_Kong/Monthly/HK_O3_Monthly.csv", row.names = F)


df_HK_PM2 = read.csv("Raw Files/Hong Kong/PM2_5.csv")
df_HK_PM2$Date = substr(df_HK_PM2$local, 1, 10)
df_HK_PM2$Time = substr(df_HK_PM2$local, 12, 16)
df_HK_PM2_Hourly = subset(df_HK_PM2, select = c(location, value, Date, Time))
colnames(df_HK_PM2_Hourly)[colnames(df_HK_PM2_Hourly) == "value"] <- "ug_m3"
df_HK_PM2_Hourly$PPB = (df_HK_PM2_Hourly$ug_m3 * 24.45) / 52.66
write.csv(df_HK_PM2_Hourly, "Hong_Kong/Hourly/HK_PM2_Hourly.csv", row.names = F)

##df_HK_CO$PPB = (df_HK_CO$ug_m3 * 24.45) / 28.01
##df_HK_CO_Daily = as.data.table(df_HK_CO)[, mean(ug_m3), by = .(location, city, Date)]
df_HK_PM2_Daily = subset(df_HK_PM2, select = c(location, value, Date, Time))
df_HK_PM2_Daily = as.data.table(df_HK_PM2_Daily)[, mean(value), by = .(location, Date)]
colnames(df_HK_PM2_Daily)[colnames(df_HK_PM2_Daily) == "V1"] <- "ug_m3"
df_HK_PM2_Daily$PPB = (df_HK_PM2_Daily$ug_m3 * 24.45) / 52.66
write.csv(df_HK_PM2_Daily, "Hong_Kong/Daily/HK_PM2_Daily.csv", row.names = F)

df_HK_PM2_Monthly = subset(df_HK_PM2, select = c(location, value, Date, Time))
df_HK_PM2_Monthly = as.data.table(df_HK_PM2_Monthly)[, mean(value), by = .(location, month(as.Date(Date)))]
colnames(df_HK_PM2_Monthly)[colnames(df_HK_PM2_Monthly) == "V1"] <- "ug_m3"
Month_PM2 <- mapvalues(df_HK_PM2_Monthly$month, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))
df_HK_PM2_Monthly$PPB = (df_HK_PM2_Monthly$ug_m3 * 24.45) / 52.66
df_HK_PM2_Monthly$Date = Month_PM2
write.csv(df_HK_PM2_Monthly, "Hong_Kong/Monthly/HK_PM2_Monthly.csv", row.names = F)



df_HK_PM10 = read.csv("Raw Files/Hong Kong/PM10.csv")
df_HK_PM10$Date = substr(df_HK_PM10$local, 1, 10)
df_HK_PM10$Time = substr(df_HK_PM10$local, 12, 16)
df_HK_PM10_Hourly = subset(df_HK_PM10, select = c(location, value, Date, Time))
colnames(df_HK_PM10_Hourly)[colnames(df_HK_PM10_Hourly) == "value"] <- "ug_m3"
df_HK_PM10_Hourly$PPB = (df_HK_PM10_Hourly$ug_m3 * 24.45) / 52.66
write.csv(df_HK_PM10_Hourly, "Hong_Kong/Hourly/HK_PM10_Hourly.csv", row.names = F)

##df_HK_CO$PPB = (df_HK_CO$ug_m3 * 24.45) / 28.01
##df_HK_CO_Daily = as.data.table(df_HK_CO)[, mean(ug_m3), by = .(location, city, Date)]
df_HK_PM10_Daily = subset(df_HK_PM10, select = c(location, value, Date, Time))
df_HK_PM10_Daily = as.data.table(df_HK_PM10_Daily)[, mean(value), by = .(location, Date)]
colnames(df_HK_PM10_Daily)[colnames(df_HK_PM10_Daily) == "V1"] <- "ug_m3"
df_HK_PM10_Daily$PPB = (df_HK_PM10_Daily$ug_m3 * 24.45) / 52.66
write.csv(df_HK_PM10_Daily, "Hong_Kong/Daily/HK_PM10_Daily.csv", row.names = F)

df_HK_PM10_Monthly = subset(df_HK_PM10, select = c(location, value, Date, Time))
df_HK_PM10_Monthly = as.data.table(df_HK_PM10_Monthly)[, mean(value), by = .(location, month(as.Date(Date)))]
colnames(df_HK_PM10_Monthly)[colnames(df_HK_PM10_Monthly) == "V1"] <- "ug_m3"
Month_PM10 <- mapvalues(df_HK_PM10_Monthly$month, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))
df_HK_PM10_Monthly$PPB = (df_HK_PM10_Monthly$ug_m3 * 24.45) / 52.66
df_HK_PM10_Monthly$Date = Month_PM10
write.csv(df_HK_PM10_Monthly, "Hong_Kong/Monthly/HK_PM10_Monthly.csv", row.names = F)



df_HK_SO2 = read.csv("Raw Files/Hong Kong/SO2.csv")
df_HK_SO2$Date = substr(df_HK_SO2$local, 1, 10)
df_HK_SO2$Time = substr(df_HK_SO2$local, 12, 16)
df_HK_SO2_Hourly = subset(df_HK_SO2, select = c(location, value, Date, Time))
colnames(df_HK_SO2_Hourly)[colnames(df_HK_SO2_Hourly) == "value"] <- "ug_m3"
df_HK_SO2_Hourly$PPB = (df_HK_SO2_Hourly$ug_m3 * 24.45) / 64.066
write.csv(df_HK_SO2_Hourly, "Hong_Kong/Hourly/HK_SO2_Hourly.csv", row.names = F)

##df_HK_CO$PPB = (df_HK_CO$ug_m3 * 24.45) / 28.01
##df_HK_CO_Daily = as.data.table(df_HK_CO)[, mean(ug_m3), by = .(location, city, Date)]
df_HK_SO2_Daily = subset(df_HK_SO2, select = c(location, value, Date, Time))
df_HK_SO2_Daily = as.data.table(df_HK_SO2_Daily)[, mean(value), by = .(location, Date)]
colnames(df_HK_SO2_Daily)[colnames(df_HK_SO2_Daily) == "V1"] <- "ug_m3"
df_HK_SO2_Daily$PPB = (df_HK_SO2_Daily$ug_m3 * 24.45) / 64.066
write.csv(df_HK_SO2_Daily, "Hong_Kong/Daily/HK_SO2_Daily.csv", row.names = F)

df_HK_SO2_Monthly = subset(df_HK_SO2, select = c(location, value, Date, Time))
df_HK_SO2_Monthly = as.data.table(df_HK_SO2_Monthly)[, mean(value), by = .(location, month(as.Date(Date)))]
colnames(df_HK_SO2_Monthly)[colnames(df_HK_SO2_Monthly) == "V1"] <- "ug_m3"
Month_SO2 <- mapvalues(df_HK_SO2_Monthly$month, c("3", "2", "1", "12"), c("2019-03-01", "2019-02-01", "2019-01-01", "2018-12-01"))
df_HK_SO2_Monthly$PPB = (df_HK_SO2_Monthly$ug_m3 * 24.45) / 64.066
df_HK_SO2_Monthly$Date = Month_SO2
write.csv(df_HK_SO2_Monthly, "Hong_Kong/Monthly/HK_SO2_Monthly.csv", row.names = F)


#####################################################################################################################################

months = c(1:12)

for(i in months)
{
  if (i < 10)
  {
  	file_name_PM2 = paste0("Hourly/PM2_5/PM2_5_0",i,".csv")
  	file_name_SO2 = paste0("Hourly/SO2/SO2_0",i,".csv")
  	file_name_NO2 = paste0("Hourly/NO2/NO2_0",i,".csv")
  	file_name_CO = paste0("Hourly/CO/CO_0",i,".csv")
  	file_name_PM10 = paste0("Hourly/PM10/PM10_0",i,".csv")
  	file_name_O3 = paste0("Hourly/Ozone/Ozone_0",i,".csv")
  	file_name = paste0("Daily/Map/0",i,".csv")
    df_PM2 = read.csv(file_name_PM2)
    df_SO2 = read.csv(file_name_SO2)
    df_NO2 = read.csv(file_name_NO2)
    df_CO = read.csv(file_name_CO)
    df_PM10 = read.csv(file_name_PM10)
    df_O3 = read.csv(file_name_O3)
  } else {
  	file_name_PM2 = paste0("Hourly/PM2_5/PM2_5_",i,".csv")
  	file_name_SO2 = paste0("Hourly/SO2/SO2_",i,".csv")
  	file_name_NO2 = paste0("Hourly/NO2/NO2_",i,".csv")
  	file_name_CO = paste0("Hourly/CO/CO_",i,".csv")
  	file_name_PM10 = paste0("Hourly/PM10/PM10_",i,".csv")
  	file_name_O3 = paste0("Hourly/Ozone/Ozone_",i,".csv")
  	file_name = paste0("Daily/Map/",i,".csv")
  	df_PM2 = read.csv(file_name_PM2)
    df_SO2 = read.csv(file_name_SO2)
    df_NO2 = read.csv(file_name_NO2)
    df_CO = read.csv(file_name_CO)
    df_PM10 = read.csv(file_name_PM10)
    df_O3 = read.csv(file_name_O3)
  }

df_PM2_Monthly = as.data.table(df_PM2)[, mean(PPM), by = .(State.Name, County.Name, Date.Local)]
df_SO2_Monthly = as.data.table(df_SO2)[, mean(PPM), by = .(State.Name, County.Name, Date.Local)]
df_NO2_Monthly = as.data.table(df_NO2)[, mean(PPM), by = .(State.Name, County.Name, Date.Local)]
df_CO_Monthly = as.data.table(df_CO)[, mean(PPM), by = .(State.Name, County.Name, Date.Local)]
df_PM10_Monthly = as.data.table(df_PM10)[, mean(PPM), by = .(State.Name, County.Name, Date.Local)]
df_O3_Monthly = as.data.table(df_O3)[, mean(PPM), by = .(State.Name, County.Name, Date.Local)]

df_PM2_Monthly$Pollutant = "PM2.5"
df_SO2_Monthly$Pollutant = "SO2"
df_NO2_Monthly$Pollutant = "NO2"
df_CO_Monthly$Pollutant = "CO"
df_PM10_Monthly$Pollutant = "PM10"
df_O3_Monthly$Pollutant = "O3"

df = rbind(df_PM2_Monthly, df_SO2_Monthly, df_NO2_Monthly, df_CO_Monthly, df_PM10_Monthly, df_O3_Monthly)
write.csv(df, file_name, row.names = F)
}
