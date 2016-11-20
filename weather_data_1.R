install.packages("ggmap")
install.packages("jsonlite")
install.packages("XML")
install.packages("lubridate")
install.packages("data.table")
install.packages("chron")
install.packages("weatherData")
library(lubridate)
library(dplyr)
library(data.table)
library(XML)
library(jsonlite)
library(ggmap)
library(plyr)
library(chron)
library(weatherData)
setwd('/Users/hinagandhi/desktop/midterm')
finAddress<- read.csv("Finland_addresses_area.csv")

#View(finAddress)
colnames(finAddress)[colnames(finAddress)=="X..address"] <- "Address"
colnames(finAddress)[colnames(finAddress)=="area_floor._m.sqr"] <- "FloorArea_mSqr"

finAddress$Address <- paste0(finAddress$Address, ", Finland")
address <- finAddress$Address
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address,source = "google", output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  
  #return Na's if we didn't get a match, checking errorneous input:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}
#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!


# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(address))){
  print(paste("Working on index", ii, "of", length(address)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(address[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
}

#now we add the latitude and longitude to the main data
finAddress$lat <- geocoded$lat
finAddress$long <- geocoded$long
colnames(finAddress)[colnames(finAddress)=="long"] <- "Longitude"
colnames(finAddress)[colnames(finAddress)=="lat"] <- "Latitude"
View(finAddress)
#finding nearest Airport, done using REST API call and parsing the xml file
apiUrl<-"http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query="
for(i in seq(from=1, to=33)){
  lat<-finAddress$Latitude[i]
  lng<-finAddress$Longitude[i]
  url<-paste0(apiUrl,lat,',',lng)
  data <- xmlParse(url)
  dataDictionary <- xmlToDataFrame(getNodeSet(data,"//station"))  
  dataDictionary<- dataDictionary[,-7:-10]
  dataDictionary<-dataDictionary[dataDictionary$icao!="",]
  dataDictionary$lat<-as.numeric(dataDictionary$lat)
  dataDictionary$lon<-as.numeric(dataDictionary$lon)
  loc <- c(lat,lng)
  dists <- geosphere::distHaversine(c(lng,lat), dataDictionary[, c('lon', 'lat')])
  finAddress$nearestAirport[i]<-dataDictionary[which.min(dists), ]$icao
}
View(finAddress)
uniqueAirport<-unique(finAddress[c("nearestAirport")])
#class(uniqueAirport)
obj_name<-NULL
#create different dataframes for unique airports
install.packages("weatherData")
library(weatherData)
install.packages("devtools")
library("devtools")
install_github("Ram-N/weatherData")
for(i in seq(from=1,to=nrow(uniqueAirport))){
  obj_name[i] <- paste('data', i, sep ='_')
  input<- getWeatherForDate(uniqueAirport[i,],start_date="2013-01-01",
                            end_date = "2013-12-08",
                            opt_detailed=T, opt_all_columns=T)
  input$stationCode<-uniqueAirport[i,]
  assign(obj_name[i],input)
  write.csv(input,paste(obj_name[i],'.csv',sep = ""))
}
data_EFHK<-data_1
data_EFHF<-data_2
data_1<-data_EFHK
data_2<-data_EFHF
View(data_1)
View(data_2)
#separating date column from Time
data_1$Date <- as.Date(as.POSIXlt(data_1$Time))
#handling amy missing entry in Time, if TimeEET is present then the UTC difference is of 2 hours, if TimeEEST is present then the difference is 3 hours from UTC
for(i in seq(from=1,to=nrow(data_1))){
  if(is.na(data_1$Time[i]) & !is.na(data_1$DateUTC[i])){
    data_1$Time[i]<-format(data_1$DateUTC[i])
    data_1$Date[i]<-as.Date(as.POSIXlt(data_1$Time[i]))
  }
}
#handle for any missing entry in TimeEET
for(i in seq(from=1,to=nrow(data_1))){
  if(is.na(data_1$TimeEET[i]) & !is.na(data_1$TimeEEST[i])){
    data_1$TimeEET[i]<-data_1$TimeEEST[i]
  }
}

#handling column type
data_1$Wind_SpeedMPH<- as.numeric(data_1$Wind_SpeedMPH)
#handling "calm" string in Wind_SpeedMPH to 0 assuming there is no measurable wind that time, before that minimum is 1.2
for(i in seq(from=1,to=nrow(data_1))){
  if(is.na(data_1$Wind_SpeedMPH[i])){
    data_1$Wind_SpeedMPH[i]<-0
  }
}
data_1$TemperatureF <- as.numeric(data_1$TemperatureF)
data_1$Dew_PointF<-as.numeric(data_1$Dew_PointF)
data_1$Sea_Level_PressureIn<-as.numeric(data_1$Sea_Level_PressureIn)
data_1$VisibilityMPH<-as.numeric(data_1$VisibilityMPH)
data_1$Humidity<-as.numeric(data_1$Humidity)
data_1$Gust_SpeedMPH<-as.numeric(data_1$Gust_SpeedMPH)
data_1$WindDirDegrees<-as.numeric(data_1$WindDirDegrees)
data_1$TimeEET <- as.numeric(data_1$TimeEET)
time=format(as.POSIXct(data_1$Time, format="%Y-%m-%d %H:%M:%S"), format="%H")
data_1$Hour <- as.numeric(time)
data_1$Date <- as.character(data_1$Date)
data_1 <- data_1[c("Date","Hour","TemperatureF","Dew_PointF","Humidity","Sea_Level_PressureIn","VisibilityMPH","Wind_SpeedMPH","WindDirDegrees","Gust_SpeedMPH","Conditions","Wind_Direction","PrecipitationIn","Events")]
View(data_1)
#removing NAs and outliers
for(j in 3:14)
{
  if(is.na(data_1[1,j]) | data_1[1,j]==-9999 | data_1[1,j]=="")
  {
    data_1[1,j] <- 0
  }
}
# removing na from row to nrows
for(j in 3:14)
{
  for(i in 2:nrow(data_1)) {
    if(is.na(data_1[i,j]) | data_1[i,j]==-9999 | data_1[i,j]=="")
    {
      data_1[i,j] <- data_1[i-1,j]
    }
  }
} 

write.csv(data_1,"newdata_1.csv")

newdf <- data_1
asdkj<- newdf[, c("Hour","Date","Conditions","Wind_Direction","Events")]
#asdkj <-mutate(group_by(asdkj, Date, Hour), 
 #              Conditions = replace(as.character(Conditions), !complete.cases(Conditions), "") ,Wind_Direction = replace(as.character(Wind_Direction), !complete.cases(Wind_Direction), ""),Events=replace(as.character(Events),!complete.cases(Events),"")) %>%
  #summarise(Conditions = paste(Conditions, collapse = " / "),Wind_Direction= paste(Wind_Direction, collapse = " / "),Events=paste(Events,collapse=" / "))
library(dplyr)
asdkj <- asdkj %>% 
  group_by_(~Date, ~Hour) %>%
  summarize_(Conditions=~paste(Conditions, collapse=' '),Wind_Direction=~paste(Wind_Direction, collapse=' '),Events=~paste(Events, collapse=' '))
View(asdkj)
# aggregating data by hour and date
agg <- aggregate(data_1[, c(3,4,5,6,7,8,9,10)], by = list(data_1$Date, data_1$Hour), mean, na.rm = F)
colnames(agg)[colnames(agg)=="Group.1"] <- "Date"
colnames(agg)[colnames(agg)=="Group.2"] <- "Hour"
agg <- agg[order(agg$Date, agg$Hour),]
data_1 <- left_join(agg,asdkj, by = c("Date"="Date", "Hour"= "Hour"))
data_1$nearestAirport<-"EFHK"
View(data_1)

write.csv(data_1,"cleandata_1.csv")

#################################################################Clean data_2
data_2$Date <- as.Date(as.POSIXlt(data_2$Time))
#handling amy missing entry in Time, if TimeEET is present then the UTC difference is of 2 hours, if TimeEEST is present then the difference is 3 hours from UTC
for(i in seq(from=1,to=nrow(data_2))){
  if(is.na(data_2$Time[i]) & !is.na(data_2$DateUTC[i])){
    data_2$Time[i]<-format(data_2$DateUTC[i])
    data_2$Date[i]<-as.Date(as.POSIXlt(data_2$Time[i]))
  }
}
#handle for any missing entry in TimeEET
for(i in seq(from=1,to=nrow(data_2))){
  if(is.na(data_2$TimeEET[i]) & !is.na(data_2$TimeEEST[i])){
    data_2$TimeEET[i]<-data_2$TimeEEST[i]
  }
}

#handling column type
data_2$Wind_SpeedMPH<- as.numeric(data_2$Wind_SpeedMPH)
#handling "calm" string in Wind_SpeedMPH to 0 assuming there is no measurable wind that time, before that minimum is 1.2
for(i in seq(from=1,to=nrow(data_2))){
  if(is.na(data_2$Wind_SpeedMPH[i])){
    data_2$Wind_SpeedMPH[i]<-0
  }
}
data_2$TemperatureF <- as.numeric(data_2$TemperatureF)
data_2$Dew_PointF<-as.numeric(data_2$Dew_PointF)
data_2$Sea_Level_PressureIn<-as.numeric(data_2$Sea_Level_PressureIn)
data_2$VisibilityMPH<-as.numeric(data_2$VisibilityMPH)
data_2$Humidity<-as.numeric(data_2$Humidity)
data_2$Gust_SpeedMPH<-as.numeric(data_2$Gust_SpeedMPH)
data_2$WindDirDegrees<-as.numeric(data_2$WindDirDegrees)
time=format(as.POSIXct(data_2$Time, format="%Y-%m-%d %H:%M:%S"), format="%H")
data_2$Hour <- as.numeric(time)
data_2$Date <- as.character(data_2$Date)
data_2 <- data_2[c("Date","Hour","TemperatureF","Dew_PointF","Humidity","Sea_Level_PressureIn","VisibilityMPH","Wind_SpeedMPH","WindDirDegrees","Gust_SpeedMPH","Conditions","Wind_Direction","PrecipitationIn","Events")]
View(data_2)
#removing NAs and outliers
for(j in 3:14)
{
  if(is.na(data_2[1,j]) | data_2[1,j]==-9999 | data_2[1,j]=="")
  {
    data_2[1,j] <- 0
  }
}
# removing na from row to nrows
for(j in 3:14)
{
  for(i in 2:nrow(data_2)) {
    if(is.na(data_2[i,j]) | data_2[i,j]==-9999 | data_2[i,j]=="")
    {
      data_2[i,j] <- data_2[i-1,j]
    }
  }
} 

write.csv(data_2,"newdata_2.csv")

newdf <- data_2
asdkj<- newdf[, c("Hour","Date","Conditions","Wind_Direction","Events")]
asdkj <- asdkj %>% 
  group_by_(~Date, ~Hour) %>%
  summarize_(Conditions=~paste(Conditions, collapse=' '),
  Wind_Direction=~paste(Wind_Direction, collapse=' '),Events=~paste(Events, collapse=' '))
# aggregating data by hour and date
agg <- aggregate(data_2[, 3:10], by = list(data_2$Date, data_2$Hour), mean, na.rm = F)
colnames(agg)[colnames(agg)=="Group.1"] <- "Date"
colnames(agg)[colnames(agg)=="Group.2"] <- "Hour"
agg <- agg[order(agg$Date, agg$Hour),]
data_2 <- left_join(agg,asdkj, by = c("Date"="Date", "Hour"= "Hour"))
data_2$nearestAirport<-"EFHF"
View(data_2)
View(data_1)
weather_data <- rbind(data_1, data_2)
################################################################
