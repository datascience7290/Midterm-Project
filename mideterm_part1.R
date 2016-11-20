install.packages("xts")
install.packages("lubridate")
library(dplyr)
library(xts)
library(chron)
library("XML")
library(lubridate)
setwd('/Users/hinagandhi/desktop/midterm')
data <- read.csv('Finland_masked.csv',header=T,sep=',')
data <- as.data.frame(data)
View(data)
consumption_data <- data[ which(data$type =='elect' | data$type == 'Dist_Heating'), ]
consumption_data <- read.csv("consumption_data_both.csv")
consumptionView_data <- as.data.frame(consumption_data)
View(consumption_data)
unique_building <- unique(consumption_data[c("BuildingID", "meternumb")])
unique_building <- unique_building[!(unique_building$BuildingID=="5288" & unique_building$meternumb==1),]
unique_building <- unique_building[!(unique_building$BuildingID=="5290" & unique_building$meternumb==8),]
unique_building <- unique_building[!(unique_building$BuildingID=="5290" & unique_building$meternumb==3),]
obj_name_power <- list()
list_dataframes <- list()
url.base <- "http://www.timeanddate.com/holidays/finland/"
year <- 2013
res <- readHTMLTable(paste(url.base, year, sep="/"))[[1]]
res <- res[!grepl("Season", res$V4),]
res <- paste(res$V1,year)
res_final <- parse_date_time(res, orders="mdy")
res_final <- gsub("UTC"," ",res_final)
base_hours <- c(0,1,2,3,4,22,23)
unique_dates <- unique(consumption_data[c("date")])
no_of_days <- nrow(unique_dates)*24
# creating dataframes
for(i in seq(from=1, to=nrow(unique_building)))
 {
  current_building_id <- unique_building[i,]$BuildingID
  current_meter_id <- unique_building[i,]$meternumb
  input <- NULL
  input<- as.data.frame(consumption_data[which(consumption_data$BuildingID==current_building_id & consumption_data$meternumb==current_meter_id),])
  input_new <- NULL
  x <- 0
  k <- 0
  if(nrow(input)!=no_of_days)
  {
    count <- 0
    for(j in seq(from=1, to=nrow(input),by=24))
    {
      if(j==1|j==(x+24-count))
      {
       if(input$hour[j-count] != 0 & !is.na(input$hour[j-count]))
        {new <- NULL
        new$BuildingID <- input$BuildingID[j]
        new$vac <- input$vac[j]
        new$meternumb <- input$meternumb[j]
        new$type <- input$type[j]
        new$date <- input$date[j]
        new$hour <- 0
        new$Consumption <-  0
        input_new <- rbind(input_new,new)
        x <- j
       }else
        {
          input_new <- rbind(input_new,input[j,])
          x <- j
        }  
      }
      for(k in 1:23)
      {
        if(input$hour[j+k-count] != k & !is.na(input$hour[j+k-count]))
        {
          new <- NULL
          mean <- mean(input$Consumption[j:(j+k)])
          new$BuildingID <- input$BuildingID[j+k]
          new$vac <- input$vac[j+k]
          new$meternumb <- input$meternumb[j+k]
          new$type <- input$type[j+k]
          new$date <- input$date[j+k]
          new$hour <- k
          new$Consumption <-  mean
          input_new <- rbind(input_new,new)
          count <- count + 1
        }else
        {
          input_new <- rbind(input_new,input[j+k-count,])
        }
      }
    }
  }
  if(is.null(input_new))
  {
    obj_name_power[[i]] <- input
  }else
  {
    obj_name_power[[i]] <- input_new
  }
}
# adding dates
all_buildings_dataframe <- NULL
for(i in seq(from=1, to=nrow(unique_building)))
{
  df_name <- NULL
  df_name <- as.data.frame(obj_name_power[[i]])
  df_name$date <- as.Date(as.character(df_name$date), "%Y%m%d")
  df_name$Year<-as.numeric(format(df_name$date, "%Y"))
  # extract month
  df_name$Month<-as.numeric(format(df_name$date, "%m"))
  #extract day
  df_name$Day<-as.numeric(format(df_name$date, "%d"))
  
  df_name <- df_name[order(df_name$date),]
  df_name["Day of Week"] <- day.of.week(df_name$Month,df_name$Day,df_name$Year)
  isWeekday <-!is.weekend(df_name$date)
  df_name$Weekday <- ifelse(isWeekday,1,0)
  df_name$date <- as.character(df_name$date)
  sundays_list <- df_name$`Day of Week` %in% 0
  isholiday <- (df_name$date %in% res_final) || (df_name$date %in% sundays_list)
  df_name$Holiday <- ifelse(isholiday,1,0)
  is_base_hour <- df_name$hour %in% base_hours
  df_name$Base_hour_Flag <- ifelse(is_base_hour,1,0)
  df_name$Consumption[is.na(df_name$Consumption)] <- 0
  df_name <- left_join(df_name,finAddress,by=c("vac"="building") )
  df_name$kwh_per_meter_sq <- df_name$Consumption/df_name$FloorArea_mSqr
  base_hours_usage <- as.data.frame(df_name[which(df_name$Base_hour_Flag==1),])
  agg <- aggregate(base_hours_usage[, c(8)], by = list(base_hours_usage$BuildingID, base_hours_usage$type, 
                                                       base_hours_usage$meternumb,base_hours_usage$Weekday,
                                                       base_hours_usage$Month,
                                                       base_hours_usage$Holiday), mean, na.rm = F)
  colnames(agg) <- c("BuildingID","type","meternumb","Weekday","Month","Holiday","base_hr_usage") 
  df_name <- left_join(df_name,agg, by = c("BuildingID"="BuildingID", "meternumb"= "meternumb","type"="type","Weekday"="Weekday","Month"="Month", "Holiday"="Holiday"))
  df_name$base_hr_class <- ifelse(df_name$Consumption > df_name$base_hr_usage,"HIGH","LOW")
  weather_data <- read.csv("weather_data.csv",header=T,sep=',')
  df_name <- left_join(df_name, weather_data, by= c("date"="Date","hour"="Hour","nearestAirport"="nearestAirport"))
  #for(k in 24:31)
  #{
  # if(is.na(df_name[1,k]))
   # {
    #  df_name[1,k] <- 0
   # }
 # }
  
# for(k in 24:31)
  #{
   # for(l in 2:nrow(df_name)) {
     # if(is.na(df_name[k,l]))
     # {
     #  df_name[k,l] <- df_name[k-1,l]
     # }
   # }
# }
  X <- paste0("buildingID_",unique_building$BuildingID[i],"_m_",unique_building$meternumb[i])
  assign(X,df_name)
  list_dataframes[[i]] <- df_name 
}

# sample check
write.csv(buildingID_5198_m_1,"buildingID_5198_m_1.csv")
# all buildings data csv
write.csv(all_buildings_dataframe,"all_buildings_dataframe.csv")
list_dataframes_x = list()
for(k in seq(from=1, to=nrow(unique_building)))
{
  k <- 1
  df_name_cleaned <- as.data.frame(list_dataframes[[k]])
  for(j in 24:31)
  {
    if(is.na(df_name_cleaned[1,j]))
    {
      df_name_cleaned[1,j] <- 0
    }
  }
  
  for(j in 24:32)
  {
    for(i in 2:nrow(df_name_cleaned)) {
      if(is.na(df_name_cleaned[i,j]))
      {
        df_name_cleaned[i,j] <- df_name_cleaned[i-1,j]
      }
    }
  }
  all_buildings_dataframe <- rbind(all_buildings_dataframe,df_name_cleaned)
  list_dataframes_x[[k]] <- df_name_cleaned
}
