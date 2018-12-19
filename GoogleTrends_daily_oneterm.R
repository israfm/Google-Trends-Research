library(lubridate)
library(gtrendsR)
library(dplyr)

keyword<-"Trump"
country<-"US"

#Definition of range times
  new_date_1<-Sys.Date()-1
  old_date_1<-as.Date(new_date_1) %m-% months(8)
  time_range_1<-paste0(as.character(old_date_1)," ",as.character(new_date_1))
  ref_date<-new_date_1
  ranges<-c(time_range_1)
  
  for (i in (2:8)){
    new_date<-as.Date(ref_date) %m-% months(7)
    old_date<-as.Date(new_date) %m-% months(8)
    time_range<-paste0(as.character(old_date)," ",as.character(new_date))
    ranges<-c(ranges,time_range)
    ref_date<-new_date
  }

#Extraction of google-trends data
  all_data<-data.frame()
  ID<-1
  for (j in ranges){
    data<-data.frame(gtrends(c(keyword),geo = c(country),time = j)$interest_over_time)[,1:2]
    data$ID<-ID
    all_data<-rbind(all_data,data)
    ID<-ID+1
  }
  
#Definition of function to get the intermediate curve between 2 trends of different periods with one month in common
  intergen<- function(df,ID,hits,newID){
    k<-min(df$ID)
    new_date_c<-max(subset(df$date,df$ID==k+1))
    old_date_c<-min(subset(df$date,df$ID==k))
    
    avg_1<- mean(subset(df$hits,df$ID==k & df$date<=new_date_c))
    avg_2<- mean(subset(df$hits,df$ID==k+1 & df$date>=old_date_c))
    
    ratiofor1<- avg_2/avg_1
    ratiofor2<- avg_1/avg_2
    
    new_data_1<-subset(df,df$ID==k & df$date>new_date_c)
    new_data_1$hits<-round(new_data_1$hits*ratiofor1)
    two_onetrans<-rbind(subset(df,df$ID==k+1),new_data_1)
    
    new_data_2<-subset(df,df$ID==k+1 & df$date<old_date_c)
    new_data_2$hits<-round(new_data_2$hits*ratiofor2)
    one_twotrans<-rbind(subset(df,df$ID==k),new_data_2)
    
    
    output_df <- 
      inner_join(two_onetrans,one_twotrans,by="date",copy=FALSE) %>% 
      select("date","hits.x","hits.y") %>% 
      mutate(hits=round(hits.x+((hits.y-hits.x)/2))) %>% 
      mutate(ID=newID) %>% 
      select ("date","hits","ID")
    return(output_df)
  }

#Definition for function for each iteration
  iteration<-function(data){
    iteration_df<-data.frame()
    IDlist<-seq(1, max(data$ID), 2)
    newID<-1
    for (n in IDlist){
      df<-subset(data,data$ID==n | data$ID==n+1)
      iteration_df<-rbind(iteration_df,intergen(df,ID,hits,newID))
      newID<-newID+1
    }
    return(iteration_df)
  }
 
#Iterate 3 times  
 final<-all_data
  for (i in 1:3)  {final<-iteration(final)}
  
