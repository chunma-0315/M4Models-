
#set work direction 
setwd("/Users/au657046/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/")
#load packages 
library(tidyverse)
library(lubridate)
#general farm infomations 
VS_pig_year<-158
VS_cat_year<-(1870+530)/2
pig_number<-3395
cat_number<-168*2
#parameters 
Ea<-80900
#TOC/VS
TOC_VS<-0.45
#CH4:(CH4+CO2)
CH4_CH_CO2<-0.25

# DK_lnA
lnA_pig_dk_barn<-30.31
lnA_cat_dk_barn<-29.07
lnA_pig_dk_store<-29.46
lnA_cat_dk_store<-28.61
#loading slurry temp files for pig farms 
# daily removal
setwd("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/STM_pig")
df.p1.dk<-read.csv("DK/0001_temp.csv",skip = 2)
str(df.p1.dk)
df.p1.se<-read.csv("SE/0001_temp.csv",skip = 2)
df.p1.de<-read.csv("DE/0001_temp.csv",skip = 2)
df.p1.nl<-read.csv("NL/0001_temp.csv",skip = 2)
# 7 days removal
setwd("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/STM_pig_7")
df.p7.dk<-read.csv("DK/0001_temp.csv",skip = 2)
str(df.p7.dk)
df.p7.se<-read.csv("SE/0001_temp.csv",skip = 2)
df.p7.de<-read.csv("DE/0001_temp.csv",skip = 2)
df.p7.nl<-read.csv("NL/0001_temp.csv",skip = 2)
# 40 days removal
setwd("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/STM_pig_40")
df.p40.dk<-read.csv("DK/0001_temp.csv",skip = 2)
str(df.p40.dk)
df.p40.se<-read.csv("SE/0001_temp.csv",skip = 2)
df.p40.de<-read.csv("DE/0001_temp.csv",skip = 2)
df.p40.nl<-read.csv("NL/0001_temp.csv",skip = 2)

#buid a dataframe for the calculation
dos<-rep(seq(1,1095,1),1)
df.1<-as.data.frame(dos)
df.1$date<-seq(as.Date('2019-5-01'),by='days',length=nrow(df.1))
df.1$month<-month(df.1$date)
df.1$day<-day(df.1$date)
df.1$removal<-c(1)
df.1$doy<-yday(df.1$date)
str(df.1)
sc<-c(1,7,40) 

## creat a list with removal frequency of 1, 7 and 40 days 

##DK pig####

removal_dk_pig <- lapply(sc,function(i,df=df.1){
  
  df$removal <- df$removal* as.numeric(i)
  
  list(paste('DK_pig_lnA',i,sep = "_"),df)
  
})
#
run_in_folder <- function(i) {
  #browser()
  
  # Make a folder with scenario ID name
  folder_name <-
    paste(
      "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/methane model/",
      i[[1]],
      sep = ""
    )
  
  dir.create(folder_name)
  
  # set the working 
  setwd(folder_name)
  #  # Write data csv
  #
  write.table(as.data.frame(i[[2]]), 
              paste( "removal.csv", sep = ""), 
              sep ="\t",
              dec = ".",
              row.names = FALSE,
              quote=FALSE)
  df.1<-read.table("removal.csv",header = T, sep ="\t")
  str(df.1)
  
  #  times of removal
  df.1$day_barn<-rep(seq(1,as.numeric(df.1$removal[1]),1), length.out=nrow(df.1))
  df.1$barn_temp <- 19.5
  
  # barn emissions 
  for (i in 1:dim(df.1)[1]) {
    if (df.1$day_barn[i] ==1) {
      
      df.1$VS_barn[i] <- VS_pig_year/365*df.1$day_barn[i]
      df.1$CH4[i]<-exp(lnA_pig_dk_barn-Ea/(8.314*(273+df.1$barn_temp[i])))*df.1$VS_barn[i]
      df.1$VS_loss[i]<-(0.75*df.1$CH4[i]/1000/CH4_CH_CO2)/TOC_VS
      df.1$ch4_m3[i]<-(df.1$CH4[i]/16*0.082*(273.15+df.1$barn_temp[i]))/1000
    } 
    
    else {
      
      df.1$VS_barn[i] <- VS_pig_year/365+df.1$VS_barn[i-1]-df.1$VS_loss[i-1]  
      df.1$CH4[i]<-exp(lnA_pig_dk_barn-Ea/(8.314*(273+df.1$barn_temp[i])))*df.1$VS_barn[i]
      df.1$VS_loss[i]<-(0.75*df.1$CH4[i]/1000/CH4_CH_CO2)/TOC_VS
      df.1$ch4_m3[i]<-(df.1$CH4[i]/16*0.082*(273.15+df.1$barn_temp[i]))/1000
    } 
    
  }
  
  
  #field application-95% removal 
  for (i in 1:dim(df.1)[1]) {
    if (df.1$month[i] ==4&df.1$day[i]==1) {
      df.1$output[i]<-0.95
      
      
    }  else{
      df.1$output[i]<-0
      
    }
    
  }
  # get slurry temperature
  for (i in 1:dim(df.1)[1]) {
    if (df.1$removal[1] ==1) {
      df.1$temp<-df.p1.dk$slurry_temp
      
    } else if (df.1$removal[1] ==7) {
      df.1$temp<-df.p7.dk$slurry_temp
      
    } else if (df.1$removal[1] ==40) {
      df.1$temp<-df.p40.dk$slurry_temp
      
    }
    
  }
  #emission from outside store
  
  for (i in 1:dim(df.1)[1]) {
    if(df.1$dos[i] <= df.1$removal[1]) {
      
      df.1$VS_store[i]<-0
      df.1$CH4_store[i]<-exp(lnA_pig_dk_store-Ea/(8.314*(273+df.1$temp[i])))*df.1$VS_store[i]
      df.1$VS_loss_store[i]<-(0.75*df.1$CH4_store[i]/1000/CH4_CH_CO2)/TOC_VS
      df.1$CH4_store_m3[i]<-(df.1$CH4_store[i]/16*0.082*(273+df.1$temp[i]))/1000
      
    }  else if(df.1$day_barn[i]==1&df.1$dos[i]>1) {  
      df.1$VS_store[i]<-df.1$VS_barn[i-1]+df.1$VS_store[i-1]-df.1$VS_store[i-1]*df.1$output[i]
      df.1$CH4_store[i]<-exp(lnA_pig_dk_store-Ea/(8.314*(273+df.1$temp[i])))*df.1$VS_store[i]
      df.1$VS_loss_store[i]<-(0.75*df.1$CH4_store[i]/1000/CH4_CH_CO2)/TOC_VS 
      df.1$CH4_store_m3[i]<-(df.1$CH4_store[i]/16*0.082*(273+df.1$temp[i]))/1000
    }else if(df.1$day_barn[i]!=1&df.1$dos[i]>df.1$removal[1]) {df.1$VS_store[i]<-df.1$VS_store[i-1]-df.1$VS_loss_store[i-1]-df.1$VS_store[i-1]*df.1$output[i]
    
    df.1$CH4_store[i]<-exp(lnA_pig_dk_store-Ea/(8.314*(273+df.1$temp[i])))*df.1$VS_store[i]
    df.1$VS_loss_store[i]<-(0.75*df.1$CH4_store[i]/1000/CH4_CH_CO2)/TOC_VS
    df.1$CH4_store_m3[i]<-(df.1$CH4_store[i]/16*0.082*(273+df.1$temp[i]))/1000
    
    
    } 
  }
  
  
  ##
  
  
  write.csv(df.1,"ch4.csv",row.names=FALSE)
}

lapply(removal_dk_pig
       , run_in_folder)

#output data

out_func <- function(i) {
  
  #browser()
  
  loc_folder <- "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/methane model/"
  
  #Read 
  
  slurry1 <- #read.table(
    read.csv(
      paste(loc_folder,#gsub(" ","",
            i[[1]],
            #), 
            "/ch4.csv",
            sep=""))
  
  slurry1 <- slurry1 %>% mutate("scn"=i[[1]])
  
  slurry1
  
}

out_list <- lapply(removal_dk_pig, out_func)

dk.pig.1 <- do.call(rbind, out_list)
str(dk.pig.1)

dk.pig.1$date<-as.Date(dk.pig.1$date,"%Y-%m-%d")

#


## sum annually emission
for (i in 1:dim(dk.pig.1)[1]) {
  if (dk.pig.1$date[i] <=as.Date("2020-05-01")) {
    dk.pig.1$year[i]<-1
    
    
  }  else if (dk.pig.1$date[i]>as.Date("2020-05-01")&dk.pig.1$date[i] <=as.Date("2021-05-01")) {
    dk.pig.1$year[i]<-2
  } else if (dk.pig.1$date[i]>as.Date("2021-05-01")) {
    dk.pig.1$year[i]<-3
  }
}

aggregate(dk.pig.1$CH4,by=list(dk.pig.1$scn,dk.pig.1$year),sum)
aggregate(dk.pig.1$CH4_store,by=list(dk.pig.1$scn,dk.pig.1$year),sum)