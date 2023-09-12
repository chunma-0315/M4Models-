#work direction
setwd("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/")
#load packages 
library(tidyverse)
library(lubridate)


# get slurry levels -------------------------------------------------------


# try get a list for each country 

country<-c("DK","DE","SE","NL")
#pig_number nick used 3395, m4model 8600
#cattle 168 dairy and 168 heifers
pig_number<-c(3395,2680,7050,7650)
cat_number<-c(168*2,269,137,168)
number<-list(country,pig_number,cat_number)

#general info about farms 
VS_pig_year<-158
VS_cattle_year<-(1870+530)/2
VS_cattle<-0.06368
VS_pig<-0.0528
pig_slurry_year<-VS_pig_year/VS_pig/1000
cattle_slurry_year<-VS_cattle_year/VS_cattle/1000

#
## slurry field application in NL
month <- seq(1,12,1)
field_pig<-c(0,0,0,0.95,0,0,0,0,0,0,0,0)
field_cat<-c(0,0,0.95,0,0,0,0.95,0,0,0,0,0)
field_NL<-list(month,field_pig,field_cat)
## slurry field application in DK
field_cat1<-c(0,0,0,0.95,0,0,0.95,0,0,0,0,0)
field_dk<-list(month,field_pig,field_cat1)

## slurry field application in SE
field_cat_se<-c(0,0,0,0,0.95,0,0.95,0,0,0,0,0)
field_SE<-list(month,field_pig,field_cat_se)

## slurry field application in DE
field_cat_de<-c(0,0,0,0.95,0,0,0.95,0,0,0,0,0)
field_DE<-list(month,field_pig,field_cat_de)
## build dataframe for cattle
dos<-rep(seq(1,1095,1),1)
df.1<-as.data.frame(dos)
df.1$date<-seq(as.Date('2019-8-01'),by='days',length=nrow(df.1))
df.1$month<-month(df.1$date)
df.1$day<-day(df.1$date)
df.1$removal<-c(1)
df.1$doy<-yday(df.1$date)
str(df.1)
## test different slurry removal frequency (1, 7 and 40d)  
sc <- c(1,7,40)

removal_list <- lapply(sc,function(i,df=df.1){
  
  df$removal <- df$removal* as.numeric(i)
  
  list(paste('DK',i,sep = "_"),df)
  
})

#
run_in_folder <- function(i) {
 
  
  # Make a folder with scenario ID name
  folder_name <-
    paste(
      "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/level/",
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
  df.1$slurry_barn <- cattle_slurry_year*number[[3]][1]/365*df.1$day_barn
  #field application-empty 95% of the slurry in the tank
  for (i in 1:dim(df.1)[1]) {
    if (df.1$month[i] ==field_dk[[1]][4]&df.1$day[i]==1) {
      df.1$output[i]<-0.95
      
      
    }  
    else if (df.1$month[i] ==field_dk[[1]][7]&df.1$day[i]==1) {
      df.1$output[i]<-0.95
      
    } else{
      df.1$output[i]<-0
      
    }
    
  }
  #
  
  for (i in 1:dim(df.1)[1]) {
    if(df.1$dos[i] <= df.1$removal[1]) {
      
      df.1$slurry_store[i]<-0
      
    }  else if(df.1$day_barn[i]==1&df.1$dos[i]>df.1$removal[1]) {  
      df.1$slurry_store[i]<-df.1$slurry_barn[i-1]+df.1$slurry_store[i-1]-df.1$slurry_store[i-1]*df.1$output[i]
    }
    
    else if(df.1$day_barn[i]!=1&df.1$dos[i]>df.1$removal[1]) {
      df.1$slurry_store[i]<-df.1$slurry_store[i-1]-df.1$slurry_store[i-1]*df.1$output[i]
      
      
      
    } 
  }
  
  
  ## 
  vol<-max(df.1$slurry_store)
  diameter<-(vol/3.14/5)^0.5*2
  print(diameter)
 
  #
  df.1$level<-df.1$slurry/(3.14*(round(diameter)/2)^2)
 #reorder rows by doy 
  df.1<-df.1 %>% arrange(date)
 # keep the slurry level from the last year only 
  df.1.1<-filter(df.1,dos>730)
  df.1.sub<-subset(df.1.1,select = c("doy","level"))
  
  write.csv(df.1.sub,"level.csv",row.names=FALSE)
  
}

lapply(removal_list
       , run_in_folder)

#
# run STM model ---------------------------------------------------------------------
setwd("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/weather")
# read all weather files 
df.dk<-read.csv("DK.csv",header=T)
df.de<-read.csv("DE.csv",header=T)
df.nl<-read.csv("NL.csv",header=T)
df.se<-read.csv("SE.csv",header=T)
# temp list


#
country <- c("DK", "DE", "SE", "NL")
weather <- list(df.dk, df.de, df.se, df.nl)
names(weather) <- country

temp <- list(country = country, weather = weather)
temp

getwd()

run_in_folder <- function(country, weather_df) {
  # Make a folder with the country name
  folder_name <- file.path(
    "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/STM_pig/",
    country
  )
  cat("Folder name:", folder_name, "\n")
  dir.create(folder_name)
  
  # set the working directory
  setwd(folder_name)
  
  # Write data txt
  write.table(as.data.frame(weather_df),
              file.path(folder_name, "weather.txt"),
              sep = "\t",
              dec = ".",
              row.names = FALSE,
              quote = FALSE)
  
  # Copy the necessary files
  file.copy(
    from = c("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/par/pars.txt", #par
             "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/Temp/user_pars/user_pars_pig.txt", #u_par
             "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/level/DK_pig_1/level.csv", #level
             "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/stm.exe", #stm
             "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/run.bat", #run.bat
             "/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March/run.sh"#run.sh
             
             
    ),
    to = paste(folder_name
    )
  )
  
  
  
  
  # Execute .exe in each folder
  
  system2(paste(folder_name,
                "\\run.bat",
                sep = ""))
  
  # # delete executable 
  # 
  
  setwd("/Users/chunma/OneDrive - Aarhus universitet/Documents/for R/M4Models_all data/2023March")
}

Map(run_in_folder, country, weather)



