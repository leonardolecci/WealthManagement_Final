##################################################
##################################################
##### Building the Fama French 3 factor model ####
##################################################
##################################################
#install.packages("timeDate")
#install.packages("chron")
#install.packages("curl")
fama_french_3F <- function(ticker, from_date, to_date){
  
library(quantmod)
library(ggplot2)
library(timeDate)
library(chron)
library(curl)
#library(lubridate)

t.id <- ticker
from.dat <- from_date
to.dat <- to_date

ticker.r <- getSymbols(t.id, from = min(from.dat, to.dat), to = max(from.dat,to.dat), auto.assign=FALSE)
ticker.r <- ticker.r[,c(1,6)]


#create temp file
tf <- tempfile()

#create temp dir
td <- tempdir()


zip.file.location <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
download.file(zip.file.location, tf, mode = "wb")
file.name <- unzip(tf, exdir=td)
y <- read.csv(file.name,skip=3)

names(y)[1]<-"Date"

#Format dates: go from numerical YYYYMM (19690801) to date format YYYY-MM-DD (1969-08-01) 
y$Date<-as.Date(format(y$Date, trim=TRUE),format="%Y%m%d")

y <<- y

rr.v <-vector(mode="numeric",length=length(nrow(ticker.r)))

#Use the adjusted entry to count for stock split for now to get market return
for(i in 2:nrow(ticker.r)){
  rr.v[i] <- as.numeric(ticker.r[i,2]) / as.numeric(ticker.r[i-1,2]) - 1
}

#remove the first element - to get the right date range
rr.v <- rr.v[-1]

#get the same dates from FF Model
subset_y <- subset(y, Date >= min(from.dat , to.dat) & Date <= max(from.dat , to.dat))
subset_y <- subset_y[1:length(rr.v),]

rr.mod<-vector(mode="numeric",length=length(nrow(subset_y)))

model = lm(rr.v ~ subset_y$Mkt.RF + subset_y$SMB + subset_y$HML)
summary(model)

################
#########################
##################################
###########################################
#### Now we can analyze more : time series 
########
#model regression
rr.mod <- 0*coef(model)[1] + coef(model)[2]*subset_y$Mkt.RF + 
  coef(model)[3]*subset_y$SMB + coef(model)[4]*subset_y$HML

#obtaining epsilon(ticker)
rr.spf <- rr.v - rr.mod

#calculate the Cumulative return for the Ticker data
tr.cum1<-vector(mode="numeric",length=length(rr.v))
for(i in 1:length(rr.v)){
  tr.cum1[i] <- 1+rr.v[i]
}

tr.cum2<-vector(mode="numeric",length=length(tr.cum1))
#cumulative product
tr.cum2<-cumprod(tr.cum1)

print(tr.cum2)
#calculate the Cumulative return for the Epsilon 
rr.spf1<-vector(mode="numeric",length=length(rr.spf))
for(i in 1:length(rr.spf)){
  rr.spf1[i] <- 1+rr.spf[i]
}

rr.spf2<-vector(mode="numeric",length=length(rr.spf1))
#cumulative product
rr.spf2<-cumprod(rr.spf1)
#write.csv(rr.spf2, file="H:\\aa\\rrspf2.csv")
#write.csv(tr.cum2, file="H:\\aa\\trcum2.csv")

#combine vectors
x_name <- "rr_spf"
y_name <- "tr_cum"
z_name <- "Date"

require(reshape2)
df <- data.frame(subset_y$Date, rr.spf2,tr.cum2)

#where tr.cum2 - is the cumulative return for ticker data
#where rr.spf2 - is the cumulative return for the error (Epsilon) between model and ticker

colnames(df) <- c(z_name,x_name, y_name)
# write.csv(df, file=paste('H:\\aa\\',t.id,from.dat,to.dat,'.csv'))
dz <<- df

  return(list(df, model))
}# closing fama_french_3F function


##################################################
##################################################
##### Building the Fama French 5 factor model ####
##################################################
##################################################

fama_french_5F <- function(ticker, from_date, to_date){
  
  library(quantmod)
  library(ggplot2)
  library(timeDate)
  library(chron)
  library(curl)
  library(plotly)
  #library(lubridate)
  
  t.id <- ticker
  from.dat <- from_date
  to.dat <- to_date
  
  ticker.r <- getSymbols(t.id, from = min(from.dat, to.dat), to = max(from.dat,to.dat), auto.assign=FALSE)
  ticker.r <- ticker.r[,c(1,6)]
  
  
  #create temp file
  tf <- tempfile()
  
  #create temp dir
  td <- tempdir()
  
  
  zip.file.location <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_daily_CSV.zip"
  download.file(zip.file.location, tf, mode = "wb")
  file.name <- unzip(tf, exdir=td)
  y <- read.csv(file.name,skip=3)
  
  names(y)[1]<-"Date"
  
  #Format dates: go from numerical YYYYMM (19690801) to date format YYYY-MM-DD (1969-08-01) 
  y$Date<-as.Date(format(y$Date, trim=TRUE),format="%Y%m%d")
  
  y <<- y
  
  rr.v <-vector(mode="numeric",length=length(nrow(ticker.r)))
  
  #Use the adjusted entry to count for stock split for now to get market return
  for(i in 2:nrow(ticker.r)){
    rr.v[i] <- as.numeric(ticker.r[i,2]) / as.numeric(ticker.r[i-1,2]) - 1
  }
  
  #remove the first element - to get the right date range
  rr.v <- rr.v[-1]
  
  #get the same dates from FF Model
  subset_y <- subset(y, Date >= min(from.dat , to.dat) & Date <= max(from.dat , to.dat))
  subset_y <- subset_y[1:length(rr.v),]
  
  rr.mod<-vector(mode="numeric",length=length(nrow(subset_y)))
  
  model = lm(rr.v ~ subset_y$Mkt.RF + subset_y$SMB + subset_y$HML +
               subset_y$RMW + subset_y$CMA)
  summary(model)
  
  ################
  #########################
  ##################################
  ###########################################
  #### Now we can analyze more : time series 
  ########
  #model regression
  rr.mod <- 0*coef(model)[1] + coef(model)[2]*subset_y$Mkt.RF + 
    coef(model)[3]*subset_y$SMB + coef(model)[4]*subset_y$HML +
    coef(model)[5]*subset_y$RMW + coef(model)[6]*subset_y$CMA
  
  #obtaining epsilon(ticker)
  rr.spf <- rr.v - rr.mod
  
  #calculate the Cumulative return for the Ticker data
  tr.cum1<-vector(mode="numeric",length=length(rr.v))
  for(i in 1:length(rr.v)){
    tr.cum1[i] <- 1+rr.v[i]
  }
  
  tr.cum2<-vector(mode="numeric",length=length(tr.cum1))
  #cumulative product
  tr.cum2<-cumprod(tr.cum1)
  
  print(tr.cum2)
  #calculate the Cumulative return for the Epsilon 
  rr.spf1<-vector(mode="numeric",length=length(rr.spf))
  for(i in 1:length(rr.spf)){
    rr.spf1[i] <- 1+rr.spf[i]
  }
  
  rr.spf2<-vector(mode="numeric",length=length(rr.spf1))
  #cumulative product
  rr.spf2<-cumprod(rr.spf1)
  #write.csv(rr.spf2, file="H:\\aa\\rrspf2.csv")
  #write.csv(tr.cum2, file="H:\\aa\\trcum2.csv")
  
  #combine vectors
  x_name <- "rr_spf"
  y_name <- "tr_cum"
  z_name <- "Date"
  
  require(reshape2)
  df <- data.frame(subset_y$Date, rr.spf2,tr.cum2)
  
  #where tr.cum2 - is the cumulative return for ticker data
  #where rr.spf2 - is the cumulative return for the error (Epsilon) between model and ticker
  
  colnames(df) <- c(z_name,x_name, y_name)
  # write.csv(df, file=paste('H:\\aa\\',t.id,from.dat,to.dat,'.csv'))
  dz <<- df
  
  return(list(df, model))
}# closing fama_french_5F function
