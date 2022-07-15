#################################
### Created by Leonardo Lecci ###
###       Jul 14 2022         ###
#################################

# Let's now install Yahoo Finance API to get real time data
#install.packages("quantmod")

library(quantmod)
library(corrplot)
source("./famafrench.R")

# Step 1: pulling in pricing data and combining the data frames gathered

# Import portfolio of client from csv
portfolio <- read.csv("./Portfolio.csv")
dim_portfolio <- dim(portfolio)
dim_portfolio <- dim_portfolio[1]
stock_vector <- c(NULL)

# Create vector with the stock held by the client
for(p in 1:dim_portfolio){
  stock_vector <- append(stock_vector, portfolio[p,1])
}
length_vector <- length(stock_vector)

#initializing joined_prices df, and vector to calculate positions for pulling only adjusted prices
joined_prices <- NULL
adj_position <- c(NULL)


# loop to get all the values of stock through quantmod
for(i in 1:length_vector){
  stock_values <- NULL
  stock_values <- getSymbols(stock_vector[i], auto.assign = FALSE)
  joined_prices <- cbind(joined_prices, stock_values)
  # We know adj. price is in the 6th column for each stock
  # so we multiply each cycle by 6 to get the positions for each stock
  adj_position <- append(adj_position, i*6)
}

# Step 2: pulling in only adjusted prices
joined_prices <- joined_prices[,adj_position]
joined_prices <- as.data.frame(joined_prices)
#Step 3: calculate returns

# Version 1 (more complex and used in real life work)

# User Defined Function to add time window
window_returns <- function(x, t){
  compounded <- rep(NA, each = (t-1))
  for(i in t:length(x)){
    compounded[i] <- log(x[i]/x[i-t+1])
  }
  return(compounded)
}#closing the window_returns UDF

#calling the UDF
# t=25 for the trading days per month

joined_monthly_returns <- NULL
trading_days_month <- 25

# for cycle for combining all log returns in a single df
for(i in 1:length_vector){
  joined_monthly_returns <- cbind(joined_monthly_returns, window_returns(x=joined_prices[,i], t=25))
}

joined_monthly_returns <- as.data.frame(joined_monthly_returns)

# Calculating portfolio return
# This is not precise for old values, portfolio allocation changes over time, this assumes a fiexd allocation for the past

# Getting weight in portfolio for each stock
stock_weight <- c(NULL)
for(p in 1:length_vector){
  stock_weight <- append(stock_weight, portfolio[p,3])
}

# creating a vector with proportional return for each stock

prop_returns <- c(NULL)
for(i in 1:length_vector){
  prop_returns <- append(prop_returns, joined_monthly_returns[i]*stock_weight[i])
}
prop_returns <- as.data.frame(prop_returns)

# Add column for portfolio's monthly return and add all monthly returns
for(i in 1:nrow(joined_monthly_returns)){
  joined_monthly_returns$portfolio[i] <- sum(prop_returns[i,],na.rm=TRUE)
}
stock_vector_port <- append(stock_vector, "portfolio")

# Calculate 12, 24, 36, 48, 60 months returns per holding and portfolio
# Taking only every 25th elements of the monthly returns
one_montlhy_returns <- as.data.frame(NULL)
one_montlhy_returns <-  joined_monthly_returns[seq(25, nrow(joined_monthly_returns), 25), ]

annual_df <- as.data.frame(NULL)
one_length <- nrow(one_montlhy_returns)
ret_vector_12_months <- c(NULL)
ret_vector_24_months <- c(NULL)
ret_vector_36_months <- c(NULL)
ret_vector_48_months <- c(NULL)
ret_vector_60_months <- c(NULL)

for(i in 1:(length_vector+1)){
  ret_vector_12_months <- append(ret_vector_12_months, sum(one_montlhy_returns[(one_length-11):one_length,i], na.rm=TRUE))
  ret_vector_24_months <- append(ret_vector_24_months, sum(one_montlhy_returns[(one_length-23):one_length,i], na.rm=TRUE))
  ret_vector_36_months <- append(ret_vector_36_months, sum(one_montlhy_returns[(one_length-35):one_length,i], na.rm=TRUE))
  ret_vector_48_months <- append(ret_vector_48_months, sum(one_montlhy_returns[(one_length-47):one_length,i], na.rm=TRUE))
  ret_vector_60_months <- append(ret_vector_60_months, sum(one_montlhy_returns[(one_length-59):one_length,i], na.rm=TRUE))
}

annual_df <- as.data.frame(ret_vector_12_months)
annual_df <- cbind(annual_df, ret_vector_24_months)
annual_df <- cbind(annual_df, ret_vector_36_months)
annual_df <- cbind(annual_df, ret_vector_48_months)
annual_df <- cbind(annual_df, ret_vector_60_months)

n_months <- c("12_Months", "24_Months", "36_Months", "48_Months", "60_Months")
rownames(annual_df) <- stock_vector_port
colnames(annual_df) <- n_months

#write.table(annual_df, file = "annual_df.csv", sep = ",", quote = FALSE)

# INVESTMENT RISK

# Calculating sigma to show total risk for assets and portfolio


time_index <- nrow(one_montlhy_returns)
sigma_vector_12_months <- c(NULL)
sigma_vector_24_months <- c(NULL)
sigma_vector_36_months <- c(NULL)
sigma_vector_48_months <- c(NULL)
sigma_vector_60_months <- c(NULL)

for(i in 1:(length_vector+1)){
  sigma_vector_12_months <- append(sigma_vector_12_months, sd(one_montlhy_returns[(time_index-11):time_index,i])*sqrt(12))
  sigma_vector_24_months <- append(sigma_vector_24_months, sd(one_montlhy_returns[(time_index-23):time_index,i])*sqrt(12))  
  sigma_vector_36_months <- append(sigma_vector_36_months, sd(one_montlhy_returns[(time_index-35):time_index,i])*sqrt(12))
  sigma_vector_48_months <- append(sigma_vector_48_months, sd(one_montlhy_returns[(time_index-47):time_index,i])*sqrt(12))
  sigma_vector_60_months <- append(sigma_vector_60_months, sd(one_montlhy_returns[(time_index-59):time_index,i])*sqrt(12))
}

sigma_df <- as.data.frame(sigma_vector_12_months)
sigma_df <- cbind(sigma_df, sigma_vector_24_months)
sigma_df <- cbind(sigma_df, sigma_vector_36_months)
sigma_df <- cbind(sigma_df, sigma_vector_48_months)
sigma_df <- cbind(sigma_df, sigma_vector_60_months)


rownames(sigma_df) <- stock_vector_port
colnames(sigma_df) <- n_months
write.table(sigma_df, file = "sigma_df.csv", sep = ",", quote = FALSE)

# TRACKING ERROR
# Adding BlackRock 60/40 Target Allocation Fund Institutional Shares: BIGPX as benchmark for the client

BIGPX_adjusted <- as.data.frame(getSymbols("BIGPX", auto.assign = FALSE)[,6])
BIGPX <- window_returns(BIGPX_adjusted[,1], t=25)
joined_monthly_returns <- cbind(joined_monthly_returns, BIGPX)
BIGPX <- joined_monthly_returns$BIGPX[seq(25, nrow(joined_monthly_returns), 25)]
one_montlhy_returns <- cbind(one_montlhy_returns, BIGPX)
stock_vector_port_bench <- append(stock_vector_port, "BIGPX")


# renaming all columns with stocks stickers
colnames(one_montlhy_returns) <- stock_vector_port_bench
colnames(joined_monthly_returns) <- stock_vector_port_bench


# calculating tracking error on active returns

te_vector_12_months <- c(NULL)
te_vector_24_months <- c(NULL)
te_vector_36_months <- c(NULL)
te_vector_48_months <- c(NULL)
te_vector_60_months <- c(NULL)

for(i in 1:(length_vector+1)){
  te_vector_12_months <- append(te_vector_12_months, sd(one_montlhy_returns[(time_index-11):time_index,i]-one_montlhy_returns$BIGPX[(time_index-11):time_index])*sqrt(12))
  te_vector_24_months <- append(te_vector_24_months, sd(one_montlhy_returns[(time_index-23):time_index,i]-one_montlhy_returns$BIGPX[(time_index-23):time_index])*sqrt(12))
  te_vector_36_months <- append(te_vector_36_months, sd(one_montlhy_returns[(time_index-35):time_index,i]-one_montlhy_returns$BIGPX[(time_index-35):time_index])*sqrt(12))
  te_vector_48_months <- append(te_vector_48_months, sd(one_montlhy_returns[(time_index-47):time_index,i]-one_montlhy_returns$BIGPX[(time_index-47):time_index])*sqrt(12))
  te_vector_60_months <- append(te_vector_60_months, sd(one_montlhy_returns[(time_index-59):time_index,i]-one_montlhy_returns$BIGPX[(time_index-59):time_index])*sqrt(12)) 
}

te_df <- as.data.frame(te_vector_12_months)
te_df <- cbind(te_df, te_vector_24_months)
te_df <- cbind(te_df, te_vector_36_months)
te_df <- cbind(te_df, te_vector_48_months)
te_df <- cbind(te_df, te_vector_60_months)

rownames(te_df) <- stock_vector_port
colnames(te_df) <- n_months


# Sharpe ratio


risk_free <- 0.00000000000000001

sharpe_vector_12_months <- c(NULL)
sharpe_vector_24_months <- c(NULL)
sharpe_vector_36_months <- c(NULL)
sharpe_vector_48_months <- c(NULL)
sharpe_vector_60_months <- c(NULL)

for(i in 1:(length_vector+1)){
  sharpe_vector_12_months <- append(sharpe_vector_12_months, (mean((((1+one_montlhy_returns[(time_index-11):time_index, i])^12)-1)-risk_free)/sigma_df$`12_Months`[i]))
  sharpe_vector_24_months <- append(sharpe_vector_24_months, (mean((((1+one_montlhy_returns[(time_index-23):time_index, i])^12)-1)-risk_free)/sigma_df$`24_Months`[i]))
  sharpe_vector_36_months <- append(sharpe_vector_36_months, (mean((((1+one_montlhy_returns[(time_index-35):time_index, i])^12)-1)-risk_free)/sigma_df$`36_Months`[i]))
  sharpe_vector_48_months <- append(sharpe_vector_48_months, (mean((((1+one_montlhy_returns[(time_index-47):time_index, i])^12)-1)-risk_free)/sigma_df$`48_Months`[i]))
  sharpe_vector_60_months <- append(sharpe_vector_60_months, (mean((((1+one_montlhy_returns[(time_index-59):time_index, i])^12)-1)-risk_free)/sigma_df$`60_Months`[i]))
}

sharpe_df <- as.data.frame(sharpe_vector_12_months)
sharpe_df <- cbind(sharpe_df, sharpe_vector_24_months)
sharpe_df <- cbind(sharpe_df, sharpe_vector_36_months)
sharpe_df <- cbind(sharpe_df, sharpe_vector_48_months)
sharpe_df <- cbind(sharpe_df, sharpe_vector_60_months)

rownames(sharpe_df) <- stock_vector_port
colnames(sharpe_df) <- n_months


# See lm modelling
last_12 <- one_montlhy_returns[(time_index-11):time_index,]
last_24 <- one_montlhy_returns[(time_index-23):time_index,]
last_36 <- one_montlhy_returns[(time_index-35):time_index,]
last_48 <- one_montlhy_returns[(time_index-47):time_index,]
last_60 <- one_montlhy_returns[(time_index-59):time_index,]

CAPM_list <- list()


for(i in 1:(length_vector+1)){
  reg_12 <- lm(last_12[,i]~last_12[,ncol(last_12)])
  reg_24 <- lm(last_24[,i]~last_24[,ncol(last_24)])
  reg_36 <- lm(last_36[,i]~last_36[,ncol(last_36)])
  reg_48 <- lm(last_48[,i]~last_48[,ncol(last_48)])
  reg_60 <- lm(last_60[,i]~last_60[,ncol(last_60)])
  reg_list <- list(reg_12, reg_24, reg_36, reg_48, reg_60)
  names(reg_list) <- n_months
  CAPM_list <- append(CAPM_list, list(reg_list))
}

names(CAPM_list) <- stock_vector_port

# PASSING ONTO FAMA FRENCH

# Fama French with 3 factors

# creating today's date and years back

today <- as.character.Date(as.Date(Sys.Date()))

begin_date_12 <- as.POSIXlt(as.Date(Sys.Date()))
begin_date_12$year <- begin_date_12$year-1
begin_date_12 <- as.character.Date(begin_date_12)

begin_date_24 <- as.POSIXlt(as.Date(Sys.Date()))
begin_date_24$year <- begin_date_24$year-2
begin_date_24 <- as.character.Date(begin_date_24)

begin_date_36 <- as.POSIXlt(as.Date(Sys.Date()))
begin_date_36$year <- begin_date_36$year-3
begin_date_36 <- as.character.Date(begin_date_36)

begin_date_48 <- as.POSIXlt(as.Date(Sys.Date()))
begin_date_48$year <- begin_date_48$year-4
begin_date_48 <- as.character.Date(begin_date_48)

begin_date_60 <- as.POSIXlt(as.Date(Sys.Date()))
begin_date_60$year <- begin_date_60$year-5
begin_date_60 <- as.character.Date(begin_date_60)


# creating final list for all regression
ff3f_list <- list()

for(i in 1:(length_vector)){
  reg_ff3f_12 <- fama_french_3F(ticker=stock_vector[i], from_date=begin_date_12, to_date=today)
  reg_ff3f_24 <- fama_french_3F(ticker=stock_vector[i], from_date=begin_date_24, to_date=today)
  reg_ff3f_36 <- fama_french_3F(ticker=stock_vector[i], from_date=begin_date_36, to_date=today)
  reg_ff3f_48 <- fama_french_3F(ticker=stock_vector[i], from_date=begin_date_48, to_date=today)
  reg_ff3f_60 <- fama_french_3F(ticker=stock_vector[i], from_date=begin_date_60, to_date=today)
  reg_ff3f_list <- list(reg_ff3f_12, reg_ff3f_24, reg_ff3f_36, reg_ff3f_48, reg_ff3f_60)
  names(reg_ff3f_list) <- n_months
  ff3f_list <- append(ff3f_list, list(reg_ff3f_list))
}

names(ff3f_list) <- stock_vector



#summary(ff3f_list$stock_vector[1]$n_months[1][[2]]) 

#looking at factor loading - are any statistically significant
#now let's visualize the model error and the cumulative stock returns
#ggplot(data=TSLA_ff3f[[1]])+
#  geom_line(aes(x=Date, y=rr_spf), color="red4")+
#  geom_line(aes(x=Date, y=tr_cum), color="blue") #red is the error and blue is the stock return

# CREATE A PLOT TO BETTER VIEW THE CORRELATION MATRIX

source("http://www.sthda.com/upload/rquery_cormat.r")
# calling a function from the source above

rquery.cormat(one_montlhy_returns[(time_index-11):time_index,])
rquery.cormat(one_montlhy_returns[(time_index-23):time_index,])
rquery.cormat(one_montlhy_returns[(time_index-35):time_index,])
rquery.cormat(one_montlhy_returns[(time_index-47):time_index,])
rquery.cormat(one_montlhy_returns[(time_index-59):time_index,])






#cor(joined_monthly_returns[(time_index-23):time_index,],use="complete.obs")

