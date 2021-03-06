### Financial Informatics ###
### HW1 ###
### 2013310443 최재필 ###

Sys.setlocale('LC_ALL', "korean") #영문 Window에서 한국어 설정
# setwd("H:/PycharmProjects/2018-2_FI_hw1")
# setwd("C:/Users/chlje/PycharmProjects/2018-2_FI_hw1")

install.packages('dplyr')
install.packages('xts')
install.packages('rvest')
install.packages('quantmod')
install.packages("RCurl")
install.packages("PerformanceAnalytics")

library(dplyr)
library(tibble)
library(xts) #시계열 처리
library(quantmod) #주가 데이터 처리
library(httr) #크롤링
library(rvest) #크롤링
library(RCurl)
library(PerformanceAnalytics)

rm(list=ls()) #변수 초기화.

####################
#### 데이터 다운로드 

### S&P 500 기업 목록: Wikipedia 크롤링. 

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
res <- GET(url)
wiki <- read_html(res)
wiki_table <- wiki %>% html_nodes(".wikitable") %>% html_table(fill=TRUE)
fcodes <- unlist(c(wiki_table[[1]][1]), use.names=FALSE)
fcodes
length(fcodes) # => 505

######################################
#### S&P 500 기업 주가 데이터 받아오기. 

SP500 <- NULL
start_date <- "2015-01-01"
for (f in fcodes) {
  tryCatch({
    SP500 <- cbind(SP500, getSymbols(f, from=start_date, auto.assign=FALSE)[,6]) 
  }, error=function(e){cat("ERROR!:", conditionMessage(e), "\n")})
} # BRK.B and BF.B download failed. 

ncol(SP500) # => 503
View(SP500) # BHF, DXC, JEF, UA,... has missing values. 

## drop columns with missing values.
SP500_noNA <-SP500[, !sapply(SP500, function(x) any(is.na(x)))]
ncol(SP500_noNA) # => 492
View(SP500_noNA)
save(SP500_noNA, file="SP500_noNA.RData")

# export to csv (for Python implementation)
SP500_out <- data.frame(index(SP500_noNA), as.data.frame(SP500_noNA)) 
colnames(SP500_out)[1] <- "date"
write.csv(SP500_out, file='SP500_out_2015-2018.csv')

load("SP500_noNA.RData") # Note that if you save your data with save(), it cannot be restored under different name. The original object names are automatically used.
View(SP500_noNA)

######################################################
#### Setting up investment universe: Top 100 of S&P500
df <- SP500_noNA
View(df)
yearly_df <- NULL

for (i in (1:ncol(df))){
  temp_df <- yearlyReturn(df[,i])
  colnames(temp_df) <- paste0(colnames(df[,i]))
  yearly_df <- cbind(yearly_df, temp_df)
  
}

yearly_df <- as.data.frame(t(yearly_df))
View(yearly_df)
top100 <- yearly_df[order(-yearly_df[,1]),]
top100 <- head(top100, n=100)
View(top100) # Top 100 stocks sorted by yearly return as of 2015-12-31

universe <- attr(top100, 'row.names')  
df <- df[,universe] 
View(df) # Time series data of top 100 stocks. (2015-1-01-01 ~ 2018-10-10)

#########################
#### Strategy 1: Momentum

original_balance = 100000 # Start investment with $100,000
balance <- 100000
t = 124 # rebalancing every t days, 124 days equals 6 months working days.  

# function to reshuffle given 100 stocks of past 365 days.
# input: df chunk of 365 days
# output: winner/loser firms (as global variables), reshuffled top 100 firms
reshuffle <- function(d) { 
  yearly_d <- NULL
  for (firm in (1:ncol(d))){
    temp_d <- yearlyReturn(d[,firm])
    colnames(temp_d) <- colnames(d[,firm])
    yearly_d <- cbind(yearly_d, temp_d)
  }
  yearly_d <- as.data.frame(t(yearly_d))
  yearly_d <- as.data.frame(t(yearly_d))
  top100_d <- yearly_d[order(-yearly_d[,1]),]
  
  winners <<- colnames(top100_d[,(1:10)])
  losers <<- colnames(top100_d[,(90:100)])
  
  return(top100_d)
}

for (i in (252:length(df))){
  if (i==252) { # return for the first year.  
    weight_first <- rep(c(0.01, 0.01, 0.01) , times=c(10, 80, 10)) # The same weight for each firm on the first year. 
    
    temp_return <- Return.portfolio(df[(i-251):i], rebalance_on = 'years', weights=weight_first, geometric = FALSE)
    yearly_return <- as.numeric(last(temp_return)) # Gets return rate of the first year's portfolio 
    
    balance <- balance * yearly_return
    
    current_top100_returns <- reshuffle(df[(i-251):i]) # Return of each stocks on the last day. (for the first year, it's Dec 31.)
    current_rank <- colnames(current_top100_returns) # List of 100 companies sorted by rank. 
  }
  
  if ((i>252) & (i%%t == 0)) {
    tryCatch({
      print(i)
      weights <- rep(c(0.013, 0.01, 0.007) , times=c(10, 80, 10)) # weight is 30% heavier/lighter for 10 winner/loser firms.
      
      temp_return <- Return.portfolio(df[(i-(t-1)):i], rebalance_on = 'years', weights=weights, geometric = FALSE)
      yearly_return <- as.numeric(last(temp_return)) # Gets return rate after t days.
      
      balance <- balance * yearly_return
      
      current_top100_returns <- reshuffle(df[(i-(t-1)):i]) # Return of each stocks on the last day. (for the first year, it's Dec 31.)
      current_rank <- colnames(current_top100_returns) # List of 100 companies sorted by rank. 
    }, error=function(e){cat("ERROR!:", conditionMessage(e), "\n")})
  } 
}
balance

length(df)
View(temp_return)

###################
#### Strategy 2: Moving Average 

weights <- rep(c(0.011, 0.01, 0.009) , times=c(10, 80, 10))
weights <- rep(c(0.00, 0.01, 0.02) , times=c(20, 40, 40))
weights



test_d <- Return.portfolio(df, rebalance_on = 'years', weights=weights, geometric = FALSE)
View(test_d)

View(df['2015-06-11'])



return_df <- Return.portfolio(df, rebalance_on = 'years', weights=NULL, geometric = FALSE) 
macd <- MACD(return_df, nFast=12, nSlow=26,nSig=9,maType=SMA, percent = FALSE)
plot(macd)
chartSeries(return_df, TA="addMACD()")

            