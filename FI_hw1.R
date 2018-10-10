### Financial Informatics ###
### HW1 ###
### 2013310443 ������ ###

Sys.setlocale('LC_ALL', "korean") #���� Window���� �ѱ��� ����
# setwd("H:/PycharmProjects/2018-2_FI_hw1")

install.packages('dplyr')
install.packages('xts')
install.packages('rvest')
install.packages('quantmod')

library(dplyr)
library(tibble)
library(xts) #�ð迭 ó��
library(quantmod) #�ְ� ������ ó��
library(httr) #ũ�Ѹ�
library(rvest) #ũ�Ѹ�

#####################################

rm(list=ls()) #���� �ʱ�ȭ.

### S&P 500 ��� ���: Wikipedia ũ�Ѹ�. 

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
res <- GET(url)
wiki <- read_html(res)
wiki_table <- wiki %>% html_nodes(".wikitable") %>% html_table(fill=TRUE)
fcodes <- unlist(c(wiki_table[[1]][1]), use.names=FALSE)
fcodes
length(fcodes) # => 505

### S&P 500 ��� �ְ� ������ �޾ƿ���. 

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

SP500_out <- data.frame(index(SP500_noNA), as.data.frame(SP500_noNA))
colnames(SP500_out)[1] <- "date"
write.csv(SP500_out, file='SP500_out_2015-2018.csv')