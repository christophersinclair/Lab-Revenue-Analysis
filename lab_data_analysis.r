# Infectious Disease Laboratory Statistical Data Analysis Script
# Written by: Christopher Sinclair - Research Programmer
# Supervisors: Dr. William Mattingly and Stephen Furmanek

# Install any necessary packages
install.packages('tidyverse')
install.packages('forecast')
library(tidyverse)
library(forecast)

# Set any options needed for plots or data wrangling
options(scipen = 5) # turns off scientitic notation for large numbers

# Generic query function for querying data from local MySQL
query <- function(q) {                                                           # function to query mysql
  library(RMySQL)                                                                # use the RMySQL package  
  conn <- dbConnect(MySQL(), user="root", password="****", host="127.0.0.1") # mysql connection string
  result<- dbGetQuery(conn, q)                                                   # run query
  lapply(dbListConnections(dbDriver(drv = "MySQL")), dbDisconnect)               # disconnects all mysql connections
  result                                                                         # return result
}

####### Amount billed by top ten tests over five years #######
total_charges <- query("select sum(total_charge) from labmgmt_db.invoices") # query for total charges over the five year period
all_ids <- query("select test_id from labmgmt_db.patient_test") # query for every test id in the database

top_ids <- lapply(seq_along(1:10), function(i) {  # gather the top ten tests that have the most billed (sales)
  names(sort(-table(all_ids)))[i]                 # neat little trick for finding the mode of a list
})
top_ids <- unlist(top_ids)                        # turn the list into a numeric vector

top_test_charges <- lapply(seq_along(top_ids), function(i,q) {               # find the amount charged for the top ten tests
  id_query <- paste("select amount from labmgmt_db.test where id=", q[i])
  query(id_query)
}, q = top_ids)
top_test_charges <- unlist(top_test_charges)

top_test_descs <- lapply(seq_along(top_ids), function(i, q) {                # grab the test descriptions for the top ten tests (this is overwritten with abbreviations)
  desc_query <- paste("select description from labmgmt_db.test where id=", q[i])
  query(desc_query)
}, q = top_ids)
top_test_descs <- unlist(top_test_descs)

number_of_bills <- lapply(seq_along(top_ids), function(i,q) {                # query the number of times each test was actually billed
  num_query <- paste("select count(distinct(id)) FROM labmgmt_db.patient_test where test_id =", q[i])
  query(num_query)
}, q = top_ids)
number_of_bills <- unlist(number_of_bills)

individual_test_charges <- c(1:10)
for (i in 1:length(top_test_charges)) {
  individual_test_charges[i] <- top_test_charges[i] * number_of_bills[i]    # calculate aggregate billing for each test over the five year period
}

top_test_descs <- c("QFT-CI", "LPN", "MPN", "CPN", "QFT-CI (UL)", "LYME", "QFT", "RIC-G", "RIC-M", "ECP") # manually entered in abbreviated test descriptions because the descriptions queried from the database are too long for the graph

barplot(individual_test_charges, names.arg=top_test_descs, ylim=c(0,1000000), xlab="Top Ordered Tests", ylab = "Amount Billed", main="Amount Billed for Top Ordered Tests",yaxt="n")
axis(2, at=seq(0,1000000,100000), labels=paste(seq(0,1000,100), "k", sep=""))


####### Time Series analysis on the amount billed from the laboratory per month #######
months<-seq.Date(from=as.Date("2013-08-01"), to=as.Date("2018-01-01"), by="month")  # create a vector with each month from 2013 to 2015

amt_billed_by_month <- lapply(seq_along(months), function(i,q) { # apply a function along all of the months between 2013 and 2018
  p <- i + 1
  monthly_query <- paste("select sum(charge) as charges from labmgmt_db.invoice_records where date_entered between '",q[i], "' and '",q[p],"'") # query for sums of charges per month
  query(monthly_query) # query the database
}, q=months)

amt_billed_by_month <- unlist(amt_billed_by_month) # transform list into vector
amt_billed_by_month[is.na(amt_billed_by_month)] <- 0 # replace NA values with 0s
amt_billed_by_month <- amt_billed_by_month[1:53]  # remove leading and trailing 0 values
months <- months[1:53]                            # remove leading and trailing months with 0 values

#plot(months, amt_billed_by_month, type="o", pch=20, cex=1.5, lwd=1, lty=1, ylim=c(0,160000), yaxt="n", xlab = "Month", ylab="Amount Billed", main = "Amount Billed per Month") # plot amount billed by month, lineplot with dots, size of dot = 20, width of dot = 1.5, line width = 2, line type = 1
#axis(2, at=seq(0,160000,10000), labels=paste(seq(0,160,10),"k", sep=""))
#abline(h=seq(0,160000,10000), lty=3)

amt <- ts(amt_billed_by_month, start=c(2013,8), frequency = 12)  # create a time series on the amount billed by month
print(amt)
library(forecast)
plot(decompose(amt)) # if there appears to be a non-random trend, you need to difference it

# order = c(AR, I, MA); AR = no. autoregressive terms; MA = no. moving avg terms; I = is it differenced (use 1 for single-lag observation differencing)
p <- Arima(amt, order=c(1,1,1))    # create an ARIMA model with 1 autoregressive term, 1 lagging differenced term, and 1 moving average term
acf(diff(amt)) # number of peaks past 1 is number of autoregressive terms
pacf(diff(amt)) # number of peaks is number of moving average terms

forecast(p,4)   # forecast of ARIMA model with 4 predicted data points
plot(forecast(p, 4), ylim=c(0,160000), yaxt="n", type="o", pch=20, cex=1.5, lwd=1, lty=1, xlab = "Month", ylab = "Amount Billed", main = "Forecasts from ARIMA(1,1,1) on Amount Billed per Month")
axis(2, at=seq(0,160000,40000), labels=paste(seq(0,160,40),"k",sep=""))
abline(h=seq(0,160000,10000), lty=3)
summary(p)
