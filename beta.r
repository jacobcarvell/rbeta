
# This script calculates the capm, beta and sml of any stock using data from yahoo finance (via quandl).

# required libraries
library(ggplot2)
library(Quandl)

# define a few functions first

percReturn <- function(Price){
  # Data needs to be passed to the function ordered
  # Data also needs to be in a vector format
  result = numeric()
  
  for (i in 2:length(Price)) {
    item = (Price[i] - Price[i-1]) / Price[i-1]
    result = c(result, item)
  }
  
  return(result)
  
}

# prompt the user for both the stock code and the index code

stockCode <- readline(prompt = "Enter the ASX code of stock (e.g. BHP): ")
indexCode <- readline(prompt = "Enter the code of stock (e.g. AXJO): ")

# prompt the user for a start and end date

dateFrom <- readline(prompt = "Enter a start date (enter to skip) format=yyyy-mm-dd : ")
dateTo <- readline(prompt = "Enter an end date (enter to skip) format=yyyy-mm-dd : ")

# promt the user to change the data frequency. From daily to weekly, monthly etc

dataFreq <- readline(prompt = "Would you like to change the data frequency (blank=daily, else enter 'weekly|monthly|quarterly|annual'): ")

# Check if the date is blank. If it is blank run without date modifiers.
# In both instances a second logic tier is added to check if a data frequency modifier should be added.
# then return the stock and index into their respective variables

if (dateFrom == "" | dateTo == "") {
  if (dataFreq == "") {
    stock = Quandl(paste("YAHOO/AX_",stockCode, sep=""))
    index = Quandl(paste("YAHOO/INDEX_",indexCode, sep=""))
  } else {
    stock = Quandl(paste("YAHOO/AX_",stockCode, sep=""), collapse = dataFreq)
    index = Quandl(paste("YAHOO/INDEX_",indexCode, sep=""), collapse = dataFreq)
  }
} else {
  if (dataFreq == "") {
    stock = Quandl(paste("YAHOO/AX_",stockCode, sep=""), start_date=dateFrom, end_date=dateTo)
    index = Quandl(paste("YAHOO/INDEX_",indexCode, sep=""), start_date=dateFrom, end_date=dateTo)
  } else {
    stock = Quandl(paste("YAHOO/AX_",stockCode, sep=""), start_date=dateFrom, end_date=dateTo, collapse = dataFreq)
    index = Quandl(paste("YAHOO/INDEX_",indexCode, sep=""), start_date=dateFrom, end_date=dateTo, collapse = dataFreq)
  }
}

# Extract any missing data values from each of the data sets by comparing the date then sort by date

cleanedStock = merge(stock, index[, "Date"], by = 1, sort = FALSE)
cleanedStock = cleanedStock[order(cleanedStock["Date"]),]
cleanedIndex = merge(index, stock[, "Date"], by = 1, sort = FALSE)
cleanedIndex = cleanedIndex[order(cleanedIndex["Date"]),]

# calculate the return of each stock

stockReturn = percReturn(cleanedStock[["Adjusted Close"]])
indexReturn = percReturn(cleanedIndex[["Adjusted Close"]])
lsDate = cleanedStock[["Date"]]
lsDate = lsDate[2:length(lsDate)]

# Extract the adjusted closing stocks from both and add to a temp variable

betaData = data.frame(lsDate, stockReturn, indexReturn)

# calculate the linear regression then store the coefficent

fit = lm(betaData[["stockReturn"]] ~ betaData[["indexReturn"]])
beta = coefficients(fit)[2]
print(beta)

# plot the result

p = ggplot(betaData, aes(x=stockReturn, y=indexReturn)) + geom_point() + geom_smooth(method=lm, se=FALSE)
p = p + xlab(paste(indexCode, "Return (%)")) + ylab(paste(stockCode, "Return (%)"))
p = p + ggtitle(paste("Scatterplot of", stockCode, "vs.", indexCode, "return as a percentage"))

print(p)

ggsave(paste("beta", stockCode, "vs.", indexCode, "as perc.png"))