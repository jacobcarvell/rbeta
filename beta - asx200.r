
# This script calculates the beta of stocks in the ASX200 using data from yahoo finance (via quandl).

# required libraries
library(ggplot2)
library(Quandl)
library(rvest)

# Enter your api key here to allow for more than 50 calls per day
Quandl.api_key("jSb_BspV8cKfsYKk_68t")

# calculate the percentage return from a list of ordered data

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

readASX200 <- function() {
  # read html from ASX200 list website
  asxPage <- read_html("http://dividend.net.au/asx-200-list-and-chart/")
  
  # extract the table from the page
  asxTable <- asxPage %>%
    html_nodes("table") %>%
    html_table()
  asxTable
  
  # extract the second table on the page then delete the first row (contains the tables headers)
  asxTable = asxTable[[2]]
  names(asxTable) = asxTable[1,]
  asxTable = asxTable[-1,]
  return(asxTable)
}

getStockData <- function(code, dateFrom, dateTo, dataFreq, index=FALSE) {
  # Check if this is to be an index or stock. If it is return a different ticker for quandl
  if (index == TRUE) {
    ticker = paste("YAHOO/INDEX_",code, sep="")
  } else {
    ticker = paste("YAHOO/AX_",code, sep="")
  }
  
  # Check if the date is blank. If it is blank run without date modifiers.
  # In both instances a second logic tier is added to check if a data frequency modifier should be added.
  # then return the stock and index into their respective variables
  if (dateFrom == "" | dateTo == "") {
    if (dataFreq == "") {
      stock = Quandl(ticker)
    } else {
      stock = Quandl(ticker, collapse = dataFreq)
    }
  } else {
    if (dataFreq == "") {
      stock = Quandl(ticker, start_date=dateFrom, end_date=dateTo)
    } else {
      stock = Quandl(ticker, start_date=dateFrom, end_date=dateTo, collapse = dataFreq)
    }
  }
  return(stock)
}

# prompt the user for the index code
indexCode <- readline(prompt = "Enter the code of index (e.g. AXJO): ")

# prompt the user for a start and end date

dateFrom <- readline(prompt = "Enter a start date (enter to skip) format=yyyy-mm-dd : ")
dateTo <- readline(prompt = "Enter an end date (enter to skip) format=yyyy-mm-dd : ")

# promt the user to change the data frequency. From daily to weekly, monthly etc

dataFreq <- readline(prompt = "Would you like to change the data frequency (blank=daily, else enter 'weekly|monthly|quarterly|annual'): ")

# get the list of ASX200 constituents and add a blank column for beta
dataTable <- readASX200()
dataTable[,"Beta"] <- NA

# get the data for the index
index = getStockData(indexCode, dateFrom, dateTo, dataFreq, index=TRUE)
cat("Successfully loaded the data for the index: ", indexCode, "\n")

for (i in 1:nrow(dataTable)) {
  
  # Add a try catch to catch errors in ticker code. Will output a NaN to the table when it cant find the stock.
  beta <- tryCatch({
    # get the data for the stock
    stockCode = dataTable[i, "Ticker"]
    
    stock = getStockData(stockCode, dateFrom, dateTo, dataFreq)
    
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
    coefficients(fit)[2]
  }, error=function(e) {
    return(NA)
  })
  
  dataTable[i, "Beta"] = beta
  
  if (is.na(beta) == TRUE) {
    cat("Unsuccessful beta for: ", stockCode, "\n")
  } else {
    cat("Successfully calcuated the beta for: ", stockCode, "\n")
  }
}

print(dataTable)