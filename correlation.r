
# This script calculates the beta of stocks in the ASX200 using data from yahoo finance (via quandl).

# required libraries
require(ggplot2)
require(Quandl)
require(rvest)
require(xts)
require(corrplot)

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

readASX20 <- function() {
  # read html from ASX200 list website
  asxPage <- read_html("http://dividend.net.au/asx-20-index/")
  
  # extract the table from the page
  asxTable <- asxPage %>%
    html_nodes("table") %>%
    html_table()
  asxTable
  
  # extract the second table on the page then delete the first row (contains the tables headers)
  asxTable = asxTable[[1]]
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

stockCorr <- function(tickers) {
  # prompt the user for a start and end date
  
  dateFrom <- readline(prompt = "Enter a start date (enter to skip) format=yyyy-mm-dd : ")
  dateTo <- readline(prompt = "Enter an end date (enter to skip) format=yyyy-mm-dd : ")
  
  # promt the user to change the data frequency. From daily to weekly, monthly etc
  
  dataFreq <- readline(prompt = "Would you like to change the data frequency (blank=daily, else enter 'weekly|monthly|quarterly|annual'): ")
  
  
  for (i in 1:nrow(tickers)) {
    
    # Add a try catch to catch errors in ticker code. Will output a NaN to the table when it cant find the stock.
    tryCatch({
      # get the data for the stock
      stockCode = tickers[i,]
      
      stock = getStockData(stockCode, dateFrom, dateTo, dataFreq)
      
      # calculate the return of each stock
      stockReturn = percReturn(stock[["Adjusted Close"]])
      lsDate = stock[["Date"]]
      lsDate = lsDate[2:length(lsDate)]
      
      # Extract the adjusted closing stocks from both and add to a temp variable
      temp = data.frame(Date=lsDate, stockCode=stockReturn)
      names(temp) = c("Date", stockCode)
      
      if (i==1) {
        dataTable = temp
      } else {
        dataTable = merge(dataTable, temp, all=FALSE)
      }
      
      # calculate the linear regression then store the coefficent
      cat("Successfully added data for: ", stockCode, "\n")
      
    }, error=function(e) {
      cat("Unsuccessful for: ", stockCode, "\n")
    })
    
  }
  
  # create the correlation matrix for the stocks and plot
  
  corrMatrix = cor(dataTable[,-1])
  corrplot(corrMatrix, method = "circle", order = "hclust", addrect = 5)
  
  return(corrMatrix)
}