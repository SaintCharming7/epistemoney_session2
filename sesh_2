# epistemoney_session2
Codes (R) used for the article: Testing the 'best' sectors tot Invest In heading Into 2023. Last thoughts.

# uploading the libraries used for this exercise
library(dygraphs)
library(corrplot)
library(RColorBrewer)
library(Hmisc)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(gridExtra)

#quarterly returns fucntion
quarterly_returns <- function(ticker, base_year)
{
  # Obtain stock price data from Yahoo! Finance
  stock <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE) 
  # Remove missing values
  stock <- na.omit(stock)
  # Keep only adjusted closing stock prices
  stock <- stock[, 6]
  
  # Confine our observations to begin at the base year and end at the last available trading day
  horizon <- paste0(as.character(base_year), "/", as.character(Sys.Date()))
  stock <- stock[horizon]
  
  # Calculate monthly arithmetic returns
  data <- periodReturn(stock, period = "quarterly", type = "arithmetic")
  
  # Assign to the global environment to be accessible
  assign(ticker, data, envir = .GlobalEnv)
}

#telecommunications
# Call our function for each stock
quarterly_returns("TMUS", 2019)
quarterly_returns("ABBV", 2019)
quarterly_returns("NOW", 2019)
quarterly_returns("VZ", 2019)

# Get NYSE Data
quarterly_returns("^NYA", 2019)

# Merge all the data and rename columns
returns <- merge.xts(TMUS, ABBV, NOW, VZ,`^NYA`)
colnames(returns) <- c("TMUS", "ABBV", "NOW","VZ","^NYA")

# Assign weights
wts <- c(1/4, 1/4, 1/4, 1/4)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returns <- Return.portfolio(R = returns[,1:4], weights = wts, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returns <- Return.portfolio(R = returns[,5], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

comp <- merge.xts(portfolio_returns, benchmark_returns)
colnames(comp) <- c("Portfolio", "Benchmark")


# Build an interactive graph to compare performance
telec <- dygraph(comp, main = "Telecommunications (pft) vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")

#internet services
# Call our function for each stock
quarterly_returns("SPGI", 2019)
quarterly_returns("MCO", 2019)
quarterly_returns("MSCI", 2019)
quarterly_returns("ICE", 2019)

# Get NYSE Data
#quarterly_returns("^NYA", 2019)

# Merge all the data and rename columns
returnsb <- merge.xts(SPGI, MCO, MSCI, ICE,`^NYA`)
colnames(returnsb) <- c("SPGI", "MCO", "MSCI","ICE","^NYA")

# Assign weights
#wts <- c(1/4, 1/4, 1/4, 1/4)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returnsb <- Return.portfolio(R = returnsb[,1:4], weights = wts, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returnsb <- Return.portfolio(R = returnsb[,5], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

compb <- merge.xts(portfolio_returnsb, benchmark_returnsb)
colnames(compb) <- c("Portfolio", "Benchmark")


# Build an interactive graph to compare performance
iserv <- dygraph(compb, main = "IServices (pft) vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")


# S. distribution
# Call our function for each stock
quarterly_returns("BABA", 2019)
quarterly_returns("SE", 2019)
quarterly_returns("VIPS", 2019)
quarterly_returns("CVNA", 2019)

# Get NYSE Data
#quarterly_returns("^NYA", 2019)

# Merge all the data and rename columns
returnsc <- merge.xts(BABA, SE, VIPS, CVNA,`^NYA`)
colnames(returnsc) <- c("BABA", "SE", "VIPS","CVNA","^NYA")

# Assign weights
#wts <- c(1/4, 1/4, 1/4, 1/4)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returnsc <- Return.portfolio(R = returnsc[,1:4], weights = wts, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returnsc <- Return.portfolio(R = returnsc[,5], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

compc <- merge.xts(portfolio_returnsc, benchmark_returnsc)
colnames(compc) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
sdist <- dygraph(compc, main = "Sdistribution (pft) vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")

# Oil & Gas
# Call our function for each stock
quarterly_returns("XOM", 2019)
quarterly_returns("CVX", 2019)
quarterly_returns("BP", 2019)
quarterly_returns("PBR", 2019)

# Get NYSE Data
#quarterly_returns("^NYA", 2019)

# Merge all the data and rename columns
returnsd <- merge.xts(XOM, CVX, BP, PBR,`^NYA`)
colnames(returnsd) <- c("XOM", "CVX", "BP","PBR","^NYA")

# Assign weights
#wts <- c(1/4, 1/4, 1/4, 1/4)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returnsd <- Return.portfolio(R = returnsd[,1:4], weights = wts, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returnsd <- Return.portfolio(R = returnsd[,5], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

compd <- merge.xts(portfolio_returnsd, benchmark_returnsd)
colnames(compd) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
oilg <- dygraph(compd, main = "Oil & Gas (pft) vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")

#banking
# Call our function for each stock
quarterly_returns("JPM", 2019)
quarterly_returns("BAC", 2019)
quarterly_returns("C", 2019)
quarterly_returns("CS", 2019)

# Get NYSE Data
#quarterly_returns("^NYA", 2019)

# Merge all the data and rename columns
returnse <- merge.xts(JPM, BAC, C, CS,`^NYA`)
colnames(returnse) <- c("JPM", "BAC", "C","CS","^NYA")

# Assign weights
#wts <- c(1/4, 1/4, 1/4, 1/4)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returnse <- Return.portfolio(R = returnse[,1:4], weights = wts, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returnse <- Return.portfolio(R = returnse[,5], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

compe <- merge.xts(portfolio_returnse, benchmark_returnse)
colnames(compe) <- c("Portfolio", "Benchmark")


# Build an interactive graph to compare performance
bank <- dygraph(compe, main = "Banking (pft) vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")

# Business Services
# Call our function for each stock
quarterly_returns("V", 2019)
quarterly_returns("MA", 2019)
quarterly_returns("AXP", 2019)
quarterly_returns("COF", 2019)

# Get NYSE Data
#quarterly_returns("^NYA", 2019)

# Merge all the data and rename columns
returnsf <- merge.xts(V, MA, AXP, COF,`^NYA`)
colnames(returnsf) <- c("V", "MA", "AXP","COF","^NYA")

# Assign weights
#wts <- c(1/4, 1/4, 1/4, 1/4)

# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returnsf <- Return.portfolio(R = returnsf[,1:4], weights = wts, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returnsf <- Return.portfolio(R = returnsf[,5], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

compf <- merge.xts(portfolio_returnsf, benchmark_returnsf)
colnames(compf) <- c("Portfolio", "Benchmark")


# Build an interactive graph to compare performance
bserv <- dygraph(compf, main = "B. Services (pft) vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")


# arranging all the portfolios
telec
iserv
sdist
oilg
bank
bserv

# big portfolio (24 stocks)

# Call our function for each stock
quarterly_returns("TMUS", 2019)
quarterly_returns("ABBV", 2019)
quarterly_returns("NOW", 2019)
quarterly_returns("VZ", 2019)
quarterly_returns("SPGI", 2019)
quarterly_returns("MCO", 2019)
quarterly_returns("MSCI", 2019)
quarterly_returns("ICE", 2019)
quarterly_returns("BABA", 2019)
quarterly_returns("SE", 2019)
quarterly_returns("VIPS", 2019)
quarterly_returns("CVNA", 2019)
quarterly_returns("XOM", 2019)
quarterly_returns("CVX", 2019)
quarterly_returns("BP", 2019)
quarterly_returns("PBR", 2019)
quarterly_returns("JPM", 2019)
quarterly_returns("BAC", 2019)
quarterly_returns("C", 2019)
quarterly_returns("CS", 2019)
quarterly_returns("V", 2019)
quarterly_returns("MA", 2019)
quarterly_returns("AXP", 2019)
quarterly_returns("COF", 2019)

# Merge all the data and rename columns
returnsg <- merge.xts(TMUS, ABBV, NOW, VZ, SPGI, MCO, MSCI, ICE, BABA, SE, VIPS, CVNA,
                      XOM, CVX, BP, PBR, JPM, BAC, C, CS, V, MA, AXP, COF,`^NYA`)
colnames(returnsg) <- c("TMUS", "ABBV", "NOW","VZ","SPGI","MCO","MSCI","ICE","BABA",
                        "SE", "VIPS", "CVNA","XOM","CVX","BP","PBR","JPM","BAC",
                        "C","CS","V","MA","AXP","COF","^NYA")

# Assign weights
#wts <- c(1/4, 1/4, 1/4, 1/4)
wtsa <- c(1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24,
          1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24, 1/24)
# Construct a portfolio using our returns object and weights
# Only select first three columns to isolate our individual stock data
portfolio_returnsg <- Return.portfolio(R = returnsg[,1:24], weights = wtsa, wealth.index = TRUE)

# isolating the NYSE data
benchmark_returnsg <- Return.portfolio(R = returnsg[,25], wealth.index = TRUE)

# Merge both (portfolio and benchmark)

compg <- merge.xts(portfolio_returnsg, benchmark_returnsg)
colnames(compg) <- c("Portfolio", "Benchmark")

# Build an interactive graph to compare performance
dygraph(compg, main = " 24 stock portfolio vs. Benchmark") %>%
  dyAxis("y", label = "Amount ($)")

