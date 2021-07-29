# Fetching data from yahoo with quantmod - Data Scrapping  from Yahoo Finance
library(quantmod)
# Import lib
library(forecast)
library(tseries)

novartis = getSymbols("NVS", auto.assign=F, 
                      from = "2015-01-01", to = "2016-01-01")
HDFCBank = getSymbols("HDB", auto.assign=F,
                      from = "2015-01-01", to = "2021-01-01")

View(HDFCBank)
dim(HDFCBank) # 1511 rows and 6 columns
# dim(novartis) # 252 rows and 6 columns
# Structure of data
str(HDFCBank)
# str(novartis)

# functions to explore unprocessed xts from quantmod
plot(as.ts(HDFCBank$HDB.Close))
chartSeries(HDFCBank, type = "line")


# Ets model
HDFCBank_ets = ets(HDFCBank$HDB.Close)

# Forecast ets
plot(forecast(HDFCBank_ets, h = 20))


# Conversion to dataframe
HDFCBank = as.data.frame(HDFCBank)

# Structure of data
str(HDFCBank)

# Datatype of date columns
class(HDFCBank$Date[1])

# Adding the rownames as date
HDFCBank$Date = rownames(HDFCBank)
HDFCBank$Date = as.Date(HDFCBank$Date)
head(HDFCBank)

# 'from' and 'to' with as.Date to make sure this is a date format
mydates = seq.Date(from = as.Date("2015-01-01"), 
                   to = as.Date("2021-01-01"), 
                   by = 1)

# Converting to a df (required for the merge)
mydates = data.frame(Date = mydates)

# Padding with 'mydates'
HDFCBank = merge(HDFCBank, mydates, by = "Date", all.y = T)


# Lowest date should be at the top and highest date should be at the bottom
# just to verify
HDFCBank <- HDFCBank[order(HDFCBank$Date), ]  
head(HDFCBank)


# We need only time column and value col
HDFCBank1 <- subset(HDFCBank, select = c(Date, HDB.Close))

# Change column names to something more meaningful
colnames(HDFCBank1) <- c("Date", "Price")

# Removing initial days to start on monday
HDFCBank1 = HDFCBank1[-(1:4),]

nrow(HDFCBank1)  # Total obs in TS data # 2189

rownames(HDFCBank1) <- 1:nrow(HDFCBank1)

# Removing sundays, watch the from as the first one to remove
HDFCBank1 = HDFCBank1[-(seq(from = 7, to = nrow(HDFCBank1), by = 7)),]
# Removing saturdays
HDFCBank1 = HDFCBank1[-(seq(from = 6, to = nrow(HDFCBank1), by = 6)),]

dim(HDFCBank1)
# Using last observatoin carried forward -  imputation
HDFCBank1 = na.locf(HDFCBank1)

rownames(HDFCBank1) <- 1:nrow(HDFCBank1)

dim(HDFCBank1)

# Train and test data preparation 
stock   <- HDFCBank1$Price[700:1700]  # Picking first 900 obs and putting them in test set 
stock
lnstock <- log(HDFCBank1$Price[1:1700])   # log reduces the overall spread of the data 
lnstock

# acf and pacf to get an idea about autocorrelation
ggtsdisplay(HDFCBank1$Price)

# ANOTHER Method for ACF AND PACF
# Desire to fit ARIMA model
# MA Term
acf(lnstock, lag.max  = 100, na.action = na.pass) # I want to look 20 time period back, Difficult to find MA term

# AR Term
pacf(lnstock, lag.max = 20,na.action = na.pass)  # (AR,1)

# Check if time series is stationary or not- Dickey-Fuller Test
adf.test(lnstock) # P-value is high, TS in not-stationary , p-value = 0.7368

class(lnstock) # Numeric
# Convert numeric vector in a TS object 
pricearima <- ts(stock, frequency = 365)
class(pricearima)
plot(pricearima)

# Automatic way of finding the value of p,d,q ( ARIMA)
fitlnstock <- auto.arima(pricearima, stepwise = T, 
                         approximation = F, 
                         trace = T)
fitlnstock


# Doing Forecasting on the outcome of autoarima
forecastvalues_ln <- forecast(fitlnstock, 1701:2189)   
forecastvalues_ln 
forecastvalues_ln$x  # Point forecast
exp(forecastvalues_ln$x) # Actual forecasted values # taking exponential
plot(forecastvalues_ln)

forecastvaluesextreacted <- as.numeric(forecastvalues_ln$mean)
finalforecastvalue <- exp(forecastvaluesextreacted)
finalforecastvalue

df <- data.frame(HDFCBank1$Price[1701:2189], finalforecastvalue)
View(df)
colnames(df) <- c("Actual Values", "forecasted Price")


percentage_error <- ((df$`Actual Values` - df$`forecasted Price`)/df$`Actual Values`)
percentage_error

mean(percentage_error)  # 0.15 

library(prophet)

colnames(HDFCBank1) <- c("ds", "y")
m <- prophet(HDFCBank1)

future <- make_future_dataframe(m, periods = 365)
future11 <- predict(m, future)
plot(m, future11)
