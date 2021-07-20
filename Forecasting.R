# Fetching data from yahoo with quantmod
library(quantmod)

novartis = getSymbols("NVS", auto.assign=F, 
                      from = "2015-01-01", to = "2016-01-01")
HDFCBank = getSymbols("HDB", auto.assign=F,
                      from = "2020-01-01", to = "2021-01-01")

# use argument return.class to modify output class to ts or mts
## using a column like a standard ts
plot(as.ts(novartis$NVS.Open))
plot(as.ts(HDFCBank$HDB.Open))
# functions to explore unprocessed xts from quantmod
chartSeries(novartis, type = "line")
chartSeries(novartis, type = "line")
# acf and pacf to get an idea about autocorrelation
install(forecast)
library(forecast)
ggtsdisplay(novartis$NVS.Open)

# Arima model
novartisarima = auto.arima(novartis$NVS.Open, 
                           stepwise = T, 
                           approximation = F, 
                           trace = T); novartisarima

# Alternative arima with autoregressive part 
novartisarima2 = Arima(novartis$NVS.Open, order = c(1,1,1))
novartisarima2


# Forecast arima
plot(forecast(novartisarima, h = 20))
plot(forecast(novartisarima2, h = 20))

# Ets model
novartisets = ets(novartis$NVS.Open)

# Forecast ets
plot(forecast(novartisets, h = 20))

## Getting a regular time series

# Conversion to dataframe
novartis = as.data.frame(novartis)

### Adding the rownames as date
novartis$Date = rownames(novartis)
novartis$Date = as.Date(novartis$Date)
head(novartis)

# Creating the date column, 'by' can be either nr days or integer
# 'from' and 'to' with as.Date to make sure this is a date format
mydates = seq.Date(from = as.Date("2015-01-01"), 
                   to = as.Date("2016-01-01"), 
                   by = 1)

# Converting to a df (required for the merge)
mydates = data.frame(Date = mydates)

# Padding with 'mydates'
mydata = merge(novartis, mydates, by = "Date", all.y = T)

# Removing initial days to start on monday
mydata = mydata[5:366,]

# Removing sundays, watch the from as the first one to remove
mydata = mydata[-(seq(from = 7, to = nrow(mydata), by = 7)),]
# Removing saturdays
mydata = mydata[-(seq(from = 6, to = nrow(mydata), by = 6)),]

# Using last observatoin carried forward -  imputation
mydata = na.locf(mydata)

## Which days are the ones best to buy or sell?

# Putting the closeprice into a weekly time series
highestprice = ts(as.numeric(mydata$NVS.High), 
                  frequency = 5)

# Various plots
seasonplot(highestprice, season.labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))
monthplot(highestprice)
monthplot(highestprice, base = median, col.base = "red")
plot(stl(highestprice, s.window = "periodic"))

# Comparison with the low prices
par(mfrow = c(1,2))
lowestprice = ts(as.numeric(mydata$NVS.Low), 
                 frequency = 5)
monthplot(lowestprice, base = median, col.base = "red")
monthplot(highestprice, base = median, col.base = "red")
par(mfrow = c(1,1))