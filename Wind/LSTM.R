library(dplyr)
library(ggthemes)
options(kableExtra.latex.load_packages = FALSE)
require(kableExtra)
library(dynlm)
library(ggplot2)
library(tidyr)
library(tseries)
library(TSA)
library(forecast)
library(gridExtra)
library(corrplot)
library(data.table)
library(quantmod)
library(plotly)
library(GGally)
library(data.table)
library(kableExtra)
library(plotly)
library(caret)
library(xgboost)
library(dplyr)
library(lubridate)
library(DT)
library(xts)
library(e1071)
library(doParallel)
library(keras)
library(tensorflow)
library(knitr)

require(quantmod)
DDAIF = getSymbols("DDAIF", from = "2010-05-01", to = "2019-05-01", auto.assign = FALSE)
DDAIF2 = getSymbols("DDAIF", from = "2019-04-22", to = "2019-05-01", auto.assign = FALSE)

################################################

DDAIF_log_returns <- DDAIF %>% Ad() %>% dailyReturn(type = "log")

DDAIF %>% Ad() %>% chartSeries()

DDAIF %>% chartSeries(TA = "addBBands();addVo();addMACD()", subset = "2018")


require(ggthemes)
require(ggplot2)
require(plotly)
p1 <- ggplot(DDAIF, aes(x = index(DDAIF), y = DDAIF[, 1])) + geom_line(color = "yellow") + 
  ggtitle("Daimler stock Opening Prices") + xlab("Date") + ylab("Opening Prices") + 
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") + theme_solarized(light = FALSE) + 
  theme(axis.text = element_text(size = 10, angle = 90), plot.title = element_text(size = 11, 
                                                                                   color = "yellow", hjust = 0.5))

ggplotly(p1)

p2 <- ggplot(DDAIF, aes(x = index(DDAIF), y = DDAIF[, 6])) + geom_line(color = "yellow") + 
  ggtitle("Daimler Adjusted Closing Prices") + xlab("Date") + ylab("Adjusted Closing Prices") + 
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") + theme_solarized(light = FALSE) + 
  theme(axis.text = element_text(size = 10, angle = 90), plot.title = element_text(size = 11, 
                                                                                   color = "yellow", hjust = 0.5))

ggplotly(p2)

require(gridExtra)
options(repr.plot.width = 10, repr.plot.height = 10)
popen = ggplot(DDAIF, aes(DDAIF.Open)) + geom_histogram(bins = 50, aes(y = ..density..), 
                                                        col = "yellow", fill = "yellow", alpha = 0.2) + geom_density()
phigh = ggplot(DDAIF, aes(DDAIF.High)) + geom_histogram(bins = 50, aes(y = ..density..), 
                                                        col = "blue", fill = "red", alpha = 0.2) + geom_density()
plow = ggplot(DDAIF, aes(DDAIF.Low)) + geom_histogram(bins = 50, aes(y = ..density..), 
                                                      col = "red", fill = "red", alpha = 0.2) + geom_density()
pclose = ggplot(DDAIF, aes(DDAIF.Close)) + geom_histogram(bins = 50, aes(y = ..density..), 
                                                          col = "black", fill = "red", alpha = 0.2) + geom_density()
grid.arrange(popen, phigh, plow, pclose, nrow = 2, ncol = 2)

# We create a response variable. To predicting future days price, we apply
# lag function in the price change

# We Calculate the various moving averages (MA) of a series for volume. For
# the past 10, 20 , 60 days
require(TTR)
DDAIF$Avg_volume_10 <- SMA(DDAIF$DDAIF.Volume, n = 10)
DDAIF$Avg_volume_20 <- SMA(DDAIF$DDAIF.Volume, n = 20)
DDAIF$Avg_volume_60 <- SMA(DDAIF$DDAIF.Volume, n = 60)

# We calculate the % of the average volume of the above days
DDAIF$Volume_perc_avg_10 <- (DDAIF$DDAIF.Volume/DDAIF$Avg_vol_10) * 100
DDAIF$Volume_perc_avg_20 <- (DDAIF$DDAIF.Volume/DDAIF$Avg_vol_20) * 100
DDAIF$Volume_perc_avg_60 <- (DDAIF$DDAIF.Volume/DDAIF$Avg_vol_60) * 100

# We calculate the range between high and low
DDAIF$Range <- DDAIF$DDAIF.High - DDAIF$DDAIF.Low

# % change of closing price.
DDAIF$perc_change_closing <- (DDAIF$DDAIF.Close - lag(DDAIF$DDAIF.Close))/lag(DDAIF$DDAIF.Close) * 
  100

# Range between prior days closing price and todays closing price
DDAIF$change_from_yest <- DDAIF$DDAIF.Close - lag(DDAIF$DDAIF.Close)

# We Calculate again the various moving averages (MA) for range now . For
# the past 10, 20 , 60 days
DDAIF$moving_avg_10 <- SMA(DDAIF$Range, n = 10)
DDAIF$moving_avg_20 <- SMA(DDAIF$Range, n = 20)
DDAIF$moving_avg_60 <- SMA(DDAIF$Range, n = 60)

# We calculate the % of the average range of the above days
DDAIF$perc_moving_avg_10 <- (DDAIF$Range/DDAIF$moving_avg_10) * 100
DDAIF$perc_moving_avg_20 <- (DDAIF$Range/DDAIF$moving_avg_20) * 100
DDAIF$perc_moving_avg_60 <- (DDAIF$Range/DDAIF$moving_avg_60) * 100

# The tot amount of money traded multiplied by the volume (in dollars)
DDAIF$cash_tradet <- DDAIF$DDAIF.Close * DDAIF$DDAIF.Volume

# The average volume of cash trated for the same periods as above
DDAIF$avg_cash_trated_10 <- SMA(DDAIF$cash_tradet, n = 10)
DDAIF$avg_cash_trated_20 <- SMA(DDAIF$cash_tradet, n = 20)
DDAIF$avg_cash_trated_60 <- SMA(DDAIF$cash_tradet, n = 60)

# The % of the avgo volume today.
DDAIF$Avg_Dollar_volume_pct_10 <- (DDAIF$cash_tradet/DDAIF$avg_cash_trated_10) * 
  100
DDAIF$Avg_Dollar_volume_pct_20 <- (DDAIF$cash_tradet/DDAIF$avg_cash_trated_20) * 
  100
DDAIF$Avg_Dollar_volume_pct_60 <- (DDAIF$cash_tradet/DDAIF$avg_cash_trated_60) * 
  100

# Todays open vs Yesterdays Close.
require(data.table)
require(dplyr)

DDAIF$nightgap <- DDAIF$DDAIF.Open - lag(DDAIF$DDAIF.Close)

# The Gap % win or loss from yesterday closing prices
DDAIF$night_gap_perc <- (DDAIF$DDAIF.Open - lag(DDAIF$DDAIF.Close))/lag(DDAIF$DDAIF.Close) * 
  100
DDAIF$perc_range_previous = abs((DDAIF$DDAIF.Close - DDAIF$DDAIF.Open)/(DDAIF$DDAIF.High - 
                                                                          DDAIF$DDAIF.Low) * 100)
DDAIF$perc_range_atpr = (DDAIF$Range/DDAIF$DDAIF.Close) * 100
DDAIF$perc_range_williams = (DDAIF$DDAIF.High - DDAIF$DDAIF.Close)/(DDAIF$DDAIF.High - 
                                                                      DDAIF$DDAIF.Low) * 100
# Compute range for 1 Month
require(zoo)
one_month_range_perc <- rollapply(DDAIF$DDAIF.High, 20, max) - rollapply(DDAIF$DDAIF.Low, 
                                                                         20, max)

DDAIF$one_month_range_perc = (DDAIF$DDAIF.Close - DDAIF$DDAIF.Low)/one_month_range_perc * 
  100
gc()  #clean RAM

# Moving averages smooth the price data to form a trend following indicator.
# They
require(TTR)
DDAIF$EMA10 <- EMA(DDAIF$DDAIF.Low, n = 10)
DDAIF$EMA20 <- EMA(DDAIF$DDAIF.Low, n = 20)
# Weighted Moving Average
DDAIF$EMA60 <- EMA(DDAIF$DDAIF.Low, n = 60)
# Double Exponential Moving Average is a measure of a security's trending
# average
DDAIF$WMA10 <- WMA(DDAIF$DDAIF.Low, n = 10)
# The EVWMA uses the volume to declare the period of the MA.
DDAIF$EVWMA10 <- EVWMA(DDAIF$DDAIF.Low, DDAIF$DDAIF.Volume)
# Zero Lag Exponential Moving Average (ZLEMA) As is the case with the double
DDAIF$ZLEMA10 <- ZLEMA(DDAIF$DDAIF.Low, n = 10)
# Volume weighted average price (VWAP) and moving volume weighted average
# price
DDAIF$VWAP10 <- VWAP(DDAIF$DDAIF.Low, DDAIF$DDAIF.Volume)
# The Hull Moving Average (HMA), developed by Alan Hull, is an extremely
# fast
DDAIF$HMA10 <- HMA(DDAIF$DDAIF.Low, n = 20)
# The ALMA moving average uses curve of the Normal (Gauss) distribution
# which
DDAIF$ALMA10 <- ALMA(DDAIF$DDAIF.Low, n = 9, offset = 0.85, sigma = 6)
# DDAIF <- DDAIF[complete.cases(DDAIF), ]
write.csv(DDAIF, file = "DDAIF_with_TI.csv", row.names = F)

# Augmented Dickey-Fuller test AND correlation test
require(tseries)
# From package tseries we use the () adf.test- Computes the Augmented
# Dickey-Fuller test for the null that x has a unit root.
adf.test(DDAIF$DDAIF.Adjusted)
# The result shows that we need to move stationarity
require(forecast)
require(xts)
require(e1071)
require(doParallel)
require(dynlm)
require(caret)
require(dynlm)

DDAIF_lm <- na.omit(DDAIF)  #we handle missing values
set.seed(123)  #algorithm for reproducability
X <- DDAIF_lm[, -6]
y <- DDAIF_lm[, 6]

# We scale the variables in order to run the models
X.scaled <- scale(X)
gc()  #clean RAM

# We merge them back
DDAIF_lm <- cbind(X.scaled, y)

# create index
numerical_Vars <- which(sapply(DDAIF_lm, is.numeric))

# save the vector
numerical_VarNames <- names(numerical_Vars)
cat("They exist", length(numerical_Vars), "numerical variables.\n")

sum_numVar <- DDAIF_lm[, numerical_Vars]

####################################

require(corrplot)

corrplot.mixed(correl_numVar, tl.col = "black", tl.pos = "lt", number.cex = 0.5)

###################################


# We remove the highly correlated variables to avoid overfitting of models
del <- cor(DDAIF_lm)
del[upper.tri(del)] <- 0
diag(del) <- 0

DDAIF_lm <- DDAIF_lm[, !apply(del, 2, function(x) any(x > 0.9))]

#------------------------------------------
# We create our Train and Test Datasets
#------------------------------------------
# For next day forecast n = days_forecast + 1. If you want more days change
# the
#+1
days_to_forecast = 7
n = days_to_forecast + 1
X_train = DDAIF_lm[1:(nrow(DDAIF_lm) - (n - 1)), -17]
# Our dependent var: Is the price adj
y_train = DDAIF_lm[n:nrow(DDAIF_lm), 17]
X_test = DDAIF_lm[((nrow(DDAIF_lm) - (n - 2)):nrow(DDAIF_lm)), -17]
ourdate <- time(DDAIF2)

require(quantmod)
# We create the validation test of the real prices of the next 7 days Adapt
# dates according to your n days of forecast
DDAIF2 = getSymbols("DDAIF", from = "2019-04-22", to = "2019-05-01", auto.assign = FALSE)

y_test <- as.numeric(DDAIF2$DDAIF.Adjusted)

train <- cbind(X_train, y_train)
# check the number of features
dim(X_train)
dim(X_test)

######################################

ker = ncol(X_train)


#######################################

keras_model <- keras_model_sequential() 

keras_model %>% 
  #We ddd a densely-connected NN layer to an output
  #ReLU (Rectified Linear Unit) Activation Function
  layer_dense(units = 60, activation = 'relu', input_shape = ker) %>% 
  layer_dropout(rate = 0.2) %>% #We apply dropout  to prevent overfitting
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'linear')

keras_model %>% compile(optimizer = "rmsprop", loss = "mse", metrics = "mse")

keras_history <- keras_model %>% fit(X_train, y_train, epochs = 200, batch_size = 28, 
                                     validation_split = 0.1)

keras_pred <- keras_model %>% predict(X_test, batch_size = 28)

plot(keras_history)
