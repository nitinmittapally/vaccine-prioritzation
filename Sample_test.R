library(forecast)
library(readxl)
library(stargazer)
library(fpp)
library(fpp2)
library(scales)
library(quantmod)
library(urca)
library(vars)
library(tseries)
library(ggplot2)
library(dplyr)
library(TSA)

data <- read_excel("C:/Users/challasa/Documents/Shravya/OneDrive/IOWA/original.xlsx", sheet = 5)

data_dtp3 <- filter(data, data$vaccine == "DTP3")

data_dtp3$coverage <- as.numeric(data_dtp3$coverage)
data_dtp3$coverage[is.na(data_dtp3$coverage)] <- 0

dtp3_orig <- filter(data_dtp3, data_dtp3$year < 2017)

plot(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage)
ts.plot(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage)
acf(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage)
pacf(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage)

# time series is not stationary
adf.test(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage)
test <- ur.df(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage,
              type = "trend", lags = 3)
summary(test)


ts.plot(diff(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage))

fit <- Arima(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage, order = c(0,1,2))
pacf(diff(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage))
acf(diff(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage))
coeftest(fit)
AIC(fit)
BIC(fit)
fit
prediction = forecast(fit, 10)
plot(prediction)

fit1 <- Arima(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage, order = c(0,1,0))
coeftest(fit1)
aic1 <- AIC(fit1)
bic1 <- BIC(fit1)
c(aic1, bic1)

fit2 <- arima(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage, order = c(1,2,0))
coeftest(fit2)
aic2 <- AIC(fit2)
bic2 <- BIC(fit2)
c(aic2, bic2)


fit3 <- arima(dtp3_orig[dtp3_orig$iso_code == "AFG", ]$coverage, order = c(1,1,1))
coeftest(fit3)
aic3 <- AIC(fit3)
bic3 <- BIC(fit3)
c(aic3, bic3)

for (ctry in dtp3_orig$iso_code) {
  fit1 <- arima(dtp3_orig[dtp3_orig$iso_code == ctry, ]$coverage, order = c(1,1,0))
  coeftest(fit1)
  aic1 <- AIC(fit1)
  bic1 <- BIC(fit1)
  c(aic1, bic1)
  
  fit2 <- arima(dtp3_orig[dtp3_orig$iso_code == ctry, ]$coverage, order = c(1,2,0))
  coeftest(fit2)
  aic2 <- AIC(fit2)
  bic2 <- BIC(fit2)
  c(aic2, bic2)
  
  
  fit3 <- arima(dtp3_orig[dtp3_orig$iso_code == ctry, ]$coverage, order = c(1,1,1))
  coeftest(fit3)
  aic3 <- AIC(fit3)
  bic3 <- BIC(fit3)
  c(aic3, bic3)
}


