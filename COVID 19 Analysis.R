library(covid19.analytics)
library(tidyverse)
library(lubridate)
library(prophet)

#Data

total_cs = covid19.data(case = "ts-confirmed")  

total_cs = filter(total_cs,total_cs$Country.Region == "India")

total_cs = data.frame(t(total_cs))

total_cs = cbind(rownames(total_cs),data.frame(total_cs,row.names = NULL))

colnames(total_cs) = c("Date","Confirmed")

total_cs = total_cs[-c(1:4),]

str(total_cs)

total_cs$Date = ymd(total_cs$Date)

total_cs$Confirmed = as.numeric(total_cs$Confirmed)

#Plot

qplot(Date,log(Confirmed),data = total_cs,main = "COVID confirmed cases")

ds = total_cs$Date

y = total_cs$Confirmed

df = data.frame(ds,y)

#Forecasting

m = prophet(df)

# Prediction

future = make_future_dataframe(m,periods = 28)

forecast = predict(m,future)

# Plot forecast

dyplot.prophet(m,forecast)

# Forecast Components

prophet_plot_components(m,forecast)

# Model Performance

pred = forecast$yhat[1:101]

actual = m$history$y

plot(pred,actual)

abline(lm(pred ~ actual), col = "red")

summary(lm(pred ~ actual))




