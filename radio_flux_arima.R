librray(tidyverse)
library(forecast)
library(quantmod)
library(tsibble)
##
rm(list=ls())
df_abs <- data.table::fread("../db/radio_flux_abs.txt")
df_abs$Ymd <- as.Date(paste(df_abs$year, df_abs$month, df_abs$day, sep = "-"))
df_abs <- df_abs %>% select(Ymd,f10.7) 
colnames(df_abs) <- c("Ymd","y")
# make time series tibble
ts_abs <-as_tsibble(df_abs,index=Ymd) %>% fill_gaps(y=11)
short_list <- ts_abs %>% filter(Ymd >="2008-01-01" & Ymd <="2013-12-31")
plot(short_list,type="l")
fit <- auto.arima(short_list$y,ic="bic")
plot(fitted(fit),col="red",type="l")
lines(short_list$y,type="l")
fit.forecast <- forecast(fit,h=60)
plot(fit.forecast,type="l")
fcast <- data.table::as.data.table(fit.forecast)
##
pdates <- seq(as.Date("2014/1/1"), as.Date("2014/3/1"), "days")
pdates <- data.table::as.data.table(pdates)
fcast <- cbind(fcast,pdates)
colnames(fcast) <- c("Pred","L80","H80","L95","H95","Ymd")
ggplot(fcast) + geom_line(aes(x=Ymd,y=Pred))
##
## forecast Sunspots
## 
isn <- data.table:: fread("../db/kh_spots.csv")
isn$Ymd <- as.Date(isn$Ymd)
isn$Spots <- isn$s_n + isn$s_s
isn$Spots <- ifelse(isn$Spots ==0,11,isn$Spots)
isn$ma <- ma(isn$Spots,order=90)
isn <- isn %>% filter(Ymd >="2008-01-01" & Ymd <="2013-12-31") %>%
  select(Ymd,ma)
colnames(isn) <- c("Ymd","y")
plot(isn,type="l")

ts_isn <- as_tsibble(isn,index=Ymd) %>% fill_gaps(y=11)
fit <- auto.arima(ts_isn$y,ic="bic")
fit
plot(fitted(fit),col="red",type="l")
lines(ts_isn$y,type="l")
fit.forecast <- forecast(fit,h=60)
plot(fit.forecast,type="l")
fcast <- data.table::as.data.table(fit.forecast)