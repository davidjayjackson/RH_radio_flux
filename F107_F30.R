library(tidyverse)
library(data.table)
library(xts)
library(prophet)
library(plotly)
rm(list=ls())
##
##
F30 <-fread("./F107_F30.csv")
summary(F30)

colnames(F30) <- c("JD","Year","Month","Day", "F10_7","F30_R" )
F30$Ymd <- as.Date(paste(F30$Year, F30$Month, F30$Day, sep = "-"))
# F301<-F30[Ymd>="1964-07-01",.(Ymd,F10_7),]
F301<-F30[Ymd>="1951-01-01",.(Ymd,F10_7),]
F301$Vote <- ifelse(F301$F10_7 >=121.5,"Yes","No")
Fit <- as.data.frame(lowess(F301$F10_7,f=0.1))
F301 <-cbind(F301,Fit$y)
colnames(F301) <-c("Ymd","F10_7","Vote","Loess")
summary(F301)
## Added by David Jackson 2020-01-08
F301 %>% ggplot(aes(x=Ymd,y=F10_7)) +geom_line() + geom_line(aes(x=Ymd,y=Loess)) +
  geom_smooth(method="glm")
## Create monthly summary (Vote) field with XTS
##
isn.xts <- xts(x = F301$F10_7, order.by = F301$Ymd)
isn.monthly <- apply.monthly(isn.xts, sum)
isn <-as.data.table(isn.monthly)
colnames(isn) <- c("Ymd","F10_7")
S3 <- isn %>% filter(Ymd <="2019-12-01")
ggplot(data=S3,aes(x=Ymd,y=F10_7)) +geom_line() +geom_smooth(method="loess",col="blue") + 
  ggtitle("Monthly Days with F10_7: 1965 - 2019")
##
## Prophet prediction based on Daily Vote Field
##
df <- F301 %>% select(Ymd,F10_7)
colnames(df) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=364.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("F10_7 Monthly: Jan. 1965 - Dec. 2019") +ylab("Predicted Days w/ F10_7") +
  xlab("Years" )
## Added David Jackson 2020-01-08
plot(m, forecast) +ggtitle("F10_7 Daily: Jan. 1951 - Dec. 2019") +ylab("Predicted Days w/ F10_7") +
  xlab("Years" ) 
##
S4 <- filter(forecast,ds >="2020-01-01" & ds <="2026-12-31")
ggplot(data=S4,aes(x=ds,y=yhat_upper,col="Upper")) +geom_col() +
  geom_col(data=S4,aes(x=ds,y=yhat,col="Predict")) +
  geom_col(data=S4,aes(x=ds,y=yhat_lower,col="Lower")) +
  xlab("Date: Month and Year") + ylab("Days Per Month") + 
  geom_smooth(data=S4,aes(x=ds,y=yhat,col="Loess"))
##
# Create a interactive plot.
plot_ly(data=S4,x=~ds,y=~yhat,mode="lines")
fcast <- as.data.table(forecast)
ggplot(data=S4,aes(x=ds,y=yhat)) + geom_line() + geom_smooth(method="glm") +
  ggtitle("Radio Flux(F10_7-2) Predictionf2020 = 2026") +
    xlab("Date/Year") + ylab("Predicted Radio Flux")
##
## Create CSV file
forecast$Year <- year(forecast$ds)
forecast$Month <- month(forecast$ds)


S5 <- select(forecast,Year,Month,ds,yhat_upper,yhat,yhat_lower)
write.csv(S5,file="dayPerMonth.csv",row.names = F)

## Added kanzel data: from 1951 2019
kanzel <- fread("kh_spots.csv")
kanzel$Ymd <- as.Date(kanzel$Ymd)
str(kanzel)
df1 <- kanzel[Ymd>="1951-01-01",.(Ymd,R)]
colnames(df1) <-c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=364.25 * 11,fourier.order=5)
m <- fit.prophet(m, df1)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("Kanzel ISN : Jan. 1951 - Dec. 2019") +ylab("Predicted ISN") +
  xlab("Years" ) 
fcast <- as.data.table(forecast)
fcast <- fcast[ds>="2020-01-01" & ds <="2026-12-31",.(ds,yhat)]
ggplot(data=fcast,aes(x=ds,y=yhat)) + geom_line() + geom_smooth(method="glm") +
  ggtitle("Kanzel ISN Predication: 2020 2026") +
  xlab("Date/Year") +ylab("Prediction")
##
##Combined plot kanzel + Radio_flux
##
ggplot(data=fcast,aes(x=ds,y=yhat,col="ISN")) + geom_line() + geom_smooth(method="glm") +
  ggtitle("Comparing ISN/Radio Flux: 2020 2026") + geom_line(data=S4,aes(x=ds,y=yhat,col="F10_7")) +
  xlab("Date/Year") +ylab("Predicted Values")

mean(kanzel$R) /mean(F301$F10_7)
RF <- F301
FDate <- seq(as.Date("1944-05-28"),as.Date("1953-12-31"),by="days")
FDATE <- as.data.table(FDate)
FDATE$F10_7 <- 0
A <- kanzel[Ymd <="1953-12-31",.(Ymd,R)]
A$F10_7 <- A$R *0.757
A <- A[,.(Ymd,F10_7)]
B <-F301[,.(Ymd,F10_7)]
C <- rbind(A,B)
