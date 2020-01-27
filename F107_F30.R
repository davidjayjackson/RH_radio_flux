library(tidyverse)
library(data.table)
library(xts)
library(prophet)
library(plotly)
library(lubridate)
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
##
F301$Year <- year(F301$Ymd)
F301$Month <- month(F301$Ymd)
isn_d <- F301 %>% filter(Year >=2009 & Year <=2019) %>% select(Year,Month,F10_7,Vote)
ggplot(data=isn_d,aes(x=Vote , fill=Vote ))+
  geom_histogram(stat="count") + labs(title="Ratio of Days Above and Below Mean: 2009-2019") + facet_wrap(~Year)

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

RF <- F301
FDate <- seq(as.Date("1944-05-28"),as.Date("1953-12-31"),by="days")
FDATE <- as.data.table(FDate)
FDATE$F10_7 <- 0
A <- kanzel[Ymd <="1953-12-31",.(Ymd,R)]
A$F10_7 <- A$R *mean(kanzel$R) /mean(F301$F10_7)
A <- A[,.(Ymd,F10_7)]
B <-F301[,.(Ymd,F10_7)]
C <- rbind(A,B)

ggplot(data=F30,aes(x=Ymd,y=F30_R,col="F30_R")) +geom_line() +
  geom_line(data=F30,aes(x=Ymd,y=F10_7,col="F10_7")) +
  ggtitle("Comparing Radio Flus: Jan 1951 - Jan 2020")
#
stats <- F30 %>% select(Year,F10_7) %>% group_by(Year) %>%
  summarise(
              Mean = mean(F30$F10_7),
              Medain = median(F30$F10_7),
              Sd = sd(F30$F10_7))
summary(stats)
# Round two
stats <- F30 %>% group_by(Year) %>%
  summarise(
    Mean = mean(F10_7),
    Median = median(F10_7),
    Sd = sd(F10_7),
    N = n()
  ) 
  ggplot(stats) + geom_line(aes(x=Year,y=Median,col="Median")) +
    geom_line(aes(x=Year,y=Mean,col="Mean"))
  ggplot(stats) + geom_line(aes(x=Year,y=Sd))
  
  ## Radio Flux data
  radio <- fread("./radio_flux.txt")
  radio$Ymd <- as.Date(paste(radio$year, radio$month, radio$day, sep = "-"))
  summary(radio)
    ggplot(data=radio,aes(x=Ymd,y=f10.7)) + geom_line() + geom_smooth()
  radio$Ma150 <- forecast::ma(radio$f10.7,order=150)
  radio$Ma365 <- forecast::ma(radio$f10.7,order=365)
  radio %>% filter(Ymd >="2014-01-01") %>%
  ggplot() + geom_line(aes(x=Ymd,y=Ma150,col="150")) +
    geom_line(aes(x=Ymd,y=Ma365,col="365")) +
    ggtitle("Radio Flux F10.7CM: 2014 - 2019")