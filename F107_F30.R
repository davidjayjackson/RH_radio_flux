library(tidyverse)
library(data.table)
library(xts)
library(prophet)
library(plotly)
rm(list=ls())
##
##
F30 <-fread("C:/Users/howe/Desktop/F30_predictions_win/F107_F30.csv",sep = ',')
summary(F30)

colnames(F30) <- c("JD","Year","Month","Day", "F10_7","F30_R" )
F30$Ymd <- as.Date(paste(F30$Year, F30$Month, F30$Day, sep = "-"))
F301<-F30[Ymd>="1964-07-01",.(Ymd,F10_7),]
F301$Vote <- ifelse(F301$F10_7 ==0,0,0)
Fit <- as.data.frame(lowess(F301$F10_7,f=0.3))
F301 <-cbind(F301,Fit$y)
colnames(F301) <-c("Ymd","F10_7","Vote","Loess")
str(F301)
summary(F301)
##
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
df <- S3 %>% select(Ymd,F10_7)
colnames(df) <- c("ds","y")
m <- prophet(seasonality.mode="multiplicative")
m <- add_seasonality(m, name="cycle_11year", period=364.25 * 11,fourier.order=5)
m <- fit.prophet(m, df)
future <- make_future_dataframe(m,periods=8000,freq="day")
forecast <- predict(m, future)
plot(m, forecast) +ggtitle("F10_7 Monthly: Jan. 1965 - Dec. 2019") +ylab("Predicted Days w/ F10_7") +
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
##
## Create CSV file
forecast$Year <- year(forecast$ds)
forecast$Month <- month(forecast$ds)
S5 <- select(forecast,Year,Month,ds,yhat_upper,yhat,yhat_lower)
write.csv(S5,file="dayPerMonth.csv",row.names = F)