library(data.table)
library(tidyverse)
library(ggplot2)
library(gplots)
library(forecast)

data2016 <- fread("2016.csv")
good_data2016 <- data2016 %>% 
  filter((data2016$WEATHER_DELAY != "NA") & (data2016$CANCELLED == 0) 
         & (data2016$DEP_DELAY >= 0))
good_data2016 <- good_data2016[DEP_DELAY >= 0,-c("CRS_DEP_TIME","CRS_ARR_TIME",
                                                 "WHEELS_OFF","WHEELS_ON","DEP_TIME",
                                                 "ARR_TIME","CRS_ELAPSED_TIME",
                                                 "CANCELLED","DIVERTED","SECURITY_DELAY",
                                                 "Unnamed: 27","CANCELLATION_CODE")]


data2017 <- fread("2017.csv")
good_data2017 <- data2017 %>% 
  filter((data2017$WEATHER_DELAY != "NA") & (data2017$CANCELLED == 0)
         & (data2017$DEP_DELAY >= 0) )
good_data2017<- good_data2017[DEP_DELAY >= 0,-c("CRS_DEP_TIME","CRS_ARR_TIME",
                                                "WHEELS_OFF","WHEELS_ON","DEP_TIME",
                                                "ARR_TIME","CRS_ELAPSED_TIME",
                                                "CANCELLED","DIVERTED","SECURITY_DELAY",
                                                "Unnamed: 27","CANCELLATION_CODE")]

data2018 <- fread("2018.csv") 
good_data2018 <- data2018 %>% 
  filter((data2018$WEATHER_DELAY != "NA") & (data2018$CANCELLED == 0)
         & (data2018$DEP_DELAY >= 0))
good_data2018 <- good_data2018[DEP_DELAY >= 0,-c("CRS_DEP_TIME","CRS_ARR_TIME",
                                                 "WHEELS_OFF","WHEELS_ON","DEP_TIME",
                                                 "ARR_TIME","CRS_ELAPSED_TIME",
                                                 "CANCELLED","DIVERTED","SECURITY_DELAY",
                                                 "Unnamed: 27","CANCELLATION_CODE")]

whole_data <-rbind(good_data2016,good_data2017,good_data2018)

IS_WINTER <- rbind(good_data2016[,.(FL_DATE < "2016-03-15" | FL_DATE > "2016-10-15")],
                   good_data2017[,.(FL_DATE < "2017-03-15" | FL_DATE > "2017-10-15")],
                   good_data2018[,.(FL_DATE < "2018-03-15" | FL_DATE > "2018-10-15")])
names(IS_WINTER)[1]<-"IS_WINTER"

whole_data<-cbind(whole_data,IS_WINTER)

plot(whole_data$DEP_DELAY,whole_data$ARR_DELAY)
boxplot(whole_data$ARR_DELAY,xlab = "ARRIVAL DELAY", horizontal = TRUE)
boxplot(whole_data$DEP_DELAY,xlab = "DEPARTURE DELAY", horizontal = TRUE)

summary(whole_data)
whole_data <-whole_data %>%
  filter((whole_data$DEP_DELAY < (75+1.5*60)) & 
           (whole_data$ARR_DELAY < (39+1.5*50)))

whole_data_num <- whole_data[,-c(1:5)]
summary(whole_data_num)

cor_matt <- cor(whole_data_num)
heatmap.2(cor_matt, Rowv=FALSE, Colv = FALSE, dendogram = "none",
          cellnote = round(cor_matt,3),
          notecol = "blue",key = FALSE,trace = 'none',margins = c(12,12))


set.seed(42)
train.index <- sample(whole_data[,.N],round((whole_data[,.N]*0.8),digits=0))

lmFull<- lm(ARR_DELAY ~ DEP_DELAY + TAXI_OUT + TAXI_IN + AIR_TIME + DISTANCE
            + WEATHER_DELAY + CARRIER_DELAY +
            NAS_DELAY + LATE_AIRCRAFT_DELAY, 
            data = whole_data_num[train.index])
summary(lmFull)
                 

lm.pred <- predict(lmFull,newdata = whole_data_num[-train.index,])
#lm.pred

accuracy(lm.pred, whole_data_num[-train.index,ARR_DELAY])

lm.wint<- lm(ARR_DELAY ~ DEP_DELAY + TAXI_OUT + TAXI_IN + AIR_TIME + DISTANCE
            + WEATHER_DELAY + CARRIER_DELAY + NAS_DELAY + LATE_AIRCRAFT_DELAY
            + IS_WINTER, data = whole_data[train.index,])
summary(lm.wint)


wint.pred<-predict(lm.wint)
accuracy(wint.pred, whole_data[-train.index,ARR_DELAY])


pcs <- prcomp(na.omit(whole_data_num, scale. = T))

summary(pcs)
pcs$rot


