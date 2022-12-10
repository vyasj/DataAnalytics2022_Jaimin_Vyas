library(dplyr)
library(ggplot2)
library(zoo)
library(rpart)
library(rpart.plot)
library(caret)

ndvi <- read.csv("NDVI_data.csv")
crops <- read.csv("psd_grains_pulses.csv")
tempchange <- read.csv("tempchange.csv")
precipitation <- read.csv("precipitation.csv")
meantemp <- read.csv("meantemp.csv")

wheat.usa <- crops %>%
  filter(Commodity_Description == "Wheat") %>%
  filter(Country_Name == "United States")

ggplot(data=wheat.usa, aes(x=Attribute_Description)) + 
  geom_bar(stat="count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

crop.prod <- wheat.usa %>%
  filter(Attribute_Description == "Production")

ggplot(data=ndvi, aes(x=ORDINAL.DATE, y=SAMPLE.VALUE)) +
  geom_point() +
  ggtitle("NDVI Over Time") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

crop.prod <- crop.prod %>%
  mutate(Cal.Month.Year = zoo::as.yearmon(paste(as.character(crop.prod$Calendar_Year), as.character(crop.prod$Month)), "%Y %m")) %>%
  mutate(Mar.Month.Year = zoo::as.yearmon(paste(as.character(crop.prod$Market_Year), as.character(crop.prod$Month)), "%Y %m"))

ggplot(data=crop.prod, aes(x=Mar.Month.Year, y=Value)) + 
  geom_point() + 
  ggtitle("Wheat Production Over Time")

qqnorm(crop.prod$Value)
qqline(crop.prod$Value)
shapiro.test(crop.prod$Value)

tempchange$Months.Code <- tempchange$Months.Code-7000
tempchange <- tempchange %>% 
  mutate(Month.Year =  zoo::as.yearmon(paste(as.character(tempchange$Year.Code), as.character(tempchange$Months.Code)), "%Y %m"))
ggplot(data=tempchange, aes(x=Month.Year, y=Value)) +
  geom_point() +
  ggtitle("Chenge in Temperature Over Time") +
  ylab("Temperature Change (C)") +
  xlab("Date")

ndvi$START.DATE <- as.Date(ndvi$START.DATE, format="%m/%d/%Y")
ndvi$END.DATE <- as.Date(ndvi$END.DATE, format="%m/%d/%Y")
ndvi <- ndvi %>%
  mutate(Month.Year = zoo::as.yearmon(ndvi$END.DATE, "%Y %m"))

ggplot(data=crop.prod, aes(x=Cal.Month.Year, y=Value)) +
  geom_point()

colnames(precipitation) <- c("Month","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")

colnames(meantemp) <- c("Year", "Annual.Mean.Temp", "5.yr.smooth")
ggplot(data=meantemp, aes(x=Year, y=Annual.Mean)) +
  geom_point() +
  ggtitle("Annual Mean Temperature Over Time")

ndvi <- ndvi %>%
  mutate(Month = as.numeric(format(START.DATE, "%m"))) %>%
  mutate(Calendar_Year = as.numeric(format(START.DATE, "%Y")))

names(crop.prod)[names(crop.prod) == "Value"] <- "Yield"
names(meantemp)[names(meantemp) == "Year"] <- "Calendar_Year"

compiled.df <- merge(crop.prod, ndvi, by=c("Calendar_Year", "Month"))
compiled.df <- merge(compiled.df, meantemp, by="Calendar_Year")

colnames(precipitation) <- c("Month","Calendar_Year","Precipitation")
precipitation$Month <- match(precipitation$Month, month.name)
compiled.df <- merge(compiled.df, precipitation, by=c("Calendar_Year", "Month"))

compiled.df <- compiled.df[c("Yield","Month","Calendar_Year","MEAN.VALUE","Annual.Mean.Temp","Precipitation")]

#compiled.df$Yield <- scale(compiled.df$Yield, center=T, scale=T)

set.seed(4600)
index <- sample(c(T,F), nrow(compiled.df), replace=T, prob=c(0.7,0.3))
train <- compiled.df[index,]
test <- compiled.df[-index,]

model1 <- lm(data=train, Yield~Month+Calendar_Year+MEAN.VALUE+Annual.Mean.Temp+Precipitation)
summary(model1)

model2 <- lm(data=train, Yield~sin(MEAN.VALUE))
summary(model2)
preds2 <- predict(model2, newdata=test)
sum((test$Yield - preds)^2)

model3 <- rpart(formula=Yield~sin(MEAN.VALUE), data=train, method="anova")
rpart.plot::rpart.plot(model3)
preds3 <- predict(model3, newdata=test)

model4 <- knnreg(x=train, y=train$Yield)
