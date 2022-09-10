gpw3 <- data.frame(read.csv("GPW3_GRUMP_SummaryInformation_2010.csv"))
head(gpw3)
epi <- data.frame(read.csv("2010EPI_data.csv", skip=1))
head(epi)

##### Exercise 1:
boxplot(epi$CLIMATE[!is.na(epi$CLIMATE)], epi$AGRICULTURE[!is.na(epi$AGRICULTURE)])
hist(epi$AGRICULTURE[!is.na(epi$AGRICULTURE)])
hist(epi$CLIMATE[!is.na(epi$CLIMATE)])

plot(ecdf(epi$AGRICULTURE), do.points=F, verticals=T)
qqnorm(epi$AGRICULTURE); qqline(epi$AGRICULTURE)

plot(ecdf(epi$CLIMATE), do.points=F, verticals=T)
qqnorm(epi$CLIMATE); qqline(epi$CLIMATE)

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5), x, xlab="Q-Q Plot for t-distribution"); qqline(x)

##### Exercise 2:
EPILand <- epi$EPI[!epi$Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), probability=T)

hist(epi$No_surface_water)
hist(epi$Desert)
hist(epi$High_Population_Density)

table(epi$EPI_regions[epi$EPI_regions != ""])
EPI.SouthAsia <- epi$EPI[epi$EPI_regions == "South Asia"]
EPI.SouthAsia
EPI.MidEast.NorAfr <- epi$EPI[epi$EPI_regions == "Middle East and North Africa"]
EPI.MidEast.NorAfr
EPI.SubSahAfr <- epi$EPI[epi$EPI_regions == "Sub-Saharan Africa"]
EPI.SubSahAfr