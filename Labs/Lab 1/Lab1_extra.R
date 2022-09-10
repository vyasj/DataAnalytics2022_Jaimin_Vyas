gpw3 <- data.frame(read.csv("GPW3_GRUMP_SummaryInformation_2010.csv"))
head(gpw3)

plot(x=gpw3$PopulationPerUnit,y=gpw3$Area)
barplot(table(gpw3$ContinentName))

hist(gpw3$RefYearFirst)
hist(gpw3$RefYearLast)
boxplot(gpw3$RefYearFirst, gpw3$RefYearLast)

