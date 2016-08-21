#Initial Exploratory Analysis


library(dplyr)
library(tidyr)
library(ggplot2)

summary(medicare.full)
summary(medicare.pneumonia)

#Explore numerical variables for full data set and for pneumonia
medicare.num <- select(medicare.full, 7:10, 17, 27:31)
medicare.pneumonia.num <- select(medicare.pneumonia, 7:10, 17, 27:31)


cor(x = medicare.num, use = "pairwise.complete.obs")
cor(x = medicare.pneumonia.num, use = "pairwise.complete.obs")

#Histograms

lapply(1:ncol(medicare.num),function(i) hist(medicare.num[,i], main =paste("Histogram of",names(medicare.num)[i]))) 
lapply(1:ncol(medicare.pneumonia.num),function(i) hist(medicare.pneumonia.num[,i], main =paste("Histogram of",names(medicare.pneumonia.num)[i]))) 

# Transform any skewed variables

medicare.full$log.Total.Discharges <- log(medicare.full$Total.Discharges)   #still slightly skewed
medicare.full$log.Average.Covered.Charges <- log(medicare.full$Average.Covered.Charges) #now symmetric
medicare.full$log.Average.Total.Payments <- log(medicare.full$Average.Total.Payments) #still slightly skewed
medicare.full$log.Average.Medicare.Payments <- log(medicare.full$Average.Medicare.Payments) #now symmetric
medicare.full$log.Median.Income.County <- log(medicare.full$Median.Income.County) #now symmetric
medicare.full$log.Density.County<- log(medicare.full$Density.County) #now symmetric
medicare.full$sqrt.Prop.Covered.by.Medicare <- sqrt(medicare.full$Prop.Covered.by.Medicare) #log made it worse, so I tried square root

#Check histograms again
medicare.pneumonia <- subset(medicare.full, DRG.Code == 194)
medicare.num <- select(medicare.full, 7:10, 17, 27:38)
medicare.pneumonia.num <- select(medicare.pneumonia, 7:10, 17, 27:38)

lapply(1:ncol(medicare.num),function(i) hist(medicare.num[,i], main =paste("Histogram of",names(medicare.num)[i]))) 
lapply(1:ncol(medicare.pneumonia.num),function(i) hist(medicare.pneumonia.num[,i], main =paste("Histogram of",names(medicare.pneumonia.num)[i]))) 

#Look at corrlation matrix for medicare.pneumonia now that variables have more symmetric distribution
cor(medicare.pneumonia.num, use = "pairwise.complete.obs")



#Look at some scatterplots using pneumonia data

pairs(~Average.Medicare.Payments + log.Average.Medicare.Payments + Prop.Covered.by.Medicare 
      + sqrt.Prop.Covered.by.Medicare + log.Total.Discharges + Poverty.Percent.County + Unemployment.County 
      + log.Median.Income.County + log.Density.County, data=medicare.pneumonia, 
      main="Scatterplot Matrix")


qplot(x = Median.Income.County, y = Average.Medicare.Payments, data = medicare.pneumonia)

qplot(x = log.Median.Income.County, y = log.Average.Medicare.Payments, data = medicare.pneumonia)

qplot(x = Density.County, y = Average.Medicare.Payments, data = medicare.pneumonia)

qplot(x = log.Density.County, y = log.Average.Medicare.Payments, data = medicare.pneumonia)

qplot(x = log.Density.County, y = log.Average.Medicare.Payments, data = medicare.pneumonia)







