#Multivariable Linear Regression 

#Start by just using pneumonia data


model1 <- lm(Prop.Covered.by.Medicare ~ Median.Income.County + Density.County, data = medicare.pneumonia)
summary(model1)

model2 <- lm(sqrt.Prop.Covered.by.Medicare ~ log.Median.Income.County + log.Density.County, data = medicare.pneumonia)
summary(model2)

model3 <- lm(log.Average.Medicare.Payments ~ log.Median.Income.County + log.Density.County, data = medicare.pneumonia)
summary(model3)

#Add more variabes? 