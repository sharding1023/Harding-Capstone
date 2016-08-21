#Multivariable Linear Regression 

model1 <- lm(Prop.Covered.by.Medicare ~ Median.Income.County + Density.County, data = medicare.pneumonia)
summary(model1)

model2 <- lm(sqrt.Prop.Covered.by.Medicare ~ log.Median.Income.County + log.Density.County, data = medicare.pneumonia)
summary(model2)