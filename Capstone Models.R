library(rpart)
library(rpart.plot)
library(caTools)

#Split the data into train and test
set.seed(1234)
split = sample.split(medicare.full$Average.Medicare.Payments, SplitRatio = 0.8)
train = subset(medicare.full, split==TRUE)
test = subset(medicare.full, split==FALSE)


# Create a linear model to predict average medicare payments using just average covered charges

qplot(x = Average.Covered.Charges, y = Average.Medicare.Payments, data = train)

linreg1 = lm(Average.Medicare.Payments ~ Average.Covered.Charges, data = train)
summary(linreg1)
# Adjusted R-squared = 0.7132

# Make predictions then find SUM OF SQUARED ERRORS (SSE)
linreg1.pred = predict(linreg1, newdata=test)
linreg1.sse = sum((linreg1.pred - test$Average.Medicare.Payments)^2)
linreg1.sse

# SSE = 2.006477e+12



#Use transformed data instead
qplot(x = log.Average.Covered.Charges, y = log.Average.Medicare.Payments, data = train)

linreg2 = lm(log.Average.Medicare.Payments ~ log.Average.Covered.Charges, data = train)
summary(linreg2)

# Adjusted R-squared = 0.6012

# Find SUM OF SQUARED ERRORS (SSE)
linreg2.pred = predict(linreg2, newdata=test)
linreg2.sse = sum((linreg2.pred - test$log.Average.Medicare.Payments)^2)
linreg2.sse

# SSE = 8597.788  (Better than first, but can I even compare these since I'm not using the same dependent variable?)


#Create a CART model
tree1 = rpart(Average.Medicare.Payments ~ Average.Covered.Charges, data = train)
prp(tree1)

#Find SSE
tree1.pred = predict(tree1, newdata=test)
tree1.sse = sum((tree1.pred - test$Average.Medicare.Payments)^2)
tree1.sse
#  SSE = 2.365277e+12n (A bit worse than original linear model)

#Improve CART model by changing the cp
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 


# Cross-validation
train(Average.Medicare.Payments ~ Average.Covered.Charges, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

#Tree with best cp
tree2 = rpart(Average.Medicare.Payments ~ Average.Covered.Charges, data = train, cp = 0.0005)
prp(tree2)

#Find SSE
tree2.pred = predict(tree2, newdata=test)
tree2.sse = sum((tree2.pred - test$Average.Medicare.Payments)^2)
tree2.sse

# SSE = 2.018496e+12 (Better than other tree, but still worse than linear model)




# Linear model with more variables
linreg3 = lm(Average.Medicare.Payments ~ Average.Covered.Charges + Total.Discharges + latitude + longitude + 
               Poverty.Percent.County + Median.Income.County + Unemployment.County + Denisity.County, data = train)
summary(linreg3)
# Adjusted R-squared = 0.05389 

# Find SSE
linreg3.pred = predict(linreg3, newdata=test)
linreg3.sse = sum((linreg3.pred - test$Average.Medicare.Payments)^2)
linreg3.sse

# SSE = 6.558639e+12 (much, much worse than just using Average.Covered.Charges)


# Simplify model by taking out some variables
linreg4 = lm(Average.Medicare.Payments ~ Average.Covered.Charges + latitude + longitude + Poverty.Percent.County + Median.Income.County + Density.County, data = train)
summary(linreg4)
# Adjusted R-squared = 0.747 

# Find SSE
linreg4.pred = predict(linreg4, newdata=test)
linreg4.sse = sum((linreg4.pred - test$Average.Medicare.Payments)^2, na.rm = TRUE)
linreg4.sse

# SSE = 1.631414e+12 (Best so far!)




#Use those same variables to make a CART model
tree3 = rpart(Average.Medicare.Payments ~ Average.Covered.Charges + latitude + longitude + Poverty.Percent.County + Median.Income.County + Density.County, data = train)
prp(tree3)

#Find SSE
tree3.pred = predict(tree3, newdata=test)
tree3.sse = sum((tree3.pred - test$Average.Medicare.Payments)^2)
tree3.sse

# SSE = 2.365277e+12 (Worse than the original linear model)

#Change cp to improve tree
# I'm gettin an error about missing values