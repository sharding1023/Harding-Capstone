library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(e1071)
library(randomForest)

################## Trees and Random Forests

#Create dataframe with variables useful for creating a tree

df <- select(medicare.full, 1:2, 7:8, 10:13, 16:23, 27:28, 30)
df<- na.omit(df)

#Rename variables to make tree more legible
names(df) <- c("DRGCode", "DRGName", "Dischar", "ACC", "AMP", "State", "Region", "Division",
                    "Owner", "Rating", "Mort", "Safety", "Readmiss", "Exper", "Effec", "Timeli",
                    "Poverty", "Income", "Density")


# Create train and test set
set.seed(1234)
split = sample.split(df$AMP, SplitRatio = 0.8)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

#Create decision tree for all DRGs

tr.control = trainControl(method = "cv", number = 10)

cp.grid = expand.grid(.cp = seq(0.00001,0.0005,0.00001)) 

train(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)

tree1 = rpart(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train, cp = 0.001)
   # The optimal cp value is less than 0.0001, but it took far too long to run with such a low cp, so I chose 0.001 instead
prp(tree1, main = "All DRGs")

# Find RMSE 
tree1.pred = predict(tree1, newdata=test)
RMSE(tree1.pred, log10(test$AMP)) # RMSE = 0.1607177

# Random Forest
forest1 = randomForest(log10(AMP) ~ ACC + Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty 
                       + Density, data = train, ntree = 20)
#THIS TOOK TOO LONG TO RUN, so I didn't make it.


#Try making trees, but for only one DRG at a time.
#Start with SIMPLE PNEUMONIA & PLEURISY W CC
df.pneu <- subset(df, DRGCode == 194)

set.seed(1234)
split = sample.split(df.pneu$AMP, SplitRatio = 0.8)
train.pneu = subset(df.pneu, split==TRUE)
test.pneu = subset(df.pneu, split==FALSE)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
train(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.pneu, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0021

tree1.pneu = rpart(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.pneu, cp = 0.0021)
prp(tree1.pneu, main = "SIMPLE PNEUMONIA & PLEURISY W CC") #ACC doesn't dominiate the tree anymore, which makes the tree far more useful and interesting. 

# Find RMSE 
tree1.pneu.pred = predict(tree1.pneu, newdata=test.pneu)
RMSE(tree1.pneu.pred, log10(test.pneu$AMP)) # RMSE = 0.08253686

# Random Forest
forest.pneu = randomForest(log10(AMP) ~ ACC + Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.pneu, ntree = 200)

# Find Random Forest RMSE 
forest.pneu.pred = predict(forest.pneu, newdata=test.pneu)
RMSE(forest.pneu.pred, log10(test.pneu$AMP)) # RMSE = 0.07112384

#Important variables
varImpPlot(forest.pneu)


# Try with SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC
df.sept <- subset(df, DRGCode == 871)

set.seed(1234)
split = sample.split(df.sept$AMP, SplitRatio = 0.8)
train.sept = subset(df.sept, split==TRUE)
test.sept = subset(df.sept, split==FALSE)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
train(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.sept, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.001

tree1.sept = rpart(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.sept, cp = 0.001)
prp(tree1.sept, main = "SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC")

# Find RMSE 
tree1.sept.pred = predict(tree1.sept, newdata=test.sept)
RMSE(tree1.sept.pred, log10(test.sept$AMP)) # RMSE = 0.0.07033426

# Random Forest
forest.sept = randomForest(log10(AMP) ~ ACC + Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.sept, ntree = 200)

# Find Random Forest RMSE 
forest.sept.pred = predict(forest.sept, newdata=test.sept)
RMSE(forest.sept.pred, log10(test.sept$AMP)) # RMSE = 0.05859889

#Important variables
varImpPlot(forest.sept)





# Try with HEART FAILURE & SHOCK W CC
df.heart <- subset(df, DRGCode == 292)

set.seed(1234)
split = sample.split(df.heart$AMP, SplitRatio = 0.8)
train.heart = subset(df.heart, split==TRUE)
test.heart = subset(df.heart, split==FALSE)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
train(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.heart, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0013

tree1.heart = rpart(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.heart, cp = 0.0013)
prp(tree1.heart, main = "HEART FAILURE & SHOCK W CC")

# Find RMSE 
tree1.heart.pred = predict(tree1.heart, newdata=test.heart)
RMSE(tree1.heart.pred, log10(test.heart$AMP)) # RMSE = 0.07874091

# Random Forest
forest.heart = randomForest(log10(AMP) ~ ACC + Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.heart, ntree = 200)

# Find Random Forest RMSE 
forest.heart.pred = predict(forest.heart, newdata=test.heart)
RMSE(forest.heart.pred, log10(test.heart$AMP)) # RMSE = 0.06739146

#Important variables
varImpPlot(forest.heart)



# Try with ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC 
df.esoph <- subset(df, DRGCode == 392)

set.seed(1234)
split = sample.split(df.esoph$AMP, SplitRatio = 0.8)
train.esoph = subset(df.esoph, split==TRUE)
test.esoph = subset(df.esoph, split==FALSE)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
train(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.esoph, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0026

tree1.esoph = rpart(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.esoph, cp = 0.0026)
prp(tree1.esoph, main = "ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC ")

# Find RMSE 
tree1.esoph.pred = predict(tree1.esoph, newdata=test.esoph)
RMSE(tree1.esoph.pred, log10(test.esoph$AMP)) # RMSE = 0.08798886

# Random Forest
forest.esoph = randomForest(log10(AMP) ~ ACC + Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.esoph, ntree = 200)

# Find Random Forest RMSE 
forest.esoph.pred = predict(forest.esoph, newdata=test.esoph)
RMSE(forest.esoph.pred, log10(test.esoph$AMP)) # RMSE = 0.07802812

#Important variables
varImpPlot(forest.esoph)





# Try with KIDNEY & URINARY TRACT INFECTIONS W/O MCC 
df.kidney <- subset(df, DRGCode == 690)

set.seed(1234)
split = sample.split(df.kidney$AMP, SplitRatio = 0.8)
train.kidney = subset(df.kidney, split==TRUE)
test.kidney = subset(df.kidney, split==FALSE)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
train(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.kidney, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0019

tree1.kidney = rpart(log10(AMP) ~ log10(ACC) + log10(Dischar) + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + log10(Income) + log10(Density), data = train.kidney, cp = 0.0019)
prp(tree1.kidney, main = "KIDNEY & URINARY TRACT INFECTIONS W/O MCC")

# Find RMSE 
tree1.kidney.pred = predict(tree1.kidney, newdata=test.kidney)
RMSE(tree1.kidney.pred, log10(test.kidney$AMP)) # RMSE = 0.09011607

# Random Forest
forest.kidney = randomForest(log10(AMP) ~ ACC + Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.kidney, ntree = 200)

# Find Random Forest RMSE 
forest.kidney.pred = predict(forest.kidney, newdata=test.kidney)
RMSE(forest.kidney.pred, log10(test.kidney$AMP)) # RMSE = 0.07606605

#Important variables
varImpPlot(forest.kidney)











#############Linear Regression
# We would like to predict Average.Medicare.Payments (or the log of that variable)


# Best subset selection
## install.packages("leaps")
library(leaps)

regfitfull = regsubsets(log10(AMP) ~ ACC + log10(ACC) + Dischar + log10(Dischar)  + Poverty + log10(Income) + 
                          Income + log10(Density) + Density + Rating, data = df, nvmax = 10, method = "forward")
summary(regfitfull)

reg.summary <- summary(regfitfull)

#Use BIC 
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "All DRGs")


#Seems like 4 variables is best. Those are log(ACC), ACC, Density, and Income
#It doesn't make sense to use both log(ACC) and ACC, so I'll take out ACC and try
#forward selection again. 

regfitfull2 = regsubsets(log10(AMP) ~ log10(ACC) + Dischar + log10(Dischar)  + Poverty + log10(Income) + 
                          Income + log10(Density) + Density + Rating, data = df, nvmax = 10, method = "forward")
summary(regfitfull2)

reg.summary2 <- summary(regfitfull2)

names(reg.summary2)

plot(reg.summary2$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "All DRGs")

# How many variables should I choose?
coef(regfitfull2, 4)

#Create model and find RMSE
linreg <- lm(log10(AMP) ~ log10(ACC) + log10(Dischar) + Income + Density, data = train)

linreg.pred = predict(linreg, newdata=test)
RMSE(linreg.pred, log10(test$AMP)) # RMSE = 0.1607177






# Does variable selection change if I focus on one DRG? Try with just pneumonia
regfitfull.pneu = regsubsets(log10(AMP) ~ ACC + log10(ACC) + Dischar + log10(Dischar)  + Poverty + log10(Income) + 
                           Income + log10(Density) + Density + Rating, data = df.pneu, nvmax = 10, method = "forward")
summary(regfitfull.pneu)

reg.summary.pneu <- summary(regfitfull.pneu)


plot(reg.summary.pneu$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "SIMPLE PNEUMONIA & PLEURISY W CC")

# How many variables should I choose?
coef(regfitfull2, 7)

#Create model and find RMSE
linreg.pneu <- lm(log10(AMP) ~ log10(ACC) + Dischar + log10(Dischar)  + log10(Income) + 
               Income + log10(Density) + Density + Rating, data = train.pneu)

linreg.pneu.pred = predict(linreg, newdata=test.pneu)
RMSE(linreg.pneu.pred, log10(test.pneu$AMP)) # RMSE = 0.1607177
