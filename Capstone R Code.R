## Data Wrangling 

#load libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(e1071)
library(randomForest)
library(leaps)


#Medicare Charge Data from 2014
medicare.orig <- read.csv('Data/Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv')
medicare.orig$Provider.Id <- factor(medicare.orig$Provider.Id)
medicare.orig$Provider.Zip.Code <- factor(medicare.orig$Provider.Zip.Code)

#Include Census Regions in Medicare Charge Data
regions <- read.csv('Data/censusregions.csv')

medicare.large <- merge(medicare.orig,regions,by.x="Provider.State", by.y = "State.Code")

#Add additional data about hospitals
hospitals <- read.csv("Data/Hospital_General_Information.csv")
hospitals2 <- hospitals %>% select(1,2,7,10,13,15,17, 19, 21, 23, 25)
names(hospitals2)[names(hospitals2) == 'Provider.ID'] <- 'Provider.Id'

medicare.large <- left_join(medicare.large, hospitals2)


# Add fips county code, latitide, and longitude for each zip code
library(noncensus)
data(zip_codes)
zip_codes$zip <- as.numeric(zip_codes$zip)
zip_codes$zip <- as.factor(zip_codes$zip)
zip_codes$fips <- as.factor(zip_codes$fips)
zip_codes2 <- select(zip_codes, 1, 4:6)

names(zip_codes2)[names(zip_codes2) == 'fips'] <- 'County.ID'
names(zip_codes2)[names(zip_codes2) == 'zip'] <- 'Provider.Zip.Code'

medicare.large <- left_join(medicare.large, zip_codes2)

#Add percent in poverty and median income by county
poverty <- read.csv('Data/Poverty by County.csv')
poverty2 <- select(poverty, 3, 10, 41)
poverty2$County.ID <- as.factor(poverty2$County.ID)
names(poverty2)[names(poverty2) == 'All.Ages.in.Poverty.Percent'] <- 'Poverty.Percent.County'
names(poverty2)[names(poverty2) == 'Median.Household.Income.in.Dollars'] <- 'Median.Income.County'


medicare.large <- left_join(medicare.large, poverty2)


#Add county density
countydensity <- read.csv("Data/CountyDensity.csv")
countydensity2 <- select(countydensity, 5, 13)
names(countydensity2)[names(countydensity2) == 'Target.Geo.Id2'] <- 'County.ID'
names(countydensity2)[names(countydensity2) == 'Density.per.square.mile.of.land.area...Population'] <- 'Density.County'
countydensity2$County.ID <- as.factor(countydensity2$County.ID)

medicare.large <- left_join(medicare.large, countydensity2)


#Separate DRG Code and Defintion

medicare.large <- separate(medicare.large, col = DRG.Definition, into = c("DRG.Code", "DRG.Name"), sep = " - ")



#Make sure everything is the right data type
medicare.large$DRG.Code <- as.factor(medicare.large$DRG.Code)
medicare.large$DRG.Name <- as.factor(medicare.large$DRG.Name)
medicare.large$Provider.Id <- as.factor(medicare.large$Provider.Id)
medicare.large$Provider.Zip.Code <- as.factor(medicare.large$Provider.Zip.Code)
medicare.large$County.ID <- as.factor(medicare.large$County.ID)

##Create dataframe with variables useful for modeling

df <- select(medicare.large, 2:3, 10:11, 13, 15:16, 19:28, 30:32)


#Rename variables names so they are shorter
names(df) <- c("DRGCode", "DRGName", "Dischar", "ACC", "AMP", "Region", "Division",
               "Owner", "Rating", "Mort", "Safety", "Readmiss", "Exper", "Effec", "Timeli", "Latitude", "Longitude",
               "Poverty", "Income", "Density")


#Create dummy variables for "Mort", "Safety", "Readmiss", "Exper", "Effec", "Timeli" levels.
# -1 = "Below the National average"
# 0 = "Same as the National average"
# 1 = Above the National average"
# NA = Not Available


df$Mort.code <- as.character(df$Mort)
df$Safety.code <- as.character(df$Safety)
df$Readmiss.code <- as.character(df$Readmiss)
df$Exper.code <- as.character(df$Exper)
df$Effec.code <- as.character(df$Effec)
df$Timeli.code <- as.character(df$Timeli)


df[df == "Below the National average"] <- -1
df[df == "Same as the National average"] <- 0
df[df == "Above the National average"] <- 1
df[df == "Not Available"] <- NA

df$Mort.code <- as.numeric(df$Mort.code)
df$Safety.code <- as.numeric(df$Safety.code)
df$Readmiss.code <- as.numeric(df$Readmiss.code)
df$Exper.code <- as.numeric(df$Exper.code)
df$Effec.code <- as.numeric(df$Effec.code)
df$Timeli.code <- as.numeric(df$Timeli.code)

df$Mort <- as.factor(df$Mort.code)
df$Safety <- as.factor(df$Safety.code)
df$Readmiss <- as.factor(df$Readmiss.code)
df$Exper <- as.factor(df$Exper.code)
df$Effec <- as.factor(df$Effec.code)
df$Timeli <- as.factor(df$Timeli.code)

#Remove all NA's
df<- na.omit(df)

#Shorten names of levels for Mort, Safety, Readmiss, Exper, Effec, Timeli
levels(df$Mort) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Safety) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Readmiss) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Exper) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Effec) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Timeli) <- c("Below Avg", "Avg", "Above Avg")

# Create data frames for top 5 most common DRGs
df.pneu <- subset(df, DRGCode == 194)
df.sept <- subset(df, DRGCode == 871)
df.heart <- subset(df, DRGCode == 292)
df.esoph <- subset(df, DRGCode == 392)
df.kidney <- subset(df, DRGCode == 690)

df.top5 <- rbind(df.pneu, df.sept, df.heart, df.esoph, df.kidney)

# Shorten DRG names
df.top5$DRGName <- as.character(df.top5$DRGName) 
df.top5$DRGName <- as.factor(df.top5$DRGName)
levels(df.top5$DRGName) <- c("Esoph", "Heart", "Kidney", "Sept", "Pneu")

# Create list with top 5 data frames
list.top5 <- list(df.pneu, df.sept, df.heart, df.esoph, df.kidney)

# Create list that includes all and top 5
list <- list(df, df.pneu, df.sept, df.heart, df.esoph, df.kidney)

# Find minimum, maximum, mean, and standard deviation of AMP for each DRG
Min <- sapply(list, function(i) min(i$AMP))
Max <- sapply(list, function(i) max(i$AMP))
Mean <- sapply(list, function(i) mean(i$AMP))
SD <- sapply(list, function(i) sd(i$AMP))

AMP.summary <- data.frame(Min, Max, Mean, SD, row.names = c("All DRGS", "Pneu", "Sept", "Heart", "Esoph", "Kidney"))
View(AMP.summary)

# Show distribution of AMP AND log10(AMP)
qplot(x = AMP, data = df, binwidth = 5000, main = "Histogram of Average Medicare Payments for All DRGs")
qplot(x = log10(AMP), data = df, binwidth = .03, main = "Histogram of log(AMP) for All DRGs")


ggplot(df.top5, aes(AMP, fill = DRGName)) + 
  geom_density(alpha = 0.25) + 
  ggtitle("Density Plot of Average Medicare Payments for each of Top 5 DRGs")

ggplot(df.top5, aes(log10(AMP), fill = DRGName)) + 
  geom_density(alpha = 0.25) + 
  ggtitle("Density Plot of log(AMP) for each of Top 5 DRGs")

###### MODELS
# For comparison, save all RMSEs in new matrix
RMSEs <- data.frame(CART = 1:6, RandomForest = 1:6, LinearRegression = 1:6, row.names = c("All DRGS", "Pneu", "Sept", "Heart", "Esoph", "Kidney"))

# Create train and test set
set.seed(1234)
split = sample.split(df$AMP, SplitRatio = 0.8)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

set.seed(1234)
split = sample.split(df.pneu$AMP, SplitRatio = 0.8)
train.pneu = subset(df.pneu, split==TRUE)
test.pneu = subset(df.pneu, split==FALSE)

set.seed(1234)
split = sample.split(df.sept$AMP, SplitRatio = 0.8)
train.sept = subset(df.sept, split==TRUE)
test.sept = subset(df.sept, split==FALSE)

set.seed(1234)
split = sample.split(df.heart$AMP, SplitRatio = 0.8)
train.heart = subset(df.heart, split==TRUE)
test.heart = subset(df.heart, split==FALSE)

set.seed(1234)
split = sample.split(df.esoph$AMP, SplitRatio = 0.8)
train.esoph = subset(df.esoph, split==TRUE)
test.esoph = subset(df.esoph, split==FALSE)

set.seed(1234)
split = sample.split(df.kidney$AMP, SplitRatio = 0.8)
train.kidney = subset(df.kidney, split==TRUE)
test.kidney = subset(df.kidney, split==FALSE)


tree1 <- rpart(log10(AMP) ~  Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train, cp = .008)

# Find RMSE 
tree1.pred = predict(tree1, newdata=test)
RMSEs[1,1] <- RMSE(tree1.pred, log10(test$AMP)) 

#Create Readable Tree for Visual Analysis
tree.readable = rpart(log10(AMP) ~  Dischar + Region  + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                        Effec + Timeli + Poverty + Income + Density, data = train, cp = .002)

prp(tree.readable, main = "Readable Tree - All DRG's")



# Random Forest
set.seed(1234)
split = sample.split(train$AMP, SplitRatio = 0.3)
train.smaller = subset(df, split==TRUE)
forest1 = randomForest(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty 
                       + Density, data = train.smaller, ntree = 200)

forest1.pred = predict(forest1, newdata=test)
RMSEs[1,2] <- RMSE(forest1.pred, log10(test$AMP))

#Important variables
varImpPlot(forest1, main = "Variable Importance for Random Forest with All DRGs")


#Try making trees, but for only one DRG at a time.
#Start with SIMPLE PNEUMONIA & PLEURISY W CC

#tr.control = trainControl(method = "cv", number = 10)
#cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
#train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + Effec + Timeli + Poverty + Income + Density, data = train.pneu, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0028

tree1.pneu = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + Income + Density, data = train.pneu, cp = 0.0028)

# Find RMSE 
tree1.pneu.pred = predict(tree1.pneu, newdata=test.pneu)
RMSEs[2,1] <- RMSE(tree1.pneu.pred, log10(test.pneu$AMP))

#Create Readable Tree for Visual Analysis
tree.pneu.readable = rpart(log10(AMP) ~ Dischar + Region  + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                             Effec + Timeli + Poverty + Income + Density, data = train.pneu, cp = .008)

prp(tree.pneu.readable, main = "SIMPLE PNEUMONIA & PLEURISY W CC")

# Random Forest
forest.pneu = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.pneu, ntree = 200)

# Find Random Forest RMSE 
forest.pneu.pred = predict(forest.pneu, newdata=test.pneu)
RMSEs[2,2] <- RMSE(forest.pneu.pred, log10(test.pneu$AMP))

#Important variables
varImpPlot(forest.pneu, main = "Variable Importance for Pneumonia Random Forest")


# Try with SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC

#tr.control = trainControl(method = "cv", number = 10)
#cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
#train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + Effec + Timeli + Poverty + Income + Density, data = train.sept, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.003

tree1.sept = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + Income + Density, data = train.sept, cp = 0.003)

# Find RMSE 
tree1.sept.pred = predict(tree1.sept, newdata=test.sept)
RMSEs[3,1] <- RMSE(tree1.sept.pred, log10(test.sept$AMP))

#Create readable tree
tree.readable.sept = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                             Effec + Timeli + Poverty + Income + Density, data = train.sept, cp = 0.008)
prp(tree.readable.sept, main = "SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC")

# Random Forest
forest.sept = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.sept, ntree = 200)

# Find Random Forest RMSE 
forest.sept.pred = predict(forest.sept, newdata=test.sept)
RMSEs[3,2] <- RMSE(forest.sept.pred, log10(test.sept$AMP))

#Important variables
varImpPlot(forest.sept, main = "Variable Importance for Septicemia Random Forest")





# Try with HEART FAILURE & SHOCK W CC

#tr.control = trainControl(method = "cv", number = 10)
#cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
#train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + Effec + Timeli + Poverty + Income + Density, data = train.heart, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0033

tree1.heart = rpart(log10(AMP) ~Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + Income + Density, data = train.heart, cp = 0.0033)

# Find RMSE 
tree1.heart.pred = predict(tree1.heart, newdata=test.heart)
RMSEs[4,1] <- RMSE(tree1.heart.pred, log10(test.heart$AMP))

#Readable Tree
tree.readable.heart = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                              Effec + Timeli + Poverty + Income + Density, data = train.heart, cp = 0.008)
prp(tree.readable.heart, main = "HEART FAILURE & SHOCK W CC")


# Random Forest
forest.heart = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.heart, ntree = 200)

# Find Random Forest RMSE 
forest.heart.pred = predict(forest.heart, newdata=test.heart)
RMSEs[4,2] <- RMSE(forest.heart.pred, log10(test.heart$AMP))

#Important variables
varImpPlot(forest.heart, main = "Variable Importance for Heart Random Forest")



# Try with ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC 

#tr.control = trainControl(method = "cv", number = 10)
#cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
#train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + Effec + Timeli + Poverty + Income + Density, data = train.esoph, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.00267

tree1.esoph = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + Income + Density, data = train.esoph, cp = 0.0027)

# Find RMSE 
tree1.esoph.pred = predict(tree1.esoph, newdata=test.esoph)
RMSEs[5,1] <- RMSE(tree1.esoph.pred, log10(test.esoph$AMP)) 

#Readable Tree
tree.readable.esoph = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                              Effec + Timeli + Poverty + Income + Density, data = train.esoph, cp = 0.008)
prp(tree.readable.esoph, main = "ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC ")


# Random Forest
forest.esoph = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.esoph, ntree = 200)

# Find Random Forest RMSE 
forest.esoph.pred = predict(forest.esoph, newdata=test.esoph)
RMSEs[5,2] <- RMSE(forest.esoph.pred, log10(test.esoph$AMP)) 

#Important variables
varImpPlot(forest.esoph, main = "Variable Importance for Esophagitis Random Forest")





# Try with KIDNEY & URINARY TRACT INFECTIONS W/O MCC 

#tr.control = trainControl(method = "cv", number = 10)
#cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
#train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + Effec + Timeli + Poverty + Income + Density, data = train.kidney, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0039

tree1.kidney = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                       Effec + Timeli + Poverty + Income + Density, data = train.kidney, cp = 0.0039)

# Find RMSE 
tree1.kidney.pred = predict(tree1.kidney, newdata=test.kidney)
RMSEs[6,1] <- RMSE(tree1.kidney.pred, log10(test.kidney$AMP))

#Readable Tree
tree.readable.kidney = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                               Effec + Timeli + Poverty + Income + Density, data = train.kidney, cp = 0.008)
prp(tree.readable.kidney, main = "KIDNEY & URINARY TRACT INFECTIONS W/O MCC")


# Random Forest
forest.kidney = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.kidney, ntree = 200)

# Find Random Forest RMSE 
forest.kidney.pred = predict(forest.kidney, newdata=test.kidney)
RMSEs[6,2] <- RMSE(forest.kidney.pred, log10(test.kidney$AMP)) 

#Important variables
varImpPlot(forest.kidney, main = "Variable Importance for Kidney Random Forest")

View(RMSEs)








#############Linear Regression
# We would like to predict log10(AMP)

# Best subset selection
## install.packages("leaps")

regfitfull = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                          Income +  Density + Rating + Mort.code + Safety.code + 
                          Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df, nvmax = 11, method = "forward")
summary(regfitfull)

reg.summary <- summary(regfitfull)


#Use BIC 
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "All DRGs")

#Choose number of variables by looking at where the min BIC is (or the inflection point)
which.min(reg.summary$bic)
#Since min is at 10, I'll do inflection point of 9 instead. 

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.916037, so I will use 9 variables.


#Create model and find RMSE
linreg <- lm(log10(AMP) ~ Timeli.code + Density + log10(Dischar) + Income + Poverty + Safety.code + Exper.code + Effec.code + Rating, data = train)
linreg.pred = predict(linreg, newdata=test)
RMSEs[1,3] <- RMSE(linreg.pred, log10(test$AMP)) 




# Create model for just SIMPLE PNEUMONIA & PLEURISY W CC 
regfitfull.pneu = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                               Income +  Density + Rating + Mort.code + Safety.code + 
                               Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.pneu, nvmax = 11, method = "forward")
summary(regfitfull.pneu)

reg.summary.pneu <- summary(regfitfull.pneu)


#Use BIC 
plot(reg.summary.pneu$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "SIMPLE PNEUMONIA & PLEURISY W CC")

#Choose number of variables by looking at where the min BIC is (or the inflection point)
which.min(reg.summary.pneu$bic) #min at 7

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.pneu$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #Since the min occurs before the inflection point, I'll choose 7 variables.


#Create model and find RMSE
linreg.pneu <- lm(log10(AMP) ~ Density + Timeli.code + log10(Dischar) + Rating + Income 
                  + Poverty + Mort.code, data = train.pneu)
summary(linreg.pneu)
linreg.pred.pneu = predict(linreg, newdata=test.pneu)
RMSEs[2,3] <- RMSE(linreg.pred.pneu, log10(test.pneu$AMP)) 




# Create model for just SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC 
regfitfull.sept = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                               Income +  Density + Rating + Mort.code + Safety.code + 
                               Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.sept, nvmax = 11, method = "forward")
summary(regfitfull.sept)

reg.summary.sept <- summary(regfitfull.sept)


#Use BIC 
plot(reg.summary.sept$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC ")

#Find where min BIC is
which.min(reg.summary.sept$bic) #at 6
#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.sept$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 7.713419. Since the min occurs first, I'll chose 6 variables


#Create model and find RMSE
linreg.sept <- lm(log10(AMP) ~ Density + Timeli.code + Income + Poverty + Rating + Mort.code, 
                 data = train.sept)

linreg.pred.sept = predict(linreg, newdata=test.sept)
RMSEs[3,3] <- RMSE(linreg.pred.sept, log10(test.sept$AMP)) 



# Create model for just HEART FAILURE & SHOCK W CC 
regfitfull.heart = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                                Income +  Density + Rating + Mort.code + Safety.code + 
                                Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.heart, nvmax = 11, method = "forward")
summary(regfitfull.heart)

reg.summary.heart <- summary(regfitfull.heart)


#Use BIC 
plot(reg.summary.heart$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "HEART FAILURE & SHOCK W CC")

#Find where min BIC is
which.min(reg.summary.heart$bic) #at 7
#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.heart$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.21602, so I will use where the min is, 7 variables




#Create model and find RMSE
linreg.heart <- lm(log10(AMP) ~ Density + Timeli.code + Rating + log10(Dischar)
                   + Income + Poverty + Mort.code, data = train.heart)
linreg.pred.heart = predict(linreg, newdata=test.heart)
RMSEs[4,3] <- RMSE(linreg.pred.heart, log10(test.heart$AMP))



# Create model for just ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC 
regfitfull.esoph = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                                Income +  Density + Rating + Mort.code + Safety.code + 
                                Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.esoph, nvmax = 11, method = "forward")
summary(regfitfull.esoph)

reg.summary.esoph <- summary(regfitfull.esoph)


#Use BIC 
plot(reg.summary.esoph$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC")

#Find where min BIC is
which.min(reg.summary.esoph$bic) #at 7
#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.esoph$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.538392, so I'll use where the min is, 7 variables


#Create model and find RMSE
linreg.esoph <- lm(log10(AMP) ~ Timeli.code + Density + Rating + log10(Dischar) + 
                     + Mort.code + Income + Poverty, data = train.esoph)
linreg.pred.esoph = predict(linreg, newdata=test.esoph)
RMSEs[5,3] <- RMSE(linreg.pred.esoph, log10(test.esoph$AMP))



# Create model for just KIDNEY & URINARY TRACT INFECTIONS W/O MCC 
regfitfull.kidney = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                                 Income +  Density + Rating + Mort.code + Safety.code + 
                                 Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.kidney, nvmax = 11, method = "forward")
summary(regfitfull.kidney)

reg.summary.kidney <- summary(regfitfull.kidney)


#Use BIC 
plot(reg.summary.kidney$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "KIDNEY & URINARY TRACT INFECTIONS W/O MCC")

#Find where min BIC is
which.min(reg.summary.kidney$bic) #at 7
#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.kidney$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.538392, so I will the where the min is instead, 7


#Create model and find RMSE
linreg.kidney <- lm(log10(AMP) ~ Timeli.code + Density + Rating + log10(Dischar) + Mort.code +
                      Income + Poverty, data = train.kidney)

linreg.pred.kidney = predict(linreg, newdata=test.kidney)
RMSEs[6,3] <- RMSE(linreg.pred.kidney, log10(test.kidney$AMP)) 



#Summaries of linear regression models
summary(linreg)
summary(linreg.pneu)
summary(linreg.sept)
summary(linreg.heart)
summary(linreg.esoph)
summary(linreg.kidney)








############ Visualizations and additional tables
### ALL DRGS
#log10(AMP) vs. log10(Dischar)
ggplot(df.vis, aes(log10(Dischar),log10(AMP))) + 
  geom_point(shape = 3, position = "jitter", alpha = 0.5) +
  geom_smooth(method = lm) +  
  ggtitle("log10(Dischar) vs. log10(AMP) for All DRGs")

#log10(AMP) vs. log10(Density)
ggplot(df.vis, aes(log10(Density),log10(AMP))) + 
  geom_point(shape = 3, position = "jitter", alpha = 0.5) +
  geom_smooth(method = lm) +  
  ggtitle("log10(Density) vs. log10(AMP) for All DRGs")


### Pneumonia Analysis
#log10(AMP) vs. log10(Density)
ggplot(df.pneu, aes(log10(Density),log10(AMP))) + 
  geom_point(shape = 3, position = "jitter", alpha = 0.5) +
  geom_smooth(method = lm) +  
  ggtitle("log10(Density) vs. log10(AMP) for Pneumonia")

#Regions for pneumonia
ggplot(df.pneu, aes(Region, AMP, fill = Region)) + 
  geom_boxplot() + ggtitle("Boxplots of Average Medicare Payments by Region for Pneumonia DRG")

#Region mean AMPs for pneumonia
means.regions.pneu <- summarise(group_by(df.pneu, Region), mean(AMP), sd(AMP))
View(means.regions.pneu)