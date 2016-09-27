## Data Wrangling 

#load libraries

library(dplyr)
library(tidyr)
library(ggplot2)

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

levels(df$Mort) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Safety) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Readmiss) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Exper) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Effec) <- c("Below Avg", "Avg", "Above Avg")
levels(df$Timeli) <- c("Below Avg", "Avg", "Above Avg")



### DECISION TREES AND RANDOM FORESTS

library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(e1071)
library(randomForest)

# Create train and test set
set.seed(1234)
split = sample.split(df$AMP, SplitRatio = 0.8)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)

#Create decision tree for all DRGs

tr.control = trainControl(method = "cv", number = 10)

cp.grid = expand.grid(.cp = seq(0.000001,0.00005,0.000001)) 

train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 4.8e-05. 

tree1 = rpart(log10(AMP) ~  Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                Effec + Timeli + Poverty + Income + Density, data = train, cp = 4.8e-05)

prp(tree1, main = "All DRGs")

# Find RMSE 
tree1.pred = predict(tree1, newdata=test)
RMSE(tree1.pred, log10(test$AMP)) # RMSE = 0.2768306

#Create Readable Tree for Visual Analysis
tree.readable = rpart(log10(AMP) ~  Dischar + Region  + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                Effec + Timeli + Poverty + Income + Density, data = train, cp = .002)

prp(tree.readable, main = "Readable Tree - All DRG's")


# Random Forest
set.seed(1234)
split = sample.split(train$AMP, SplitRatio = 0.2)
train.smaller = subset(df, split==TRUE)
forest1 = randomForest(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty 
                       + Density, data = train.smaller, ntree = 200)

forest1.pred = predict(forest1, newdata=test)
RMSE(forest1.pred, log10(test$AMP)) # RMSE = 0.281763

#Important variables
varImpPlot(forest1)


#Try making trees, but for only one DRG at a time.
#Start with SIMPLE PNEUMONIA & PLEURISY W CC
df.pneu <- subset(df, DRGCode == 194)

set.seed(1234)
split = sample.split(df.pneu$AMP, SplitRatio = 0.8)
train.pneu = subset(df.pneu, split==TRUE)
test.pneu = subset(df.pneu, split==FALSE)

tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = seq(0.0001,0.005,0.0001)) 
train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train.pneu, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0019

tree1.pneu = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + Income + Density, data = train.pneu, cp = 0.0019)
prp(tree1.pneu, main = "SIMPLE PNEUMONIA & PLEURISY W CC") 

# Find RMSE 
tree1.pneu.pred = predict(tree1.pneu, newdata=test.pneu)
RMSE(tree1.pneu.pred, log10(test.pneu$AMP)) # RMSE = 0.08174722

#Create Readable Tree for Visual Analysis
tree.pneu.readable = rpart(log10(AMP) ~ Dischar + Region  + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                        Effec + Timeli + Poverty + Income + Density, data = train.pneu, cp = .008)

prp(tree.pneu.readable, main = "SIMPLE PNEUMONIA & PLEURISY W CC")

# Random Forest
forest.pneu = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.pneu, ntree = 200)

# Find Random Forest RMSE 
forest.pneu.pred = predict(forest.pneu, newdata=test.pneu)
RMSE(forest.pneu.pred, log10(test.pneu$AMP)) # RMSE = 0.07018965

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
train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train.sept, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.004

tree1.sept = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + Income + Density, data = train.sept, cp = 0.004)
prp(tree1.sept, main = "SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC")

# Find RMSE 
tree1.sept.pred = predict(tree1.sept, newdata=test.sept)
RMSE(tree1.sept.pred, log10(test.sept$AMP)) # RMSE = 0.06898943

#Create readable tree
tree.readable.sept = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                     Effec + Timeli + Poverty + Income + Density, data = train.sept, cp = 0.008)
prp(tree.readable.sept, main = "SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC")

# Random Forest
forest.sept = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.sept, ntree = 200)

# Find Random Forest RMSE 
forest.sept.pred = predict(forest.sept, newdata=test.sept)
RMSE(forest.sept.pred, log10(test.sept$AMP)) # RMSE = 0.06281184

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
train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train.heart, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0042

tree1.heart = rpart(log10(AMP) ~Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + Income + Density, data = train.heart, cp = 0.0042)
prp(tree1.heart, main = "HEART FAILURE & SHOCK W CC")

# Find RMSE 
tree1.heart.pred = predict(tree1.heart, newdata=test.heart)
RMSE(tree1.heart.pred, log10(test.heart$AMP)) # RMSE = 0.08521931

#Readable Tree
tree.readable.heart = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + Income + Density, data = train.heart, cp = 0.008)
prp(tree.readable.heart, main = "HEART FAILURE & SHOCK W CC")


# Random Forest
forest.heart = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.heart, ntree = 200)

# Find Random Forest RMSE 
forest.heart.pred = predict(forest.heart, newdata=test.heart)
RMSE(forest.heart.pred, log10(test.heart$AMP)) # RMSE = 0.07487536

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
train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train.esoph, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0026

tree1.esoph = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + Income + Density, data = train.esoph, cp = 0.0026)
prp(tree1.esoph, main = "ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC ")

# Find RMSE 
tree1.esoph.pred = predict(tree1.esoph, newdata=test.esoph)
RMSE(tree1.esoph.pred, log10(test.esoph$AMP)) # RMSE = 0.09360937

#Readable Tree
tree.readable.esoph = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                      Effec + Timeli + Poverty + Income + Density, data = train.esoph, cp = 0.008)
prp(tree.readable.esoph, main = "ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC ")


# Random Forest
forest.esoph = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.esoph, ntree = 200)

# Find Random Forest RMSE 
forest.esoph.pred = predict(forest.esoph, newdata=test.esoph)
RMSE(forest.esoph.pred, log10(test.esoph$AMP)) # RMSE = 0.0852079

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
train(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
        Effec + Timeli + Poverty + Income + Density, data = train.kidney, method = "rpart", trControl = tr.control, tuneGrid = cp.grid) 
#RMSE was used to select the optimal model using  the smallest value. The final value used for the model was cp = 0.0038

tree1.kidney = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                       Effec + Timeli + Poverty + Income + Density, data = train.kidney, cp = 0.0038)
prp(tree1.kidney, main = "KIDNEY & URINARY TRACT INFECTIONS W/O MCC")

# Find RMSE 
tree1.kidney.pred = predict(tree1.kidney, newdata=test.kidney)
RMSE(tree1.kidney.pred, log10(test.kidney$AMP)) # RMSE = 0.08245488

#Readable Tree
tree.readable.kidney = rpart(log10(AMP) ~ Dischar + Region + Owner + Rating + Mort + Safety + Readmiss + Exper + 
                       Effec + Timeli + Poverty + Income + Density, data = train.kidney, cp = 0.008)
prp(tree.readable.kidney, main = "KIDNEY & URINARY TRACT INFECTIONS W/O MCC")


# Random Forest
forest.kidney = randomForest(log10(AMP) ~ Dischar+ Region + Owner + Rating + Mort + Safety + Readmiss + Exper +  Effec + Timeli + Income + Poverty + Density, data = train.kidney, ntree = 200)

# Find Random Forest RMSE 
forest.kidney.pred = predict(forest.kidney, newdata=test.kidney)
RMSE(forest.kidney.pred, log10(test.kidney$AMP)) # RMSE = 0.07606605

#Important variables
varImpPlot(forest.kidney)











#############Linear Regression
# We would like to predict log10(AMP)

# Best subset selection
## install.packages("leaps")
library(leaps)

regfitfull = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                          Income +  Density + Rating + Mort.code + Safety.code + 
                          Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df, nvmax = 11, method = "forward")
summary(regfitfull)

reg.summary <- summary(regfitfull)


#Use BIC 
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "All DRGs")

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.619189, so I will use 9 variables.


#Create model and find RMSE
linreg <- lm(log10(AMP) ~ Timeli.code + Density + log10(Dischar) + Income + Poverty + Safety.code + Exper.code + Effec.code + Rating, data = train)
summary(linreg)
linreg.pred = predict(linreg, newdata=test)
RMSE(linreg.pred, log10(test$AMP)) # RMSE = 0.2908499




# Create model for just SIMPLE PNEUMONIA & PLEURISY W CC 
regfitfull.pneu = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                          Income +  Density + Rating + Mort.code + Safety.code + 
                          Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.pneu, nvmax = 11, method = "forward")
summary(regfitfull.pneu)

reg.summary.pneu <- summary(regfitfull.pneu)


#Use BIC 
plot(reg.summary.pneu$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "SIMPLE PNEUMONIA & PLEURISY W CC")

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.pneu$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.249382, so I will use 8 variables.


#Create model and find RMSE
linreg.pneu <- lm(log10(AMP) ~ Density + Timeli.code + log10(Dischar) + Rating + Income 
                  + Poverty + Mort.code + Readmiss.code, data = train.pneu)
summary(linreg.pneu)
linreg.pred.pneu = predict(linreg, newdata=test.pneu)
RMSE(linreg.pred.pneu, log10(test.pneu$AMP)) # RMSE = 0.1605963




# Create model for just SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC 
regfitfull.sept = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                               Income +  Density + Rating + Mort.code + Safety.code + 
                               Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.sept, nvmax = 11, method = "forward")
summary(regfitfull.sept)

reg.summary.sept <- summary(regfitfull.sept)


#Use BIC 
plot(reg.summary.sept$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "SEPTICEMIA OR SEVERE SEPSIS W/O MV 96+ HOURS W MCC ")

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.sept$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 7.756416, so I will use 8 variables.


#Create model and find RMSE
linreg.sept <- lm(log10(AMP) ~ Density + Timeli.code + Income + Poverty + Rating + Mort.code 
                  + Readmiss.code + log10(Dischar), data = train.sept)
summary(linreg.sept)
linreg.pred.sept = predict(linreg, newdata=test.sept)
RMSE(linreg.pred.sept, log10(test.sept$AMP)) # RMSE = 0.2168597



# Create model for just HEART FAILURE & SHOCK W CC 
regfitfull.heart = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                               Income +  Density + Rating + Mort.code + Safety.code + 
                               Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.heart, nvmax = 11, method = "forward")
summary(regfitfull.heart)

reg.summary.heart <- summary(regfitfull.heart)


#Use BIC 
plot(reg.summary.heart$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "HEART FAILURE & SHOCK W CC")

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.heart$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.297385, so I will use 8 variables.




#Create model and find RMSE
linreg.heart <- lm(log10(AMP) ~ Density + Timeli.code + Rating + log10(Dischar)
                   + Income + Poverty + Mort.code + Safety.code, data = train.heart)
summary(linreg.heart)
linreg.pred.heart = predict(linreg, newdata=test.heart)
RMSE(linreg.pred.heart, log10(test.heart$AMP)) # RMSE = 0.1467455



# Create model for just ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC 
regfitfull.esoph = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                               Income +  Density + Rating + Mort.code + Safety.code + 
                               Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.esoph, nvmax = 11, method = "forward")
summary(regfitfull.esoph)

reg.summary.esoph <- summary(regfitfull.esoph)


#Use BIC 
plot(reg.summary.esoph$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "ESOPHAGITIS, GASTROENT & MISC DIGEST DISORDERS W/O MCC")

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.esoph$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.355533, so I will use 8 variables.


#Create model and find RMSE
linreg.esoph <- lm(log10(AMP) ~ Timeli.code + Density + Rating + log10(Dischar) + 
                    Income + Poverty + Mort.code + Exper.code, data = train.esoph)
summary(linreg.esoph)
linreg.pred.esoph = predict(linreg, newdata=test.esoph)
RMSE(linreg.pred.esoph, log10(test.esoph$AMP)) # RMSE = 0.2715242



# Create model for just KIDNEY & URINARY TRACT INFECTIONS W/O MCC 
regfitfull.kidney = regsubsets(log10(AMP) ~ log10(Dischar)  + Poverty +  
                               Income +  Density + Rating + Mort.code + Safety.code + 
                               Readmiss.code + Exper.code + Effec.code + Timeli.code, data = df.kidney, nvmax = 11, method = "forward")
summary(regfitfull.kidney)

reg.summary.kidney <- summary(regfitfull.kidney)


#Use BIC 
plot(reg.summary.kidney$bic, xlab = "Number of Variables", ylab = "BIC Statistic", main = "KIDNEY & URINARY TRACT INFECTIONS W/O MCC")

#Find inflection point of BIC graph
nvar <- c(1:11)
model <- lm(reg.summary.kidney$bic ~ nvar + I(nvar^2))
summary(model)
coeff <- coefficients(model)

-coeff[2]/(2*coeff[3]) #This gives me 8.439649, so I will use 8 variables.


#Create model and find RMSE
linreg.kidney <- lm(log10(AMP) ~ Timeli.code + Density + Rating + log10(Dischar) + 
                      Income + Poverty + Mort.code + Exper.code, data = train.kidney)
summary(linreg.kidney)
linreg.pred.kidney = predict(linreg, newdata=test.kidney)
RMSE(linreg.pred.kidney, log10(test.kidney$AMP)) # RMSE = 0.2450633











############ Visualizations

#Start by  putting top DRGs together in one dataframe

df.top5 <- rbind(df.pneu, df.sept, df.heart, df.esoph, df.kidney)

# Shorten DRG names
df.top5$DRGName <- as.character(df.top5$DRGName) 
df.top5$DRGName <- as.factor(df.top5$DRGName)
levels(df.top5$DRGName) <- c("Esoph", "Heart", "Kidney", "Sept", "Pneu")

# Show distribution of AMP AND log10(AMP) divided up by DRG
ggplot(df.top5, aes(DRGName, AMP, color = DRGName)) + 
      geom_boxplot()  

ggplot(df.top5, aes(log10(AMP), fill = DRGName)) + 
  geom_density(alpha = 0.25)

# Look at log10(AMP) vs. density
ggplot(df.top5, aes(Density, log10(AMP), color = DRGName)) + 
      geom_point(shape = 3, position = "jitter", alpha = 0.5) +
      theme(legend.position = "bottom")


ggplot(df.top5, aes(log10(Density), log10(AMP), color = DRGName)) + 
  geom_point(shape = 3, position = "jitter", alpha = 0.5) +
  theme(legend.position = "bottom")

ggplot(df.top5, aes(Density, log10(AMP), color = DRGName)) + 
  geom_point(shape = 3, position = "jitter", alpha = 0.5) +
  facet_grid(.~DRGName)

ggplot(df.top5, aes(log10(Density), log10(AMP), color = DRGName)) + 
  geom_point(shape = 3, position = "jitter", alpha = 0.5) +
  facet_grid(.~DRGName)
  


# Plots comparing Timeli, Safety, Exper, Effec, Readmiss, and Mort

ggplot(df.top5, aes(log10(AMP), fill = Timeli)) + 
  geom_density(alpha = 0.25) + 
  facet_grid(DRGName~.)

ggplot(df.top5, aes(log10(AMP), fill = Safety)) + 
  geom_density(alpha = 0.25) + 
  facet_grid(DRGName~.)

ggplot(df.top5, aes(log10(AMP), fill = Exper)) + 
  geom_density(alpha = 0.25) + 
  facet_grid(DRGName~.)

ggplot(df.top5, aes(log10(AMP), fill = Effec)) + 
  geom_density(alpha = 0.25) + 
  facet_grid(DRGName~.)

ggplot(df.top5, aes(log10(AMP), fill = Readmiss)) + 
  geom_density(alpha = 0.25) + 
  facet_grid(DRGName~.)

ggplot(df.top5, aes(log10(AMP), fill = Mort)) + 
  geom_density(alpha = 0.25) + 
  facet_grid(DRGName~.)




#Plots about region
library(maps)
#load us map data
all_states <- map_data("state")


ggplot(data = df.kidney, aes(Longitude, Latitude, color = AMP)) + 
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" ) +
  geom_point() + 
  scale_colour_gradient(low="yellow1", high="maroon", trans="log") +
  coord_cartesian(xlim=c(-130,-70), ylim = c(25, 50))
  

ggplot(data = df, aes(Longitude, Latitude, color = AMP)) + 
  geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" ) +
  geom_point() + 
  scale_colour_gradient(low="yellow1", high="maroon", trans="log") +
  coord_cartesian(xlim=c(-130,-70), ylim = c(25, 50))


# Plots about hospital ownership
ggplot(df, aes(Owner, log10(AMP), fill = Owner)) + 
  geom_boxplot() + ggtitle("All DRGs")
  
