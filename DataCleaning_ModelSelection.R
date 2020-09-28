################################################################################################
# Data Cleaning - Model Selection
# Author: Burcu Ozek
################################################################################################

install.packages("ggpubr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("olsrr")
install.packages("car")
install.packages("carData")

#required librararies
library(ggpubr)
library(ggplot2)
library(readcsv)
library(GGally)
library(readcsv)
library(dplyr)
library(tidyr)
library(gplots)
library(olsrr)
library(car)
library(carData)

library(readxl)
Data <- read_excel("Desktop/Concrete Slump Test Data.xlsx")
View(Data)

##### Create a scatterplot matrix of “Concrete Slump Test Data” and select an initial set of predictor variables ###

plot(Data[, c(2:8)])
ggpairs(Data[, c(2:8)])

#### Build a few potential regression models using “Concrete Slump Test Data” ####

model_slump <- lm( Slump ~ Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate, Data)
model_slump            
summary(model_slump)

model_slump_flow <- lm( SlumpFlow ~ Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate, Data)
model_slump_flow           
summary(model_slump_flow)

model_CS <- lm( CompressiveStrength~ Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate, Data)
model_CS         
summary(model_CS)

# all possible model
model_all <- ols_step_all_possible(model_CS)
summary(model_all)

#### Perform regression diagnostics using both typical approach and enhanced approach ####

confint(model_slump)
confint(model_slump_flow)
confint(model_CS)

# typical approach
par(mfrow = c(2,2))
plot(model_CS)

par(mfrow = c(2,2))
plot(model_slump)

par(mfrow = c(2,2))
plot(model_slump_flow)

# enchange approach
library(car)
par(mfrow = c(2,2))
qqPlot(model_slump,labels=row.names(data),id.method = "identify",simulate = TRUE , main ="Q-Q Plot Slump")
qqPlot(model_slump_flow,labels=row.names(data),id.method = "identify",simulate = TRUE , main ="Q-Q Plot Slum Flow")
qqPlot(model_CS,labels=row.names(data),id.method = "identify",simulate = TRUE , main ="Q-Q Plot Compressive Strength")
#qqplot(model_CS, labels=row.names(data), id.method = "identify", simulate = TRUE , main ="Q-Q Plot")


# Identify unusual observations and take corrective measures

outlierTest(model_slump)
outlierTest(model_slump_flow)
outlierTest(model_CS)

corrected_data <- Data[-8,]
corrected_data<- Data[-48,]
corrected_data<- Data[-67,]
corrected_data

# Select the best regression model
model_all_slump <- ols_step_all_possible(model_slump)
model_all_slump_flow <- ols_step_all_possible(model_slump_flow)
model_all_CS <- ols_step_all_possible(model_CS)

#Fine tune the selection of predictor variables
#stepwise
model_step_slump<- step(glm(Slump ~1 , data = corrected_data),direction="both",scope=~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate)
model_step_slump
model_step_slump_flow<- step(glm(SlumpFlow ~1 , data = corrected_data),direction="both",scope=~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate)
model_step_slump_flow
model_step_CS<- step(glm(CompressiveStrength ~1 , data = corrected_data),direction="both",scope=~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate)
model_step_CS

model_for_slump<- step(glm(Slump ~1 , data = corrected_data),direction="forward",scope=~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate)
model_for_slump
model_for_slump_flow<- step(glm(SlumpFlow ~1 , data = corrected_data),direction="forward",scope=~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate)
model_for_slump_flow
model_for_CS<- step(glm(CompressiveStrength ~1 , data = corrected_data),direction="forward",scope=~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate)
model_for_CS

model_back_slump<- step(glm(Slump ~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate , data = corrected_data),direction="backward")
model_back_slump
model_back_slump_flow<- step(glm(SlumpFlow ~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate , data = corrected_data),direction="backward")
model_back_slump_flow
model_back_CS<- step(glm(CompressiveStrength ~Cement+ Slag+FlyAsh+Water+SP+CoarseAggregate+FineAggregate , data = corrected_data),direction="backward")
model_back_CS
