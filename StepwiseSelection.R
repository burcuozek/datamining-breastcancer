################################################################################################
# Model Selection - Stepwise Regression
#Author: Burcu Ozek

################################################################################################


#packages
install.packages("olsrr")
install.packages("car")
install.packages("dplyr")

#libraries
library(readxl)
library(olsrr)
library(car)

#data read
ff_data <- read_excel("C:/Users/Forest Fires Data (1).xlsx")

#Part A
#Matrix scatterplots of the mean variables
cont_predictors <- ff_data[,-c(1:4)]
cont_predictors <- cont_predictors[,-9]
ggpairs(cont_predictors)

#Part B
model <- lm(Area ~ X + Y+ Month + Day + FFMC + DMC + DC + ISI +Temp + RH + Wind + Rain , data = ff_data)
summary(model)

model_all <- ols_step_all_possible(model)
summary(model_all)

#Part c
model_best <- lm(Area ~ X + DMC + RH , data = ff_data)
confint(model_best)

#traditional
par(mfrow=c(2,2))
plot(model_best)

#enhanced
par(mfrow=c(1,1))
qqPlot(model_best, labels = row.names(ff_data), id.method = "identify", simulate= TRUE, main = "Q-Q Plot" )

durbinWatsonTest(model_best)
residplot(model)

#Outlier Test
outlierTest(model_best)
ff_data <- ff_data[-c(239, 416,480),]

#selecting the best model
model <- lm(Area ~ X + Y+ Month + Day + FFMC + DMC + DC + ISI +Temp + RH + Wind + Rain , data = ff_data)
summary(model)

model_all <- ols_step_all_possible(model)
summary(model_all)

#Forward regression model 
model_forw <- step(lm(Area ~1 , data = ff_data),direction="forward",scope=~X + Y+ Month + 
                     Day + FFMC + DMC + DC + ISI +Temp + RH + Wind + Rain)
summary(model_forw)

#Backward regression model
model_back <- step(glm(Area ~. , data = ff_data),direction="backward")
summary(model_back)

#Stepwise both ways regression model 
model_forw <- step(lm(Area ~1 , data = ff_data),direction="both",scope=~X + Y+ Month + 
                     Day + FFMC + DMC + DC + ISI +Temp + RH + Wind + Rain)
summary(model_forw)




