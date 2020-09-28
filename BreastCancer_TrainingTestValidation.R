################################################################################################
#Breast Cancer Classification - Project 
#Author: Burcu Ozek
################################################################################################

#required packages
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("ROSE")
installed.packages("pscl")
install.packages("rpart")
install.packages("DescTools")
install.packages("caret")
install.packages("lift")

#required librararies
library(ggpubr)
library(ggplot2)
library(readcsv)
library(GGally)
library(readcsv)
library(gplots)
library(ROSE)
library(pscl)
library(rpart)
library(caret)
library(lift)

#data installation and minor changes

data <- read.csv("C:/Users/ozek.b/Desktop/project/data.csv")
colnames(data) <- c("id", "diagnosis", "radius_m", "texture_m", "perimeter_m", "area_m", "smooth_m","compact_m", "concavity_m", "concave_m", "symmetry_m","fractal_m",
                    "radius_se", "texture_se", "perimeter_se", "area_se", "smooth_se","compact_se", "concavity_se", "concave_se", "symmetry_se","fractal_se",
                    "radius_w", "texture_w", "perimeter_w", "area_w", "smooth_w","compact_w", "concavity_w", "concave_w", "symmetry_w","fractal_w")
data <- data[,-33]
View(data)

############################Turning M and B values into binaries ####################

data <- data[,-1]
data$diagnosis <- as.numeric(gsub("M", 1, gsub("B", 0, data$diagnosis)))

############################# TRAINING and VALIDATION DATASETS ########################

set.seed(123)

train_percentage = 0.7

smp_siz = nrow(data)*train_percentage
train_ind = sample(seq_len(nrow(data)),size = smp_siz)

data_train =data[train_ind,] 
data_valid =data[-train_ind,] ?

######################## Model with training dataset ###################################################
#logistic model
model <- glm(diagnosis ~ radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m ,
             data = data_train)

summary(model)

pR2(model)
PseudoR2(model, which = "McFaddenAdj")

#forward logistic model

model_forw <- step(glm(diagnosis ~1 , data = data_train),direction="forward",scope=~radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m)
summary(model_forw)
pR2(model_forw)
PseudoR2(model_forw, which = "McFaddenAdj")


#backward logistic model
model_back <- step(glm(diagnosis ~. , data = data_train),direction="backward")
summary(model_back)

pR2(model_back)
PseudoR2(model_back, which = "McFaddenAdj")

#both way stepwise logistic model

model_both<- step(glm(diagnosis ~1 , data = data_train),direction="both",scope=~radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m)
summary(model_both)

pR2(model_both)
PseudoR2(model_both, which = "McFaddenAdj")

#model with interaction terms

model_inter <- glm(diagnosis ~ (radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m )^2,
                   data = data_train)

summary(model_inter)

pR2(model_inter)
PseudoR2(model_inter, which = "McFaddenAdj")

#model with interactions stepwise both ways

model_inter_both <- step(glm(diagnosis ~1 , data = data_train),direction="both",scope=~(radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m)^2)
summary(model_inter_both)

pR2(model_inter_both)
PseudoR2(model_inter_both, which = "McFaddenAdj")

################################# Model with Validation Dataset ###################################

#forward logistic model

model_forw_valid <- step(glm(diagnosis ~1 , data = data_valid),direction="forward",scope=~radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m)
summary(model_forw_valid)
pR2(model_forw_valid)
PseudoR2(model_forw_valid, which = "McFaddenAdj")


#model with interaction and stepwise

model_inter_both_valid <- step(glm(diagnosis ~1 , data = data_valid),direction="both",scope=~(radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m)^2)
summary(model_inter_both_valid)

pR2(model_inter_both_valid)
PseudoR2(model_inter_both_valid, which = "McFaddenAdj")

#####################################Best Model after calculations ##################

model_best <- step(glm(diagnosis ~1 , data = data_valid),direction="forward",scope=~radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m, family= binomial)

######################## Prediction using the best model ##########################

#validation data
predict_model_best <- predict.glm(model_best, type = 'response')
predict_model_best

p_best <- 1/(1+ exp(-predict_model_best))

roc.curve(data_valid$diagnosis, p_best, col="red", lwd =5)

#training data
predict_model_forw <- predict.glm(model_forw, type = 'response')
predict_model_forw

p_model_forw <- 1/(1+ exp(-predict_model_forw))

roc.curve(data_train$diagnosis, predict_model_forw, col="red", lwd =5)

############################ Lift Chart #################################################

#If we can find time we can automatically find the optimal threshold value

######################## Confusion Matrix #################################################

pred_best5 <- ifelse(p_best> 0.5, 1,0)
confusionMatrix(as.factor(pred_best5), as.factor(data_valid$diagnosis))

pred_best6 <- ifelse(p_best> 0.6, 1,0)
confusionMatrix(as.factor(pred_best6), as.factor(data_valid$diagnosis))

pred_best7 <- ifelse(p_best> 0.7, 1,0)
confusionMatrix(as.factor(pred_best7), as.factor(data_valid$diagnosis))

