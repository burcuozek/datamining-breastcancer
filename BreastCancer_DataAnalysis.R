################################################################################################
#Breast Cancer Project 
#Data Visualization and Model Implementation
#Author: Burcu Ozek
################################################################################################

#required packages
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("olsrr")
install.packages("ROSE")
installed.packages("pscl")

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
library(ROSE)
library(pscl)

#data installation and minor changes

data <- read.csv("C:/Users/project/data.csv")
colnames(data) <- c("id", "diagnosis", "radius_m", "texture_m", "perimeter_m", "area_m", "smooth_m","compact_m", "concavity_m", "concave_m", "symmetry_m","fractal_m",
                    "radius_se", "texture_se", "perimeter_se", "area_se", "smooth_se","compact_se", "concavity_se", "concave_se", "symmetry_se","fractal_se",
                    "radius_w", "texture_w", "perimeter_w", "area_w", "smooth_w","compact_w", "concavity_w", "concave_w", "symmetry_w","fractal_w")
data <- data[,-33]
View(data)

#missing data check

mis_data <- data.frame(missing_matrix <- is.na(data))
count(mis_data, vars=FALSE)

######################## Scatter Plots  ###########################################

#Matrix scatterplots of the mean variables
plot(data[, c(3:12)])
# alternative, nicer plot 
ggpairs(data[, c(3:12)])

#Matrix scatterplots of the se variables
plot(data[, c(13:22)])
# alternative, nicer plot 
ggpairs(data[, c(13:22)])

#Matrix scatterplots of the worst variables
plot(data[, c(23:32)])
# alternative, nicer plot 
ggpairs(data[, c(23:32)])

######################### Boxplot    ############################################

#Boxplots of the mean values
rad <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$radius_m)) + ylab("Radius")

text <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$texture_m)) + ylab("Texture")

peri <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$perimeter_m)) + ylab("Perimeter")

area <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$area_m)) + ylab("Area")

smoot <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$smooth_m)) + ylab("Smoothness")

compact <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$compact_m)) + ylab("Compactness")

concavity <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$concavity_m)) + ylab("Concavity")

concave <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$concave_m)) + ylab("Concave Points")

symmetry <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$symmetry_m)) + ylab("Symmetry")

fractal <- ggplot(data =data) +
  geom_boxplot(mapping = aes(y=data$fractal_m)) + ylab("Fractal Dimension")

boxplots_mean <- ggarrange(rad, text, peri ,area, smoot, compact, concavity, concave, symmetry, fractal + rremove("x.text"), 
          ncol = 2, nrow = 5) 
annotate_figure(boxplots_mean,
                top = "Boxplots of the mean variables")

#########################    Heatmap  ############################################

data_heat <- data[,-c(1:2)]

## heatmap with values
heatmap.2(cor(data_heat), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(data_heat),1),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

############################Turning M and B values into binaries ####################

data <- data[,-1]
data$diagnosis <- as.numeric(gsub("M", 1, gsub("B", 0, data$diagnosis)))

############################# TRAINING and VALIDATION DATASETS ########################

set.seed(123)

train_percentage = 0.7

smp_siz = nrow(data)*train_percentage
train_ind = sample(seq_len(nrow(data)),size = smp_siz)

data_train =data[train_ind,] 
data_valid =data[-train_ind,]  

######################## Model with training data ###################################################

#logistic model
model <- glm(diagnosis ~ radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
    radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
      radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w,
    data = data_train)

summary(model)

pR2(model)

#forward logistic model

model_forw <- step(glm(diagnosis ~1 , data = data_train),direction="forward",scope=~radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
                     radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
                     radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w)
summary(model_forw)

pR2(model_forw)

#backward logistic model

model_back <- step(glm(diagnosis ~. , data = data_train),direction="backward")
summary(model_back)

pR2(model_back)

#both way stepwise logistic model

model_both<- step(glm(diagnosis ~1 , data = data_train),direction="both",scope=~radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
                    radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
                    radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w)
summary(model_both)

pR2(model_both)

#model with interaction terms

model_inter <- glm(diagnosis ~ (radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
                    radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
                    radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w)^2,
                  data = data_train)

summary(model_inter)

pR2(model_inter)

#model with interactions stepwise both ways

model_inter_both <- step(glm(diagnosis ~1 , data = data_train),direction="both",scope=~(radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
                           radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
                           radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w)^2)
summary(model_inter_both)

pR2(model_inter_both)

################################# Model with Validation Dataset ###################################

#model
model_valid <- glm(diagnosis ~ radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
               radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
               radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w,
             data = data_valid)

summary(model_valid)

pR2(model_valid)

#model with interaction and stepwise
model_inter_both_valid <- step(glm(diagnosis ~1 , data = data_valid),direction="both",scope=~(radius_m+ texture_m + perimeter_m + area_m + smooth_m + compact_m + concavity_m + concave_m + symmetry_m + fractal_m +
                                                                                          radius_se+ texture_se + perimeter_se + area_se + smooth_se + compact_se + concavity_se + concave_se + symmetry_se + fractal_se +
                                                                                          radius_w + texture_w + perimeter_w + area_w + smooth_w + compact_w + concavity_w + concave_w + symmetry_w + fractal_w)^2)
summary(model_inter_both_valid)

pR2(model_inter_both_valid)

#Future works:

#ROC curve
#Lift & decile Charts
#Confusion Matrix 
