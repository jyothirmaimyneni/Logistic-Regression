# Load necessary libraries
library(tidyverse)
library(caret)
library(lubridate)

# Load the data
data <- read.csv('Diabetes Dataset Aug 2023.csv')
#Figure.21
str(data)
summary(data)
# Select numerical columns for plotting
numerical_data <- data %>% select(AGE, Urea, Cr, HbA1c, Chol, TG, HDL, LDL, VLDL, BMI)

# Melt the data into long format for faceting
melted_data <- reshape2::melt(numerical_data)

# Plot histograms with facets//Figure.22
ggplot(melted_data, aes(x=value)) + 
  geom_histogram(binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.7) +
  facet_wrap(~variable, scales="free", ncol=3) + 
  labs(title="Distributions of Numerical Variables", x="", y="Frequency") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Data Preprocessing
# Replace negative HbA1c values with median
data$HbA1c[data$HbA1c < 0] <- median(data$HbA1c)
# Transform categorical variables//Figure.23
data$Gender <- ifelse(data$Gender == "M", 1, 0)
data$CLASS <- ifelse(data$CLASS == "Y", 1, ifelse(data$CLASS == "N", 0, 2))
str(data)
# Split the data
set.seed(42)
trainIndex <- createDataPartition(data$CLASS, p = 0.8, list = FALSE, times = 1)
dataTrain <- data[trainIndex, ]
dataTest <- data[-trainIndex, ]
# Filter out 'P' (pre-diabetic) entries for model estimation
dataTrain <- dataTrain[dataTrain$CLASS != 2, ]
dataTest <- dataTest[dataTest$CLASS != 2, ]

# Build the logistic regression model//Figure.24
logisticModel <- glm(CLASS ~ ., family = "binomial", data = dataTrain)
summary(logisticModel)
##VIF //Figure.28
install.packages('car')
library(car)
vif(logisticModel)

# Model Evaluation//Figure.25
prediction <- predict(logisticModel, newdata = dataTest, type = "response")
predictedClass <- ifelse(prediction > 0.5, 1, 0)
confusionMatrix <- table(dataTest$CLASS, predictedClass)
print(confusionMatrix)

# Test on 'P' cases//Figure.26
P_data <- data[data$CLASS == 2, ]
P_prediction <- predict(logisticModel, newdata = P_data, type = "response")
P_predictedClass <- ifelse(P_prediction > 0.5, 1, 0)
print(table(P_predictedClass))
###coefficients//Figure.27
summary(logisticModel)$coefficients

# AUC-ROC Curve//Figure.29
library(pROC)
roc_obj <- roc(dataTest$CLASS, predictedClass)
auc(roc_obj)
plot.roc(roc_obj, main="ROC Curve")
##
Classification_Accuracy <- sum(diag(dataTest$CLASS)/sum(predictedClass))
Classification_Accuracy