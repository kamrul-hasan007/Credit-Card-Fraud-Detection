head(creditcard)
summary(creditcard)

ls()

#renamed my dataset
data <- creditcard

#injecting issues in my dataset
data$Amount[sample(1:nrow(data), 100)] <- NA
data <- rbind(data, data[1:50, ])
data$Amount[1:10] <- -999

#Missing Values
sum(is.na(data))
data$Amount[is.na(data$Amount)] <- mean(data$Amount, na.rm = TRUE)

#Removed duplicate rows from the dataset
data <- data[!duplicated(data), ]

#Replaced invalid negatives with median
data$Amount[data$Amount < 0] <- median(data$Amount)


#Outliers removed using IQR bounds
boxplot(data$Amount)
Q1 <- quantile(data$Amount, 0.25)
Q3 <- quantile(data$Amount, 0.75)
IQR <- Q3 - Q1

data <- data[data$Amount > (Q1 - 1.5*IQR) & data$Amount < (Q3 + 1.5*IQR), ]


#Converted target to factor and normalized Amount(Transformation)
data$Class <- as.factor(data$Class)
data$Amount <- scale(data$Amount)


#Balanced classes to improve analysis and modeling.
install.packages("ROSE")
library(ROSE)

data_balanced <- ovun.sample(Class ~ ., data = data, method = "both", N = 10000)$data

table(data_balanced$Class)




#Exploratory Data Analysis (EDA)

#Summary
summary(data_balanced)


#Mean Comparison
aggregate(Amount ~ Class, data = data_balanced, mean)


#Variation (Spread)
aggregate(Amount ~ Class, data = data_balanced, sd)


#Visualizations (ggplot2)
install.packages("ggplot2")
library(ggplot2)

# Histogram
ggplot(data_balanced, aes(x = Amount)) + geom_histogram(bins = 50)

# Boxplot by class
ggplot(data_balanced, aes(x = Class, y = Amount)) + geom_boxplot()

#Train-Test Split
set.seed(123)
train_index <- sample(1:nrow(data_balanced), 0.7 * nrow(data_balanced))
train <- data_balanced[train_index, ]
test <- data_balanced[-train_index, ]

#Logistic Regression
model_log <- glm(Class ~ ., data = train, family = "binomial")

pred_prob <- predict(model_log, test, type = "response")
pred_log <- ifelse(pred_prob > 0.5, 1, 0)

#Random Forest
install.packages("randomForest")
library(randomForest)

model_rf <- randomForest(Class ~ ., data = train, ntree = 100)
pred_rf <- predict(model_rf, test)

#Evaluation
# Logistic Regression Confusion Matrix
table(Predicted = pred_log, Actual = test$Class)

# Random Forest Confusion Matrix
table(Predicted = pred_rf, Actual = test$Class)

#accuracy
mean(pred_log == test$Class)
mean(pred_rf == test$Class)

