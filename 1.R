install.packages("tidyverse")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")

library(tidyverse)
library(caret)
library(randomForest)
library(e1071)

bike_data <- read.csv("C:/Users/HP/Downloads/DMA_assignment_Dataset/bike_sales_2024.csv")

str(bike_data)
summary(bike_data)

bike_data <- bike_data %>% drop_na()
bike_data$Category <- as.factor(bike_data$Category)

set.seed(123)
trainIndex <- createDataPartition(bike_data$Sales, p = 0.8, list = FALSE)
train_data <- bike_data[trainIndex, ]
test_data <- bike_data[-trainIndex, ]

model_rf <- randomForest(Sales ~ ., data = train_data)

predictions <- predict(model_rf, test_data)
results <- postResample(predictions, test_data$Sales)
print(results)

new_data <- data.frame(Feature1 = value1, Feature2 = value2)
predicted_sales <- predict(model_rf, new_data)
print(predicted_sales)
# Summary statistics for the numerical columns
summary(bike_data)

# Check for missing values
colSums(is.na(bike_data))

# Get the structure of the dataset
str(bike_data)

ggplot(bike_data, aes(x = Price)) + 
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribution of Bike Prices", x = "Price", y = "Frequency")
ggplot(bike_data, aes(x = Bike_Model, y = Quantity, fill = Bike_Model)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Sales Quantity by Bike Model", x = "Bike Model", y = "Quantity Sold")


ggplot(bike_data, aes(x = Store_Location, y = Quantity, fill = Store_Location)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Sales Quantity by Store Location", x = "Store Location", y = "Quantity Sold")


ggplot(bike_data, aes(x = Customer_Age)) + 
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Age Distribution of Customers", x = "Age", y = "Frequency")
# Correlation between numerical features
cor(bike_data %>% select(Price, Quantity, Customer_Age))



ggplot(bike_data, aes(x = Payment_Method, fill = Payment_Method)) + 
  geom_bar() + 
  theme_minimal() + 
  labs(title = "Payment Methods Distribution", x = "Payment Method", y = "Count")


ggplot(bike_data, aes(x = Customer_Gender, y = Quantity, fill = Customer_Gender)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Sales Quantity by Customer Gender", x = "Gender", y = "Quantity Sold")
# Convert categorical variables to factors if needed
bike_data$Customer_Gender <- as.factor(bike_data$Customer_Gender)
bike_data$Bike_Model <- as.factor(bike_data$Bike_Model)
bike_data$Store_Location <- as.factor(bike_data$Store_Location)
bike_data$Payment_Method <- as.factor(bike_data$Payment_Method)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(bike_data$Quantity, p = 0.8, list = FALSE)
train_data <- bike_data[trainIndex, ]
test_data <- bike_data[-trainIndex, ]

# Train a Random Forest model to predict Quantity
model_rf <- randomForest(Quantity ~ Price + Customer_Age + Customer_Gender + Bike_Model + Store_Location, data = train_data)

# Evaluate the model
predictions <- predict(model_rf, test_data)
results <- postResample(predictions, test_data$Quantity)
print(results)

# Feature importance
importance(model_rf)
varImpPlot(model_rf)

