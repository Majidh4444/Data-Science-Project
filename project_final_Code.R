library(plyr)
library(rpart.plot)
library(caret)
library(gridExtra)
library(tidyverse)
library(rsample)
library(e1071)
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(rpart)
library(randomForest)
library(ggpubr)

# Load data
churn <- read.csv('D:\\Majidh\\Amrita\\Academics\\Sem 6\\R Programming\\Project\\Project\\Telco-Customer-Churn.csv')

# Glimpse data
glimpse(churn)

## Data Preprocessing
# Check for missing values
sapply(churn, function(x) sum(is.na(x)))

#Check the proportion 
sum(is.na(churn$TotalCharges))/nrow(churn)

# Remove rows with missing values
churn_clean <- churn[complete.cases(churn), ]

# Convert categorical variables to factors
churn_clean$SeniorCitizen <- as.factor(mapvalues(churn_clean$SeniorCitizen, from=c("0","1"), to=c("No", "Yes")))
churn_clean$MultipleLines <- as.factor(mapvalues(churn_clean$MultipleLines, from=c("No phone service"), to=c("No")))
for(i in 10:15){
  churn_clean[,i] <- as.factor(mapvalues(churn_clean[,i], from= c("No internet service"), to= c("No")))
}
churn_clean$customerID <- NULL

## Data Visualization for Descriptive Statistics
# Demographic data
p1 <- ggplot(churn_clean, aes(x = gender)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p2 <- ggplot(churn_clean, aes(x = SeniorCitizen)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p3 <- ggplot(churn_clean, aes(x = Partner)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p4 <- ggplot(churn_clean, aes(x = Dependents)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)

# Plot demographic data within a grid
grid.arrange(p1, p2, p3, p4, ncol=2)

# Offered services
p5 <- ggplot(churn_clean, aes(x = PhoneService)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p6 <- ggplot(churn_clean, aes(x = MultipleLines)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p7 <- ggplot(churn_clean, aes(x = InternetService)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p8 <- ggplot(churn_clean, aes(x = OnlineSecurity)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p9<- ggplot(churn_clean, aes(x = OnlineBackup)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p10 <- ggplot(churn_clean, aes(x = DeviceProtection)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p11 <- ggplot(churn_clean, aes(x = TechSupport)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p12 <- ggplot(churn_clean, aes(x = StreamingTV)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p13 <- ggplot(churn_clean, aes(x = StreamingMovies)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)

# Plot service data within a grid
grid.arrange(p5, p6, p7, p8, p9, p10, p11, p12, p13, ncol=3)

# Other categorical variables
p14 <- ggplot(churn_clean, aes(x = Contract)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p15 <- ggplot(churn_clean, aes(x = PaperlessBilling)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p16 <- ggplot(churn_clean, aes(x = PaymentMethod)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)

# Plot contract data within a grid
grid.arrange(p14, p15, p16, ncol=1)

# Quantitative variables
p17 <- ggplot(data = churn_clean, aes(tenure, color = Churn))+ 
  geom_freqpoly(binwidth = 5, size = 1)
p18 <- ggplot(data = churn_clean, aes(MonthlyCharges, color = Churn))+ 
  geom_freqpoly(binwidth = 5, size = 1)
p19 <- ggplot(data = churn_clean, aes(TotalCharges, color = Churn))+ 
  geom_freqpoly(binwidth = 200, size = 1)

# Plot quantitative data within a grid
grid.arrange(p17, p18, p19, ncol=1)

# Churn
p20 <- ggplot(churn_clean, aes(x = Churn)) + 
  geom_bar(aes(fill = Churn)) + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3)
p20

# Checking correlation
churn_clean %>%
  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>%
  cor() %>%
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)

# Splitting data into training and testing sets
set.seed(56)
split_train_test <- createDataPartition(churn_clean$Churn,p=0.7,list=FALSE)
dtrain<- churn_clean[split_train_test,]
dtest<-  churn_clean[-split_train_test,]

# Remove Total Charges from the training dataset
dtrain <- dtrain[,-19]
dtest <- dtest[,-19]

## Modelling
# Decision Tree
tr_fit <- rpart(Churn ~., data = dtrain, method="class")
rpart.plot(tr_fit)

tr_prob1 <- predict(tr_fit, dtest)
tr_pred1 <- ifelse(tr_prob1[,2] > 0.5,"Yes","No")
table(Predicted = tr_pred1, Actual = dtest$Churn)

tr_prob2 <- predict(tr_fit, dtrain)
tr_pred2 <- ifelse(tr_prob2[,2] > 0.5,"Yes","No")
tr_tab1 <- table(Predicted = tr_pred2, Actual = dtrain$Churn)
tr_tab2 <- table(Predicted = tr_pred1, Actual = dtest$Churn)

# Train
confusionMatrix(
  as.factor(tr_pred2),
  as.factor(dtrain$Churn),
  positive = "Yes" 
)

# Test
confusionMatrix(
  as.factor(tr_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)

tr_acc <- sum(diag(tr_tab2))/sum(tr_tab2)
tr_acc*100

# Random Forest
# Set control parameters for random forest model selection
ctrl <- trainControl(method = "cv", number=5, 
                     classProbs = TRUE, summaryFunction = twoClassSummary)

# Exploratory random forest model selection
#rf_fit1 <- train(Churn ~., data = dtrain,
#                  method = "rf",
#                  ntree = 75,
#                  tuneLength = 5,
#                  metric = "ROC",
#                  trControl = ctrl)
#
#saveRDS(rf_fit1, "Churn.RDS")
rf_fit1 <- readRDS("Churn.RDS")

# Run optimal model
rf_fit2 <- randomForest(factor(Churn) ~., data = dtrain, 
                        ntree = 75, mtry = 2, 
                        importance = TRUE, proximity = TRUE)
# Display variable importance from random tree
  varImpPlot(rf_fit2, sort=T, n.var = 10, 
           main = 'Top 10 important variables')

rf_pred1 <- predict(rf_fit2, dtest)
table(Predicted = rf_pred1, Actual = dtest$Churn)

plot(rf_fit2)

rf_pred2 <- predict(rf_fit2, dtrain)
rf_tab1 <- table(Predicted = rf_pred2, Actual = dtrain$Churn)
rf_tab2 <- table(Predicted = rf_pred1, Actual = dtest$Churn)

# Train
confusionMatrix(
  as.factor(rf_pred2),
  as.factor(dtrain$Churn),
  positive = "Yes" 
)

# Test
confusionMatrix(
  as.factor(rf_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)

rf_acc <- sum(diag(rf_tab2))/sum(rf_tab2)
rf_acc*100

# Logistic Regression
dtrain$gender = as.factor(dtrain$gender)
dtrain$Partner = as.factor(dtrain$Partner)
dtrain$Dependents = as.factor(dtrain$Dependents)
dtrain$PhoneService = as.factor(dtrain$PhoneService)
dtrain$InternetService = as.factor(dtrain$InternetService)
dtrain$Contract = as.factor(dtrain$Contract)
dtrain$PaperlessBilling = as.factor(dtrain$PaperlessBilling)
dtrain$PaymentMethod = as.factor(dtrain$PaymentMethod)
dtrain$Churn = as.factor(dtrain$Churn)

lr_fit <- glm(Churn ~., data = dtrain,
              family=binomial)
summary(lr_fit)

lr_prob1 <- predict(lr_fit, dtest, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = dtest$Churn)

lr_prob2 <- predict(lr_fit, dtrain, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$Churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$Churn)

# Train
confusionMatrix(
  as.factor(lr_pred2),
  as.factor(dtrain$Churn),
  positive = "Yes" 
)

# Test
confusionMatrix(
  as.factor(lr_pred1),
  as.factor(dtest$Churn),
  positive = "Yes" 
)

lr_acc<- sum(diag(lr_tab2))/sum(lr_tab2)
lr_acc

## Data Visualization based on models
p21 <- ggplot(churn_clean, aes(x = Contract, fill = Churn)) + 
  geom_bar() + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3) + 
  labs(title="Churn rate by contract status")
p21

p22 <- ggplot(churn_clean, aes(x = InternetService, fill = Churn)) + 
  geom_bar() + 
  geom_text(aes(y =..count.. -200, label = paste0(round(prop.table(..count..),4) * 100, '%')), stat = 'count', position = position_dodge(.1), size = 3) + 
  labs(title="Churn rate by internet service status")
p22

p23 <- ggplot(churn_clean, aes(x = tenure, fill = Churn)) + 
  geom_histogram(binwidth = 1) + 
  labs(x = "Months",
       title = "Churn rate by tenure")
p23

p24 <- ggplot(churn_clean, aes(x = TotalCharges, fill = Churn)) + 
  geom_histogram(binwidth = 100) + 
  labs(x = "Dollars (binwidth=100)",
       title = "Churn rate by tenure")
p24

