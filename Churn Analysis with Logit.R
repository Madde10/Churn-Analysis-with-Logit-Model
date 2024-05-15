#Mudasir Wazir
#Assignment 7



library(readxl)
d <- read_excel("C:/Users/mudas/OneDrive/Desktop/BAIS/SDM/New folder/TelcoChurn.xlsx")
View(d)
str(d)

colSums(is.na(d))
d <- d[complete.cases(d), ]    

table(d$Churn)   # Unbalanced sample
table(d$Churn, d$gender) #evenly distributed
table(d$Churn, d$SeniorCitizen) 
table(d$Churn,d$InternetService)
table(d$Churn,d$PhoneService)
table(d$PaperlessBilling,d$Churn)
table(d$Dependents,d$Churn)
table(d$Churn,d$PaymentMethod)
table(d$Churn,d$Partner)
table(d$Churn,d$DeviceProtection)
# other tables suggest churn rate is influenced by those variables.

# Convert character columns to factors
for(col in names(d)) {
  if(is.character(d[[col]])) {
    d[[col]] <- factor(d[[col]])
  }
}


d$Churn <- ifelse(d$Churn == "Yes", 1, 0)
d$SeniorCitizen = as.factor(d$SeniorCitizen)
str(d)

cor(d$MonthlyCharges, d$TotalCharges) 
cor(d$tenure, d$TotalCharges)



telephone_only <- subset(d, PhoneService == "Yes" & InternetService == "No")
internet_only <- subset(d, PhoneService == "No" & InternetService != "No")
both_services <- subset(d, PhoneService == "Yes" & InternetService != "No")

set.seed(1024)

# Function to split data into training and testing
split_data <- function(dataset, size = 0.75) {
  indices <- sample(1:nrow(dataset), round(size * nrow(dataset)))
  list(train = dataset[indices, ], test = dataset[-indices, ])
}

# Splitting each subset
phsplit <- split_data(telephone_only)
internetsplit <- split_data(internet_only)
bothsplit <- split_data(both_services)


# i) Modelling Subscribers of telephone services only group


dim(phsplit$train)
dim(phsplit$test)

model_phone <- glm(Churn ~ tenure + Contract + PaymentMethod + PaperlessBilling + 
                     MonthlyCharges + MultipleLines + SeniorCitizen + 
                     Partner + Dependents, family = binomial(link = "logit"), data = phsplit$train)
summary(model_phone)
library(DescTools)

VIF(model_phone) #Multicollinearity check : Remove Total charges and Multiple Lines

model_phone <- glm(Churn ~ tenure + Contract + PaymentMethod + PaperlessBilling + 
                     MonthlyCharges  + SeniorCitizen + 
                     Partner + Dependents, family = binomial(link = "logit"), data = phsplit$train)

summary(model_phone)
VIF(model_phone)


# ii) Modelling Subscribers of internet services only group

dim(internetsplit$train)
dim(internetsplit$test)


model_internet = glm(Churn ~ tenure + Contract + PaymentMethod + PaperlessBilling + 
                       MonthlyCharges + SeniorCitizen + 
                       Partner + Dependents + OnlineSecurity + OnlineBackup + DeviceProtection 
                     + TechSupport + StreamingTV + StreamingMovies , family = binomial(link = "logit"), data = internetsplit$train)
summary(model_internet)
VIF(model_internet) ##Multicollinearity check : Remove Total charges and Streaming Movies

model_internet = glm(Churn ~ tenure + Contract + PaymentMethod + PaperlessBilling + 
                       MonthlyCharges + SeniorCitizen + 
                       Partner + Dependents + OnlineSecurity + OnlineBackup + DeviceProtection 
                     + TechSupport , family = binomial(link = "logit"), data = internetsplit$train)

summary(model_internet)
VIF(model_internet)



# ii) Modelling Subscribers of both services group


dim(bothsplit$train)
dim(bothsplit$test)


model_both = glm(Churn ~ tenure + Contract + PaymentMethod + PaperlessBilling + 
                       MonthlyCharges + SeniorCitizen + InternetService +
                       Partner + Dependents + OnlineSecurity + OnlineBackup + DeviceProtection 
                     + TechSupport  , family = binomial(link = "logit"), data = bothsplit$train)

summary(model_both)
VIF(model_both) ##Multicollinearity check

library(stargazer)
stargazer(model_phone,model_internet,model_both, type = 'text', single.row = TRUE)

library(rcompanion)
nagelkerke(model_phone) #better than null, good fit
nagelkerke(model_internet)# better than null , good fit 
nagelkerke(model_both)# better than null , good fit

#However, need to calculate other metrics like Reacll, precision,f1 score and AUC. 




#Calculating the top three predictors for each group and thier marginal effects


sig_predictors_phone <- summary(model_phone)$coefficients[summary(model_phone)$coefficients[, "Pr(>|z|)"] < 0.05, ]
sig_predictors_internet <- summary(model_internet)$coefficients[summary(model_internet)$coefficients[, "Pr(>|z|)"] < 0.05, ]
sig_predictors_both <- summary(model_both)$coefficients[summary(model_both)$coefficients[, "Pr(>|z|)"] < 0.05, ]

# Calculate marginal effects for significant predictors only
LogitScalar_phone <- mean(dlogis(predict(model_phone, type = "link")))
marginal_effects_phone <- LogitScalar_phone * sig_predictors_phone[, "Estimate"]

LogitScalar_internet <- mean(dlogis(predict(model_internet, type = "link")))
marginal_effects_internet <- LogitScalar_internet * sig_predictors_internet[, "Estimate"]

LogitScalar_both <- mean(dlogis(predict(model_both, type = "link")))
marginal_effects_both <- LogitScalar_both * sig_predictors_both[, "Estimate"]


#Rank the predictors by the absolute value of their marginal effects
ranked_predictors_phone <- sort(abs(marginal_effects_phone), decreasing = TRUE)
ranked_predictors_internet <- sort(abs(marginal_effects_internet), decreasing = TRUE)
ranked_predictors_both <- sort(abs(marginal_effects_both), decreasing = TRUE)
# Select the top three predictors
top_three_predictors_phone <- head(ranked_predictors_phone, 4)
top_three_predictors_internet <- head(ranked_predictors_internet, 4)
top_three_predictors_both <- head(ranked_predictors_both, 4)

print(top_three_predictors_phone)
print(top_three_predictors_internet)
print(top_three_predictors_both) #includes intercept so we need to exclude this 




#Performance metrics
library(pROC)
library(caret)


#phone only

test_ph <- phsplit$test[ , c(3:6,16:19,21)]
pred_phone <-predict(model_phone, newdata=test_ph, type="response")
pred_phone <- ifelse(pred_phone>0.5, 1, 0)


cm_phone <- confusionMatrix(factor(pred_phone, levels = c(0, 1)), 
                            factor(test_ph$Churn, levels = c(0, 1)))
cm_phone
recall_phone <- cm_phone$byClass['Sensitivity']
recall_phone
precision_phone <- cm_phone$byClass['Pos Pred Value']
precision_phone 

f1_score_phone <- 2 * (precision_phone * recall_phone) / (precision_phone + recall_phone)
f1_score_phone
roc_phone <- roc(test_ph$Churn ~ pred_phone)
auc_phone <- auc(roc_phone)
auc_phone
plot(roc_phone, main = "ROC Curve for Phone-Only Customers")
print(paste("AUC for Phone-Only Customers:", round(auc_phone, 2)))



#internet 
test_int <- internetsplit$test[ , c(3:6,10:13,16:19,21)]
pred_int <-predict(model_internet, newdata=test_int, type="response")
pred_int <- ifelse(pred_int>0.5, 1, 0)

cm_int <- confusionMatrix(factor(pred_int, levels = c(0, 1)), 
                            factor(test_int$Churn, levels = c(0, 1)))
cm_int
recall_int <- cm_int$byClass['Sensitivity']
recall_int
precision_int <- cm_int$byClass['Pos Pred Value']
precision_int 

f1_score_int <- 2 * (precision_int * recall_int) / (precision_int + recall_int)
f1_score_int
roc_int <- roc(test_int$Churn ~ pred_int)
auc_int <- auc(roc_int)
auc_int
plot(roc_int, main = "ROC Curve for Internet-Only Customers")
print(paste("AUC for Internet-Only Customers:", round(auc_int, 2)))



#both services

test_both <- bothsplit$test[ , c(3:6,9:13,16:19,21)]
pred_both <-predict(model_both, newdata=test_both, type="response")
pred_both <- ifelse(pred_both>0.5, 1, 0)

cm_both <- confusionMatrix(factor(pred_both, levels = c(0, 1)), 
                          factor(test_both$Churn, levels = c(0, 1)))
cm_both
recall_both <- cm_both$byClass['Sensitivity']
recall_both
precision_both <- cm_both$byClass['Pos Pred Value']
precision_both 

f1_score_both <- 2 * (precision_both * recall_both) / (precision_both + recall_both)
f1_score_both
roc_both <- roc(test_both$Churn ~ pred_both)
auc_both <- auc(roc_both)
auc_both


plot(roc_both, main = "ROC Curve for Customers of both services ")
print(paste("AUC for Customers of both services:", round(auc_both, 2)))





performance_table <- data.frame(
  Group = c("Phone Only", "Internet Only", "Both Services"),
  Accuracy = c(cm_phone$overall['Accuracy'],cm_int$overall['Accuracy'],cm_both$overall['Accuracy']),
  Recall = c(recall_phone, recall_int, recall_both),
  Precision = c(precision_phone, precision_int, precision_both),
  F1_Score = c(f1_score_phone, f1_score_int, f1_score_both),
  AUC = c(auc_phone, auc_int, auc_both)
)



