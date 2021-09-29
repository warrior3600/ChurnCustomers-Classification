getwd()
setwd("C:/Users/ruben/Documents/Practicals/R-Practicals/R_practice/Logistic_Regression")
#Load the libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(dplyr)

#Load the datasets
churn_data <- read.csv("churn_data.csv")
cust_data <- read.csv("customer_data.csv")
internet_data <- read.csv("internet_data.csv")

#Datasets validations
dim(churn_data)
dim(cust_data)
dim(internet_data)
#Check if duplicate ids are present
length(unique(churn_data$customerID)) == length(churn_data$customerID)
length(unique(cust_data$customerID)) == length(cust_data$customerID)
length(unique(internet_data$customerID)) == length(internet_data$customerID)
#counts <- summarise(group_by(churn_data,customerID),table(customerID))
#View(churn_data[counts$`table(customerID)`!=1]) ##alt method
#Now we check if the customer ids are present in all the datsets
setdiff(churn_data$customerID,cust_data$customerID)
setdiff(cust_data$customerID,internet_data$customerID)
setdiff(internet_data$customerID,churn_data$customerID)

#Merge the data
telecom <- merge(cust_data,internet_data, by = "customerID",all = F)
telecom <- merge(telecom,churn_data, by = "customerID",all = F)
str(telecom)

##Exploratory Data Analysis
#Barcharts for categorical variables with stacked telecom information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none") #Assigning a variable for theme customization of the bar chart
plot_grid(ggplot(telecom,aes(x=PhoneService,fill=Churn)) + geom_bar(),
          ggplot(telecom,aes(x=gender,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=factor(SeniorCitizen),fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=Partner,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=MultipleLines,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=InternetService,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=OnlineSecurity,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=OnlineBackup,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=DeviceProtection,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=TechSupport,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=StreamingTV,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=StreamingMovies,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=Contract,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=PaperlessBilling,fill=Churn)) + geom_bar()+bar_theme1,
          ggplot(telecom,aes(x=PaymentMethod,fill=Churn)) + geom_bar()+
            bar_theme1, align = "h")

# Histogram and Boxplots for numeric variables in single plot . 
#plot_grid(ggplot(telecom,aes(x=MonthlyCharges,fill = Churn)) + geom_density(alpha = 0.5) + bar_theme1,
#          ggplot(telecom,aes(x=TotalCharges,fill = Churn)) + geom_density(alpha = 0.5) + bar_theme1,
#          ggplot(telecom,aes(x=MonthlyCharges)) + geom_boxplot() + bar_theme1,
#          ggplot(telecom,aes(x=TotalCharges)) + geom_boxplot() + bar_theme1)
plot_grid(ggplot(telecom,aes(x=tenure,fill = Churn)) + geom_histogram(alpha = 0.5) + bar_theme1,
          ggplot(telecom,aes(x=MonthlyCharges,fill=Churn)) + geom_histogram(alpha = 0.5) + bar_theme1,
          ggplot(telecom,aes(x=TotalCharges,fill=Churn)) + geom_histogram(alpha = 0.5) + bar_theme1,
          ggplot(telecom,aes(x=tenure)) + geom_boxplot() + bar_theme1,
          ggplot(telecom,aes(x=MonthlyCharges)) + geom_boxplot() + bar_theme1,
          ggplot(telecom,aes(x=TotalCharges)) + geom_boxplot() + bar_theme1)

#Correlation between all the numeric variables
ggpairs(telecom[,c("tenure","MonthlyCharges","TotalCharges")])

##Data Preperation
#Bring the variables in the right format
telecom$SeniorCitizen <- ifelse(telecom$SeniorCitizen == 1,"Yes","No")

#Outlier treatment and imputing missing values
sapply(telecom[,c("tenure","MonthlyCharges","TotalCharges")],
       function(x) quantile(x,seq(0,1,0.01),na.rm = T))

#Missing Values
sapply(telecom, function(x) sum(is.na(x)))
View(subset(telecom,is.na(TotalCharges)))
telecom <- telecom[!is.na(telecom$TotalCharges),]

##Feature Standardization
#Converting target into a binary 1 and 0 form for use in modelling
telecom$Churn <- ifelse(telecom$Churn == "Yes",1,0)

#Creating dummy variables by creating a seperate dataframe
set.seed(123)
telecom_chr<- telecom[,-c(1,14,19,20,21)]#Extracting categorical variable
telecom_fact<- data.frame(sapply(telecom_chr, function(x) factor(x)))#Converting tio the factor
dummies <- data.frame(sapply(telecom_fact, function(x) data.frame(model.matrix(~x,data = telecom_fact))[,-1]))#Creating dummies for each categorical variable
telecom_final <- cbind(telecom[,c(21,14,19,20)],dummies)
View(telecom_final)#Final dataset prepared fro modelling

##Data Modelling
#Create the train and test datasets
indices <- sample.split(telecom_final,SplitRatio = 0.7)
train <- telecom_final[indices,]
test <- telecom_final[!(indices),]

#Create the logistic regression model
model<-glm(Churn~.,data = train,family = "binomial")
summary(model)

#Test the data with the predicted model
test$prediction <- predict(model,type = "response",newdata = test)
View(test)

##Data Accuracy and Model Evaluation
#Lets use probability cutoff of 50%
test$pred_churn <- factor(ifelse(test$prediction>=0.5,"Yes","No"))#segmenting the values into Yes and No based on cutoff
test$actual_churn <- factor(ifelse(test$Churn==1,"Yes","No"))#Converting the churn back to yes and no 
test$pred_churn
test$actual_churn
table(test$actual_churn,test$pred_churn)

test_conf <-confusionMatrix(test$pred_churn,test$actual_churn,positive = "Yes")
test_conf

#Improve the balance between specificity and sensitivity with changing prob cutoff to 40%
test$pred_churn <- factor(ifelse(test$prediction >= 0.40, "Yes", "No"))
table(test$actual_churn,test$pred_churn)
test_conf <- confusionMatrix(test$pred_churn, test$actual_churn, positive = "Yes")
test_conf

test_conf$overall

#Compute the optimum value of p
optimum <- function(cutoff)#Creating a function to compute the confusion matrix for a general value of cutoff
{
  prediction_churn <- factor(ifelse(test$prediction>=cutoff,"Yes","No"))
  
  conf_mat <- confusionMatrix(prediction_churn,test$actual_churn, positive = "Yes")
  acc<-conf_mat$overall[1]
  sens<-conf_mat$byClass[1]
  spec<-conf_mat$byClass[2]
  out<-t(as.matrix(c(sens,spec)))
  colnames(out) <- c("sensitivity","specificity")
  return(out)
}
s <- seq(0.01,0.8,length = 100)#Creating a sequence of 100 cutoffs from 1% to 80%
s
OUT<- matrix(0,100,2)#Creating an zero matrix of length 100
OUT
#Computing the matrix
for (i in 1:100)
{
  OUT[i,] <- optimum(s[i]) 
}
OUT 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff
#Choosing the optimum value of 0.3052525
test$pred_churn <- factor(ifelse(test$prediction>=cutoff,"Yes","No"))
conf_final <- confusionMatrix(test$pred_churn,test$actual_churn,positive = "Yes")
conf_final


