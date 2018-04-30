#args <- commandArgs(TRUE)

install.packages("readr", repos='http://cran.us.r-project.org')
library(readr)
train <- read_csv("C:/Users/11200745/Desktop/Data_Hackathons/Big_Mart_Sales/BigMart_Train.csv")


train$Source = "train"
colnames(train)

total = train
nrow(train)
ncol(train)

nrow(total)
ncol(total)

sum(is.na(total$Item_Identifier))
sum(is.na(total$Item_Weight))
sum(is.na(total$Item_Fat_Content))
sum(is.na(total$Item_Visibility))
sum(is.na(total$Item_Type))
sum(is.na(total$Item_MRP))
sum(is.na(total$Outlet_Identifier))
sum(is.na(total$Outlet_Establishment_Year))
sum(is.na(total$Outlet_Size))
sum(is.na(total$Outlet_Location_Type))
sum(is.na(total$Outlet_Type))
sum(is.na(total$Item_Outlet_Sales))
sum(is.na(total$Source))

summary(total)
nrow(table(total$Item_Identifier))
nrow(table(total$Item_Weight))
nrow(table(total$Item_Fat_Content))
table(total$Item_Fat_Content)
nrow(table(total$Item_Visibility))
nrow(table(total$Item_Type))
table(total$Item_Type)
nrow(table(total$Item_MRP))
nrow(table(total$Outlet_Identifier))
nrow(table(total$Outlet_Establishment_Year))
nrow(table(total$Outlet_Size))
table(total$Outlet_Size)
nrow(table(total$Outlet_Location_Type))
table(total$Outlet_Location_Type)
nrow(table(total$Outlet_Type))
table(total$Outlet_Type)
nrow(table(total$Item_Outlet_Sales))
nrow(table(total$Source))
table(total$Source)

mean(total[["Item_Weight"]])

total$Item_Weight[is.na(total$Item_Weight)] <- mean(total$Item_Weight, na.rm=TRUE)
sum(is.na(total$Item_Weight))

table(total$Outlet_Size)
total$Outlet_Size[is.na(total$Outlet_Size)] <- "Medium"
sum(is.na(total$Outlet_Size))

total$Item_Visibility[total$Item_Visibility == 0] <- mean(total$Item_Visibility, na.rm=TRUE)

total$II = substr(total$Item_Identifier, 1,2)
table(total$II)
total$II[total$II == "DR"] <- "Drinks"
total$II[total$II == "FD"] <- "Food_Consumables"
total$II[total$II == "NC"] <- "Non_Consumables"
table(total$II)


total$Outlet_Years = 2017 - total$Outlet_Establishment_Year
summary(total$Outlet_Years)

table(total$Item_Fat_Content)
total$Item_Fat_Content[total$Item_Fat_Content == "LF"] = "Low Fat"
total$Item_Fat_Content[total$Item_Fat_Content == "low fat"] = "Low Fat"
total$Item_Fat_Content[total$Item_Fat_Content == "reg"] = "Regular"
total$Item_Fat_Content[total$Item_Fat_Content == "Low Fat" & total$II == "Non_Consumables"] = "Non_Edible"

table(total$Item_Fat_Content)


table(total$Outlet_Identifier)
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT010"]   <- 1
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT013"]   <- 2
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT017"]   <- 3
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT018"]   <- 4
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT019"]   <- 5
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT027"]   <- 6
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT035"]   <- 7
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT045"]   <- 8
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT046"]   <- 9
total$Outlet_Identifier[total$Outlet_Identifier  == "OUT049"]   <- 10

table(total$Item_Fat_Content)
total$Item_Fat_Content[total$Item_Fat_Content == "Low Fat"] <- "1"
total$Item_Fat_Content[total$Item_Fat_Content == "Non_Edible"] <- "2"
total$Item_Fat_Content[total$Item_Fat_Content == "Regular"] <- "3"

table(total$Outlet_Location_Type)
total$Outlet_Location_Type[total$Outlet_Location_Type == "Tier 1"] <- "1"
total$Outlet_Location_Type[total$Outlet_Location_Type == "Tier 2"] <- "2"
total$Outlet_Location_Type[total$Outlet_Location_Type == "Tier 3"] <- "3"

table(total$Outlet_Size)
total$Outlet_Size[total$Outlet_Size == "High"] <- "1"
total$Outlet_Size[total$Outlet_Size == "Medium"] <- "2"
total$Outlet_Size[total$Outlet_Size == "Small"] <- "3"

table(total$II)
total$II[total$II == "Drinks"] <- "1"
total$II[total$II == "Food_Consumables"] <- "2"
total$II[total$II == "Non_Consumables"] <- "3"

table(total$Outlet_Type)
total$Outlet_Type[total$Outlet_Type == "Grocery Store"] <- "1"
total$Outlet_Type[total$Outlet_Type == "Supermarket Type1"] <- "2"
total$Outlet_Type[total$Outlet_Type == "Supermarket Type2"] <- "3"
total$Outlet_Type[total$Outlet_Type == "Supermarket Type3"] <- "4"

table(total$Item_Type)


colnames(total)
total = total[c(-5,-8)]

colnames(total)

train_df = total[which(total$Source == "train"), ]
nrow(train_df)
ncol(train_df)
                         


colnames(train_df)
train_df <- train_df[-11]

#-------------------------------------------------------------------------------

#Mean Based
mean_sales = mean(train_df$Item_Outlet_Sales)
mean_sales  #2181.3
#---------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------

# summary(train_df$Item_Outlet_Sales)
# par(mfrow=c(2,2)) 
# hist(train_df$Item_Outlet_Sales)
#  
# hist(train_df$Item_Outlet_Sales,breaks=15,col="red",xlab="price",main="Colored histogram with 15 bins")
# 
#  
# hist(train_df$Item_Outlet_Sales,freq=FALSE,breaks=12,col="red", xlab="Sales",main="Histogram,rug_plot,densitycurve") 
# rug(jitter(train_df$Item_Outlet_Sales)) 
# lines(density(train_df$Item_Outlet_Sales), col="blue",lwd=2)
# 
# x <- train_df$Item_Outlet_Sales 
# h<-hist(x,breaks=12,col="red",xlab="Sales",main="Histogram with normal curve and box") 
# xfit<-seq(min(x), max(x), length=40)
# yfit<-dnorm(xfit, mean=mean(x), sd=sd(x)) 
# yfit <- yfit*diff(h$mids[1:2])*length(x) 
# lines(xfit, yfit, col="blue", lwd=2) 
# box()

#KERNEL DENSITY PLOTS
# plot(density(x)) 
#  
# par(mfrow=c(2,1)) 
# d <- density(train_df$Item_Outlet_Sales)       
# plot(d)                         
# d <- density(train_df$Item_Outlet_Sales)
# plot(d, main="Kernel Density of Price")
# polygon(d, col="red", border="blue")
# rug(CarSales_outlier$x, col="brown")
# 
# 
# #quantile plot of price
# 
# qqnorm(train_df$Item_Outlet_Sales)
# qqline (train_df$Item_Outlet_Sales, col=1)

#Anderson Darling test
install.packages("nortest", repos='http://cran.us.r-project.org')
library(nortest)
ad.test(train_df$Item_Outlet_Sales)
#data is non normal

#View(train_df)
colnames(train_df)
corr_df = train_df[c(-1,-3,-6,-7,-8,-9,-11)]
cordf<-cor(corr_df)
# corrplot::corrplot(cordf)

#dropping x: Item_Fat_Content has to be dropped as no significant relation between groups
train_df$Item_Fat_Content <- as.factor(train_df$Item_Fat_Content)
train_df$Outlet_Identifier <- as.factor(train_df$Outlet_Identifier)
train_df$Outlet_Size <- as.factor(train_df$Outlet_Size)
train_df$Outlet_Location_Type <- as.factor(train_df$Outlet_Location_Type)
train_df$Outlet_Type <-as.factor(train_df$Outlet_Type)
train_df$II<-as.factor(train_df$II)


kruskal.test(Item_Outlet_Sales ~ Item_Fat_Content, data= train_df)
kruskal.test(Item_Outlet_Sales ~ Outlet_Identifier, data= train_df)
kruskal.test(Item_Outlet_Sales ~ Outlet_Size, data= train_df)
kruskal.test(Item_Outlet_Sales ~ Outlet_Location_Type, data= train_df)
kruskal.test(Item_Outlet_Sales ~ Outlet_Type, data= train_df)
kruskal.test(Item_Outlet_Sales ~ II, data= train_df)

colnames(train_df)

train_df <-train_df[-3]

#---------------------------------------

xt <-train_df[1:5000,c(-1)]
 
xt1 <- train_df[5001:8523,c(-1)]


#-----------------------------------------------------------------------------------------

#10 fold cross validation with decision trees using rpart
install.packages("caret", repos='http://cran.us.r-project.org')
library(caret)
install.packages("rpart", repos='http://cran.us.r-project.org')
library(rpart)

# xt <-train_df[1:5000,c(-1)]
# 
# xt1 <- train_df[5001:8523,c(-1)]
# 


# define training control
train_control<- trainControl(method="cv", number=10,  savePredictions = TRUE)

# train the model 
model<- train(Item_Outlet_Sales~., data=xt, trControl=train_control, method="rpart")

# make predictions
predictions_dtree<- predict(model,xt1)

# append predictions
cv_dtree_test_df<- cbind(xt1,predictions_dtree)
#View(cv_dtree_test_df)

print(model)
summary(model)
#plot(model)

#write.csv(x = cv_dtree_test_df, file = args[2])


#--------------------------------------------------------------------------------

#Linear Regression Model

# xt <-train_df[1:5000,c(-1)]
#  
# xt1 <- train_df[5001:8523,c(-1)]




# Train the model using the training sets and check score
linear <- lm(xt$Item_Outlet_Sales ~ ., data=xt)
summary(linear)
#Predict Output
predicted_lr= predict(linear,xt1)
lr_test_df<- cbind(xt1,predicted_lr)
#View(lr_test_df)
#Rsquared 56.3%

#write.csv(x = lr_test_df, file = args[2])

#---------------------------------------------------
# #box cox
# xt <-train_df[1:5000,c(-1)]
# 
# xt1 <- train_df[5001:8523,c(-1)]
# 
# xt2<-test_df

install.packages("MASS", repos='http://cran.us.r-project.org')
library(MASS)


#boxcox(linear)
#refine boxcox
#boxcox(linear, lambda = seq(0, 0.1, 0.2))



# par(mfrow=c(2,1)) 
y<-log(xt$Item_Outlet_Sales)
# hist(y)
# 
# qqnorm(y)
# qqline (y, col=1)
 

#adding log data


linear_train<-cbind(xt, y)
colnames(linear_train)
linear_train <-linear_train[-9]

#Linear Regression post boxcox
linear_boxcox<-lm(y~., linear_train)                            
summary(linear_boxcox)


#Residuals plot
#plot(linear_boxcox$residuals)

pred1<-predict(linear_boxcox,xt1)
#plot(pred1)
#plot(linear_train$y,pred1)
sales_predicted_lrbc<-exp(pred1)
lr_bc_test_df<-cbind(xt1,pred1, sales_predicted_lrbc)
#View(lr_bc_test_df)
nrow(lr_bc_test_df)

#write.csv(x = lr_bc_test_df, file = args[2])


#-------------------------------------------------------------------------------------


install.packages("tidyverse", repos='http://cran.us.r-project.org')
install.packages("broom", repos='http://cran.us.r-project.org')
install.packages("glmnet", repos='http://cran.us.r-project.org')
install.packages("DAAG", repos='http://cran.us.r-project.org')

library(tidyverse)
library(broom)
library(glmnet)
library(DAAG)


#----------------------------------------------------------------------


#Elasticnet

nrow(train_df)
yt<-train_df[1:5000, ]
y<-yt$Item_Outlet_Sales
xt <-train_df[1:5000,c(-1,-9)]
x<-data.matrix(xt)       
yt1<-train_df[5001:8523, ]
y1<-yt1$Item_Outlet_Sales
xt1 <- train_df[5001:8523,c(-1,-9)]
x1<-data.matrix(xt1)

lambdas <- 10^seq(3, -2, by = -.1)

fit.lasso <- glmnet(x, y, family="gaussian", alpha=1)
fit.ridge <- glmnet(x, y, family="gaussian", alpha=0)
fit.elnet <- glmnet(x, y, family="gaussian", alpha=.5)

fit.lasso.cv <- cv.glmnet(x, y, type.measure="mse", alpha=1, 
                          family="gaussian")
fit.ridge.cv <- cv.glmnet(x, y, type.measure="mse", alpha=0,
                          family="gaussian")
fit.elnet.cv <- cv.glmnet(x, y, type.measure="mse", alpha=.5,
                          family="gaussian")

for (i in 0:10) {
  assign(paste("fit", i, sep=""), cv.glmnet(x, y, type.measure="mse", 
                                            alpha=i/10,family="gaussian"))
}

# par(mfrow=c(3,2))
# # For plotting options, type '?plot.glmnet' in R console
# plot(fit.lasso, xvar="lambda")
# plot(fit10, main="LASSO")
# 
# plot(fit.ridge, xvar="lambda")
# plot(fit0, main="Ridge")
# 
# plot(fit.elnet, xvar="lambda")
# plot(fit5, main="Elastic Net")

yhat0 <- predict(fit0, s=fit0$lambda.min, newx=x1)
yhat1 <- predict(fit1, s=fit1$lambda.min, newx=x1)
yhat2 <- predict(fit2, s=fit2$lambda.min, newx=x1)
yhat3 <- predict(fit3, s=fit3$lambda.min, newx=x1)
yhat4 <- predict(fit4, s=fit4$lambda.min, newx=x1)
yhat5 <- predict(fit5, s=fit5$lambda.min, newx=x1)
yhat6 <- predict(fit6, s=fit6$lambda.min, newx=x1)
yhat7 <- predict(fit7, s=fit7$lambda.min, newx=x1)
yhat8 <- predict(fit8, s=fit8$lambda.min, newx=x1)
yhat9 <- predict(fit9, s=fit9$lambda.min, newx=x1)
yhat10 <- predict(fit10, s=fit10$lambda.min, newx=x1)

mse0 <- mean((y1 - yhat0)^2)
mse1 <- mean((y1 - yhat1)^2)
mse2 <- mean((y1 - yhat2)^2)
mse3 <- mean((y1 - yhat3)^2)
mse4 <- mean((y1 - yhat4)^2)
mse5 <- mean((y1 - yhat5)^2)
mse6 <- mean((y1 - yhat6)^2)
mse7 <- mean((y1 - yhat7)^2)
mse8 <- mean((y1 - yhat8)^2)
mse9 <- mean((y1 - yhat9)^2)
mse10 <- mean((y1 - yhat10)^2)


mse0
mse1
mse2
mse3
mse4
mse5
mse6
mse7
mse8
mse9
mse10

rmse3=sqrt(mse3)
rmse3

elnet_yhat<-yhat3
elnet_test_df<-cbind(x1, yhat3)
#View(elnet_test_df)
#write.csv(x = elnet_test_df, file = args[2])

coef(fit.elnet.cv)

#----------------------------------------------------
fit.lasso <- glmnet(x, y, family="gaussian", alpha=1)

fit.lasso.cv <- cv.glmnet(x, y, type.measure="mse", alpha=1, 
                          family="gaussian")

# par(mfrow=c(2,1))
# # For plotting options, type '?plot.glmnet' in R console
# plot(fit.lasso, xvar="lambda")
# plot(fit.lasso.cv, main="LASSO")

yhat_lasso <- predict(fit.lasso.cv, s=fit.lasso.cv$lambda.min, newx=x1)


mse_lasso <- mean((y1 - yhat_lasso)^2)

rmse_lasso=sqrt(mse_lasso)
rmse_lasso

lasso_test_df<-cbind(x1, yhat_lasso)
#View(lasso_test_df)
coef(fit.lasso.cv)

#write.csv(x = lasso_test_df, file = args[2])

#----------------------------------------------------
#ridge


fit.ridge <- glmnet(x, y, family="gaussian", alpha=0)



fit.ridge.cv <- cv.glmnet(x, y, type.measure="mse", alpha=0,
                          family="gaussian")




# par(mfrow=c(2,1))
# # For plotting options, type '?plot.glmnet' in R console
# 
# 
# plot(fit.ridge, xvar="lambda")
# plot(fit.ridge.cv, main="Ridge")
# 


yhat_ridge <- predict(fit.ridge.cv, s=fit.ridge.cv$lambda.min, newx=x1)


mse_ridge <- mean((y1 - yhat_ridge)^2)


rmse_ridge=sqrt(mse_ridge)
rmse_ridge

yhat_ridge
ridge_test_df<-cbind(x1, yhat_ridge)
#View(ridge_test_df)


coef(fit.ridge.cv)
#write.csv(x = ridge_test_df, file = args[2])

#-----------------------------------------------------

#RandomForest
# xt <-train_df[1:5000,c(-1)]
# 
# xt1 <- train_df[5001:8523,c(-1)]



install.packages("randomForest", repos='http://cran.us.r-project.org')
library(randomForest)
fit <- randomForest(xt$Item_Outlet_Sales~. , data=xt)
print(fit) 
importance(fit) 

# make predictions
predictions_rf<- predict(fit,xt1)

# append predictions
rf_test_df<- cbind(xt1,predictions_rf)
#View(rf_test_df)

#write.csv(x = rf_test_df, file = args[2])

#----------------------------------------------------------------------------------

#XGBoost
# xt <-train_df[1:5000,c(-1)]
# 
# xt1 <- train_df[5001:8523,c(-1)]
colnames(xt)
colnames(xt1)

install.packages("xgboost", repos='http://cran.us.r-project.org')
library(xgboost)

train <- xt
test <- xt1

dim(train[-8])
dim(test[-8])

traindata<-data.matrix(train[-8])
class(traindata)
trainlabel <-as.numeric(train$Item_Outlet_Sales)
class(trainlabel)

testdata<-data.matrix(test[-8])
class(testdata)
testlabel <-as.numeric(test$Item_Outlet_Sales)
class(testlabel)



dtrain <- xgb.DMatrix(data = traindata, label=trainlabel)
dtest <- xgb.DMatrix(data = testdata, label=testlabel)

watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, booster = "gblinear", max_depth=2, nthread = 2, nrounds=2, watchlist=watchlist, eval_metric = "rmse", eval_metric = "error",objective = "reg:linear")

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
#xgb.plot.importance(importance_matrix = importance_matrix)

pred_xgboost <- predict(bst, dtest)
xgboost_test_df<- cbind(testdata, testlabel, pred_xgboost)
#View(xgboost_test_df)

#write.csv(x = xgboost_test_df, file = args[2])

#----------------------------------------------------------------------------------------

total_pred <- cbind(train_df[5001:8523, ], predictions_dtree, predicted_lr,sales_predicted_lrbc, elnet_yhat, yhat_lasso, yhat_ridge, predictions_rf, pred_xgboost)
write.csv(x = total_pred, file = args[2])

