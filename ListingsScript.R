#Listings

#Clear memory
rm(list=ls())

setwd("/Users/adambartlett/Documents/AdamWork/CapstoneData/")
#import data
dataAirbnb <- read.csv("listings.csv",header = TRUE, sep = ",")
#list the names of the variables to be removed
names(dataAirbnb[c(1,2:25,30:33,35,37:39,41:45,47,48,60,62:65,69:76,78:79,88:89,92:95)])
dataAirbnb <- subset(dataAirbnb, select = -c(1,2:25,30:33,35,37:39,41:45,47,48,60,62:65,69:76,78:79,88:89,92:95))
#remove missing data
dataAirbnb <- na.omit(dataAirbnb)
dataAirbnb <- dataAirbnb[dataAirbnb$host_response_time != "N/A", ]

attach(dataAirbnb)

#get amenities in a readable list format
newAM <- as.character(amenities)
newAM <- strsplit(newAM,",")
newAM <- gsub( "[^[:alnum:],]", "", newAM)
#remove first character from first element of each element in list
i = 1
while (i<=length(newAM)){
  newAM[[i]][1] <- substr(newAM[[i]][1],2,1000000)
  i = i+1
}
newAM <- strsplit(newAM,",")

#create amenities count vector
amenities_count <- vector()
for (i in 1:length(newAM)){
  amenities_count[i] <- length(newAM[[i]])
}

dataAirbnb <- cbind(dataAirbnb,amenities_count) #Add amenities counts

#change string NA to real NA and remove NA
na <- function(vector) {
  vector <- sub("N/A",NA,vector)
}
dataAirbnb[,2] <- na(dataAirbnb[,2])
dataAirbnb[,3] <- na(dataAirbnb[,3])
dataAirbnb <- na.omit(dataAirbnb)

#change string percentage variables to numeric variables
fn <- function(vector) {
  vector <- sub("%","",vector)
  vector <- as.numeric(vector)
  vector <- vector/100
}
dataAirbnb[,2] <- fn(dataAirbnb[,2])
dataAirbnb[,3] <- fn(dataAirbnb[,3])

#change T F into 1 and 0
gn <- function(vector) {
  vector <- sub("t",1,vector)
  vector <- sub("f",0,vector)
}
dataAirbnb[,4] <- gn(dataAirbnb[,4])
dataAirbnb[,6] <- gn(dataAirbnb[,6])
dataAirbnb[,11] <- gn(dataAirbnb[,11])
dataAirbnb[,32] <- gn(dataAirbnb[,32])
dataAirbnb[,33] <- gn(dataAirbnb[,33])

#remove "$" and convert to numeric
dn <- function(vector){
  vector <- as.character(vector)
  vector <- substr(vector,2,1000000)
  vector <- gsub(",","",vector)
  vector <- as.numeric(vector)
}  
dataAirbnb$price <- dn(dataAirbnb$price)
dataAirbnb$extra_people <- dn(dataAirbnb$extra_people)
attach(dataAirbnb)

#create plots for Price
#par(mfrow = c(1,2))
#boxplot(price)
#title(main = "Boxplot of Price",sub="Figure 1", ylab = "Price($)",ylim = c(0,2000))
#hist(price, main = "Histogram of Price", sub="Figure 2", xlab = "Price($)", ylab = "Frequency", xlim = c(0,2000), ylim = c(0,6000))

#Check correlaiton among numeric predictors
numPred <- dataAirbnb[c(2,3,5,9,10,14,15,16,17,21,22,23,24,25,26,27,28,29,30,31,35)]
corNumPred <- cor(numPred) #accomodations, bedrooms, beds are correlated. The review predictors have high correlation

#Create matrix of factor predictors
X <- as.matrix(cbind(factor(dataAirbnb[,1],levels=c("a few days or more","within a day","within a few hours","within an hour"),ordered=TRUE),dataAirbnb[,4],dataAirbnb[,6],factor(dataAirbnb[,7]),factor(dataAirbnb[,8]),dataAirbnb[,11],factor(dataAirbnb[,12]),factor(dataAirbnb[,13],levels=c("Shared room","Private room","Entire home/apt"),ordered=TRUE),factor(dataAirbnb[,18],levels=c("Couch","Airbed","Futon","Pull-out Sofa","Real Bed"),ordered=TRUE),dataAirbnb[,33],factor(dataAirbnb[,34],levels=c("strict","moderate","flexible"),ordered=TRUE)))
#drop requires_license as all are no "0"

#Create matrix of all predictors, not including amenities
matPred <- as.data.frame(cbind(numPred,X))

library(MASS)
'/ Comments out regular price model
#Full model
modFull <- lm(price ~.,data = matPred)
#Empty model
modNull <- lm(price ~ 1,data = matPred)
#Forward Regression
stepF <- stepAIC(modNull,scope=list(lower=modNull, upper=modFull),
                 direction= "forward", trace=TRUE)
summary(stepF)

#Backward Regression
stepB <- stepAIC(modFull, direction= "backward", trace=TRUE)
summary(stepB)


#Transform price to Log, price was skewed log transform will help to linearize
#Full model
modLogFull <- lm(log(price) ~.,data = matPred)
#Empty model
modLogNull <- lm(log(price) ~ 1,data = matPred)
#Forward Regression
stepLogF <- stepAIC(modLogNull,scope=list(lower=modLogNull, upper=modLogFull),
                 direction= "forward", trace=TRUE)
summary(stepLogF)
anova(stepLogF)
par(mfrow=c(2,2))
plot(stepLogF)


#Backward Regression
stepLogB <- stepAIC(modLogFull, direction= "backward", trace=TRUE)
summary(stepLogB)
anova(stepLogB)
par(mfrow=c(2,2))
plot(stepLogB)
#Forward and Backward models give the same predictors
#Outliter detected in both models. Determine if it is a leverage point

#Check leverage points forward model
levF = hat(model.matrix(stepLogF))
plot(levF)
matPred[levF>0.8,]
f <- matPred[levF>0.8,]

#Check leverage points backward model
levB = hat(model.matrix(stepLogB))
plot(levB)
matPred[levB>0.8,]
b <- matPred[levB>0.8,]

plot(accommodates,cooks.distance(stepLogF))
#identify(accommodates,cooks.distance(stepLogF))
#identify(dataAirbnb$accommodates,cooks.distance(stepLogF)) #this will allow you to find the leverage point

plot(accommodates,cooks.distance(stepLogB))
#identify(accommodates,cooks.distance(stepLogB))


library(car) #needed to run vif()
vif(stepLogF)
vif(stepLogB)

#removed review_scores_rating collinear with other rating variables
/'
#Remove highest and lowest vaues
dataAirbnb <- dataAirbnb[-(which(dataAirbnb$price==max(dataAirbnb$price))),]
dataAirbnb <- dataAirbnb[-(which(dataAirbnb$price==min(dataAirbnb$price))),]

#Mean price and log(price)
mean(dataAirbnb$price)
sd(dataAirbnb$price)
mean(log(dataAirbnb$price))
sd(log(dataAirbnb$price))

#Histogram of city price after minimum and maximum values have been removed
par(mfrow=c(1,2))

hist(dataAirbnb$price, breaks=50, main = "Toronto Airbnb Listing Prices", sub="Figure 1", xlab = "Price ($) Mean: $114.93", ylab = "Frequency",xlim = c(0,1500),ylim=c(0,2500))
hist(log(dataAirbnb$price), breaks=50, main = "Toronto Airbnb Listing Log(Prices)", sub="Figure 2", xlab = "Log(Price) Mean: 4.55", ylab = "Frequency",xlim=c(2.5,8),ylim=c(0,550))




#Drop insignificant predictors
attach(dataAirbnb)
#Factors Included: host_is_superhost, property_type, room_type, intstant_bookable
newAirbnb <- as.data.frame(cbind(log(price),accommodates,bathrooms,bedrooms,latitude,review_scores_value,number_of_reviews,extra_people,review_scores_cleanliness,host_acceptance_rate,factor(host_is_superhost),amenities_count,review_scores_location,factor(dataAirbnb[,12]),factor(dataAirbnb[,13],levels=c("Shared room","Private room","Entire home/apt"),ordered=TRUE),factor(instant_bookable)))

#Train Test model
#split model into training and testing sets
rn_train <- sample(nrow(newAirbnb),floor(nrow(newAirbnb)*0.8))
train <- newAirbnb[rn_train,]
test <- newAirbnb[-rn_train,]
mod1 <- lm(train[,1] ~ ., data = train[,-1])

#Prediction
predict.mod1 <- predict(mod1,interval="prediction",newdata=test)

#Errors
par(mfrow=c(1,1))
errors1 <- predict.mod1[,"fit"] - test$V1
hist(errors1)

#MSRE mod1
sqrt(sum((predict.mod1[,"fit"]-test$V1)^2)/nrow(test))

#PRED mod1
rel_change1 <- 1 - ((test$V1 - abs(errors1)) / test$V1)
pred1_25 <- table(rel_change1<0.25)["TRUE"] / nrow(test)
paste("PRED(25):", pred1_25)

#Residuals
res.mod1 <- resid(mod1)

#Standardized Residuals 
stres.mod1 <- rstandard(mod1)

#Fitted Values Training Set
fit.mod1 <- fitted(mod1)

#Standardized Residual Plots
par(mfrow=c(3,3))
plot(train[,2],stres.mod1)

#Plot standardized residuals vs explanatory variables
#Standardized residuals vs number_of_reviews is not a random distribution

#Take the log of number_of_reviews
newAirbnb2 <- as.data.frame(cbind(log(price),accommodates,bathrooms,bedrooms,latitude,review_scores_value,log(number_of_reviews),extra_people,review_scores_cleanliness,host_acceptance_rate,factor(host_is_superhost),amenities_count,review_scores_location,factor(dataAirbnb[,12]),factor(dataAirbnb[,13],levels=c("Shared room","Private room","Entire home/apt"),ordered=TRUE),factor(instant_bookable)))
rn_train2 <- sample(nrow(newAirbnb2),floor(nrow(newAirbnb2)*0.8))
train2 <- newAirbnb2[rn_train2,]
test2 <- newAirbnb2[-rn_train2,]
mod2 <- lm(train2[,1] ~ ., data = train2[,-1])
summary(mod2)

#Prediction2
predict.mod2 <- predict(mod2,interval="prediction",newdata=test2)

#Errors
par(mfrow=c(1,1))
errors2 <- predict.mod2[,"fit"] - test2$V1
hist(errors2)

#MSRE mod2
sqrt(sum((predict.mod2[,"fit"]-test2$V1)^2)/nrow(test2))
#MSRE very similar

#PRED mod2
rel_change2 <- 1 - ((test2$V1 - abs(errors2)) / test2$V1)
pred2_25 <- table(rel_change2<0.25)["TRUE"] / nrow(test2)
paste("PRED(25):", pred2_25)

#Check MSRE mod2 (outliers out)
#train_remove <- train2[-c(841,1177,4201,4255,4891,4925,6045),]
#mod_remove <- lm(train_remove[,1] ~ ., data = train_remove[,-1])
#predict.mod_remove <- predict(mod_remove,interval="prediction",newdata=test2)
#sqrt(sum((predict.mod_remove[,"fit"]-test2$V1)^2)/nrow(test2))
#RMSE mod2 0.3468, outliers were not leverage points. No impact on model

attach(dataAirbnb)

#6-fold cross validation
a <- log(dataAirbnb$price)
b <- as.data.frame(cbind(dataAirbnb$accommodates,dataAirbnb$bathrooms,dataAirbnb$bedrooms,dataAirbnb$latitude,dataAirbnb$review_scores_value,log(dataAirbnb$number_of_reviews),dataAirbnb$extra_people,dataAirbnb$review_scores_cleanliness,dataAirbnb$host_acceptance_rate,factor(dataAirbnb$host_is_superhost),dataAirbnb$amenities_count,dataAirbnb$review_scores_location,factor(dataAirbnb[,12]),factor(dataAirbnb[,13],levels=c("Shared room","Private room","Entire home/apt"),ordered=TRUE),factor(dataAirbnb$instant_bookable)))
data <- as.data.frame(cbind(a,b))

n_train <- nrow(dataAirbnb)
n_folds <- 6
mod_coef <- c()
mod_msre <- c()
folds_i <- sample(rep(1:n_folds, length.out = n_train))
for (k in 1:n_folds) {
  test_i <- which(folds_i == k)
  trainxy <- data[-test_i, ]
  testxy <- data[test_i, ]
  x <- as.data.frame(trainxy[,-1])
  y <- as.vector(trainxy[,1])
  mod_i <- lm(y ~ ., data = x)
  x <- as.data.frame(testxy[,-1])
  y <- as.vector(testxy[,1])
  fit_i <- predict(mod_i,interval="prediction",newdata=x)
  coef_i <- coef(mod_i)
  errors_i <- fit_i[,"fit"] - y
  msre_i <- sqrt(sum((errors_i^2)/nrow(x)))
  mod_coef <- rbind(mod_coef,coef_i)
  mod_msre <- rbind(mod_msre,msre_i)
}

modelCoefs <- colMeans(mod_coef)
modelMSRE <- mean(mod_msre)

#Testing subsets of Toronto data with cross validated model
#datasample <- sample(nrow(data),floor(nrow(newAirbnb2)*0.7))
#test <- data[datasample,]

mat_data <- cbind(rep(1,nrow(data)),data[,-1])
predPrice <- as.matrix(mat_data) %*% as.vector(modelCoefs)

errors_mod <- predPrice - data[,1]
#MSRE averaged model
sqrt(sum((errors_mod)^2)/nrow(data))

#PRED25 full model
rel_change <- 1 - ((data[,1] - abs(errors_mod)) / data[,1])
pred_25 <- table(rel_change<0.25)["TRUE"] / nrow(data)
paste("PRED(25):", pred_25)

#PRED10 full model
rel_change <- 1 - ((data[,1] - abs(errors_mod)) / data[,1])
pred_10 <- table(rel_change<0.10)["TRUE"] / nrow(data)
paste("PRED(10):", pred_10)

save(modelCoefs, file = "modelCoefficients.rda")

#standardized residuals
st_errors_mod <- errors_mod/sd(errors_mod)

par(mfrow=c(1,2))
plot(predPrice,st_errors_mod, main = "Standardized Residuals v Fitted log(Price)",sub="Figure 3",xlab = "Fitted log(Price)", ylab = "Standardized Residuals",ylim = c(-5,5))
hist(errors_mod, breaks = 50, main = "Histogram of Residuals", sub="Figure 4", xlab = "Residuals", ylab = "Frequency", xlim = c(-2.5,2.5), ylim = c(0,850))

#Clear memory
rm(list=ls())
#Test model on different cities data

#Set path
setwd("/Users/adambartlett/Documents/AdamWork/CapstoneData/")

#Load modelCoefficients
load("modelCoefficients.rda")

#Load Montreal data
#dataAirbnb_city <- read.csv("listings-montreal.csv",header = TRUE, sep = ",")
#Load Vancouver data
dataAirbnb_city <- read.csv("listings-vancouver.csv",header = TRUE, sep = ",")

attach(dataAirbnb_city)


#Factors Included: host_is_superhost, property_type, room_type, intstant_bookable
dataAirbnb_city <- subset(dataAirbnb_city, select = c(price,accommodates,bathrooms,bedrooms,latitude,review_scores_value,number_of_reviews,extra_people,review_scores_cleanliness,host_acceptance_rate,host_is_superhost,amenities,review_scores_location,property_type,room_type,instant_bookable))

#get amenities in a readable list format
newAM <- as.character(amenities)
newAM <- strsplit(newAM,",")
newAM <- gsub( "[^[:alnum:],]", "", newAM)
#remove first character from first element of each element in list
i = 1
while (i<=length(newAM)){
  newAM[[i]][1] <- substr(newAM[[i]][1],2,1000000)
  i = i+1
}
newAM <- strsplit(newAM,",")

#create amenities count vector
amenities_count <- vector()
for (i in 1:length(newAM)){
  amenities_count[i] <- length(newAM[[i]])
}

#change string NA values to real NA values
na <- function(vector) {
  vector <- sub("N/A",NA,vector)
}
dataAirbnb_city$host_acceptance_rate <- na(dataAirbnb_city$host_acceptance_rate)

#change string percentage variables to numeric variables
fn <- function(vector) {
  vector <- sub("%","",vector)
  vector <- as.numeric(vector)
  vector <- vector/100
}
dataAirbnb_city$host_acceptance_rate <- fn(dataAirbnb_city$host_acceptance_rate)

#change T and F into 1 and 0
gn <- function(vector) {
  vector <- sub("t",1,vector)
  vector <- sub("f",0,vector)
}
dataAirbnb_city$host_is_superhost <- gn(dataAirbnb_city$host_is_superhost)
dataAirbnb_city$instant_bookable <- gn(dataAirbnb_city$instant_bookable)

#remove "$" and convert to numeric
dn <- function(vector){
  vector <- as.character(vector)
  vector <- substr(vector,2,1000000)
  vector <- gsub(",","",vector)
  vector <- as.numeric(vector)
}

dataAirbnb_city$price <- dn(dataAirbnb_city$price)
dataAirbnb_city$extra_people <- dn(dataAirbnb_city$extra_people)

dataAirbnb_city <- cbind(dataAirbnb_city,amenities_count)

#remove missing data
dataAirbnb_city <- na.omit(dataAirbnb_city)

attach(dataAirbnb_city)

#Remove highest and lowest price values 
dataAirbnb_city <- dataAirbnb_city[-(which(dataAirbnb_city$price==max(dataAirbnb_city$price))),]
dataAirbnb_city <- dataAirbnb_city[-(which(dataAirbnb_city$price==min(dataAirbnb_city$price))),]

#Mean and SD of price and log(price)
mean(dataAirbnb_city$price)
mean(log(dataAirbnb_city$price))

#Histogram of city price after minimum and maximum values have been removed
par(mfrow=c(1,2))

#Switch to Montreal for Montreal data set
hist(dataAirbnb_city$price, breaks=50, main = "Montreal Airbnb Listing Prices", sub="Figure 11", xlab = "Price ($) Mean: $90.39", ylab = "Frequency",xlim = c(0,1500),ylim = c(0,1000))
hist(log(dataAirbnb_city$price), breaks=50, main = "Montreal Airbnb Listing Log(Prices)", sub="Figure 12", xlab = "Log(Price) Mean: 4.29", ylab = "Frequency",xlim = c(2.5,8),ylim = c(0,550))

#Switch to Vancouver for Vancouver data set
#hist(dataAirbnb_city$price, breaks=50, main = "Vancouver Airbnb Listing Prices", sub="Figure 13", xlab = "Price ($) Mean: $121.32", ylab = "Frequency")
#hist(log(dataAirbnb_city$price), breaks=50, main = "Vancouver Airbnb Listing Log(Prices)", sub="Figure 14", xlab = "Log(Price) Mean: 4.63", ylab = "Frequency")

dataAirbnb_city <- as.data.frame(cbind(log(dataAirbnb_city$price),dataAirbnb_city$accommodates,dataAirbnb_city$bathrooms,dataAirbnb_city$bedrooms,dataAirbnb_city$latitude,dataAirbnb_city$review_scores_value,log(dataAirbnb_city$number_of_reviews),dataAirbnb_city$extra_people,dataAirbnb_city$review_scores_cleanliness,dataAirbnb_city$host_acceptance_rate,factor(dataAirbnb_city$host_is_superhost),dataAirbnb_city$amenities_count,dataAirbnb_city$review_scores_location,factor(dataAirbnb_city$property_type),factor(dataAirbnb_city$room_type,levels=c("Shared room","Private room","Entire home/apt"),ordered=TRUE),factor(dataAirbnb_city$instant_bookable)))

#data set values with a column of ones in the first column for matrix mulitplication of the intercept coefficient
mat_dataAirbnb_city <- cbind(rep(1,nrow(dataAirbnb_city)),dataAirbnb_city[,-1])
#matrix multiply data matrix with coefficient vector to get predicted log(price) 
pred_logPrice_city <- as.matrix(mat_dataAirbnb_city) %*% as.vector(modelCoefs)

#model error
errors_mod_city <- pred_logPrice_city - dataAirbnb_city[,1]

#MSRE using Toronto model coefficients on Monteal/Vancouver data
sqrt(sum((errors_mod_city)^2)/nrow(dataAirbnb_city))

#PRED25 full model
rel_change <- 1 - ((dataAirbnb_city[,1] - abs(errors_mod_city)) / dataAirbnb_city[,1])
pred_25 <- table(rel_change<0.25)["TRUE"] / nrow(dataAirbnb_city)
paste("PRED(25):", pred_25)

#PRED10 full model
rel_change <- 1 - ((dataAirbnb_city[,1] - abs(errors_mod_city)) / dataAirbnb_city[,1])
pred_10 <- table(rel_change<0.10)["TRUE"] / nrow(dataAirbnb_city)
paste("PRED(10):", pred_10)

#standardized residuals
st_errors_mod_city <- errors_mod_city/sd(errors_mod_city)

par(mfrow=c(1,2))

#Switch to Montreal for Montreal data set
#plot(pred_logPrice_city,st_errors_mod_city, main = "Montreal Std. Residuals v Fitted log(Price)",sub="Figure 5",xlab = "Fitted log(Price)", ylab = "Std. Residuals")
#hist(errors_mod_city, breaks = 50, main = "Histogram of Montreal Residuals", sub="Figure 6", xlab = "Residuals", ylab = "Frequency")

#Switch to Vancouver for Vancouver data set
plot(pred_logPrice_city,st_errors_mod_city, main = "Vancouver Std. Residuals v Fitted log(Price)",sub="Figure 7",xlab = "Fitted log(Price)", ylab = "Std.Residuals")
hist(errors_mod_city, breaks = 50, main = "Histogram of Vancouver Residuals", sub="Figure 8", xlab = "Residuals", ylab = "Frequency")


#The model is overfitted to the Toronto data set.
#The model does not perform well on the Montreal or Vancouver data sets
#Longitude was included to the Toronto model and tested with the Montreal and Vancouver data sets and the results were worse.
#This is likely because latitude is roughly the same for Toronto, Montreal and Vancouver but longtitude is vastly different.
#The coefficient for longitude created on the Toronto data set would not work properly for Monteal and Vancouver.