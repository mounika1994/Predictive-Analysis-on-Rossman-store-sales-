#Read the input files
store <-read.csv(file="store.csv", na.strings=c("", "NA"))
test <-read.csv(file="test.csv", na.strings=c("", "NA"))
train <-read.csv(file="train.csv", na.strings=c("", "NA"))

#Finding out missing values for variables
sum(is.na(train))
sum(is.na(test))
sum(is.na(store))

#To see the missing values locations in test dataset
test[!complete.cases(test),]
train[!complete.cases(train),]
store[!complete.cases(store),]

#To summarize the datasets
library(plyr)
summary(train)
summary(test)
summary(store)

#making sure that closed shops have sales = 0
summary(train$Sales[!train$Open])

#since prediction of sales for closed stores will be zero, we can opt out the closed stores data
train <- train[train$Open == 1,]

length(unique(train$Store))
length(unique(test$Store))

#All the stores in the train set are not present in the test set, so training for those stores wouldn't make sense
#Removing those stores from the train set
train <- train[train$Store %in% unique(test$Store),]

#the store is opened all days except sunday, the day of week for the rows that have NA values does not have sundays
#Replacing NA values with 1 for these rows

test[is.na(test$Open),]$Open <- 1
sum(is.na(test$Open))

#Converting the Open, Promo, SchoolHoliday to logical values in train and test datasets
train$Open <- as.logical(train$Open)
train$Promo <- as.logical(train$Promo)
train$SchoolHoliday <- as.logical(train$SchoolHoliday)

test$Open <- as.logical(test$Open)
test$Promo <- as.logical(test$Promo)
test$SchoolHoliday <- as.logical(test$SchoolHoliday)

#Exploring sales statistics
summary(train$Sales)
sd(train$Sales)

#Plotting a histogram for sales in train dataset
hist(train$Sales,xlab="Sales")

#Plotting a boxplot for sales in train dataset
boxplot(train$Sales)

#There are so many outliers above 20000. Investigating sales above 20000
summary(train[train$Sales > 20000,])
outliers <- subset(train[train$Sales > 20000, ])

hist(aggregate(train$Sales, 
               by = list(train$Store), mean)$x, 75,
     main = "Mean sales of each store")

hist(train$Customers, 40)

hist(aggregate(train$Customers, 
               by = list(train$Store), mean)$x, 100,
     main = "Mean customers/store")

# Predicting that sales and customers are positively related, promo and sales are positively related
# as promo would attract more customers and customers is positively related to sales

tapply(train$Sales,train$DayOfWeek,mean)
#It is observed that sales are high on Mondays and Sundays and there is even distribution on all other days

#Performing ttest for with Promo and without promo
t.test(train[train$Promo,]$Sales,train[!train$Promo,]$Sales)

#Performing ttest for with Promo and without promo w.r.t. Customers
t.test(train[train$Promo,]$Customers,train[!train$Promo,]$Customers)

#we can observe that Promo has significant effect on sales
boxplot(train[train$Promo,]$Sales, train[!train$Promo,]$Sales, names= c("Sales with Promo", "Sales without Promo"))

boxplot(train[train$Promo,]$Customers, train[!train$Promo,]$Customers, names= c("Promo with Customer", "Promo without Customer"))

#Testing the same for SchoolHoliday and StateHoliday
t.test(train[train$StateHoliday != 0,]$Sales,train[train$StateHoliday == 0,]$Sales)
t.test(train[train$SchoolHoliday,]$Sales,train[!train$SchoolHoliday,]$Sales)
#State Holidays have very significant effect, there is increase in sales for school holidays too, but the rate is very small

#merging train and store data
train_store <- merge(train, store, by = "Store")

library(ggplot2)

#Finding the effect of promo interval on sales
ggplot(train_store, aes(x = factor(PromoInterval), y = Sales)) + 
  geom_jitter(alpha = 0.1) + 
  geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

#To analyse the impact of storetype on sales
ggplot(train_store, 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
  geom_smooth(size = 2)

#To find the distribution of the different store types 
ggplot(train_store, aes(StoreType, fill= StoreType)) +geom_bar()+
  ylab("Store count of total store") +
  ggtitle("Distribution of avilable StoreTypes")

#To find the distribution of the different assortment types 
ggplot(train_store, aes(Assortment, fill= Assortment)) +
  geom_bar()+xlab("AssortmentType")+ggtitle("Distribution of available AssortmentTypes")

#To analyse the effect of assortment type on sales
ggplot(train_store, aes(x = Assortment , y = Sales, fill= Assortment)) + 
  geom_boxplot() + scale_y_continuous(breaks = seq(0,100000,5000))+
  ggtitle("Boxplot showing the effect of Assortment Type on Sales")

#To analyse the effect of promotions on sales
ggplot(train_store, aes(x = Promo2 , y = Sales, color = factor (Promo2))) + 
  geom_boxplot() + scale_y_continuous(breaks = seq(0,100000,10000))+
  scale_x_continuous(breaks = seq(0,1,1))+xlab("Promotion on/off")+
  ylab("Sales of Stores")+
  ggtitle("Boxplot of the effect of the promotion on sales")

#Boxplot showing the Sales over the different competition years
qplot(factor(CompetitionOpenSinceYear), Sales, data = train_store, 
      fill = factor(CompetitionOpenSinceYear),geom = "boxplot")+
  scale_y_continuous(breaks = seq(0,100000,5000))+xlab("Competition Year")


ggplot(train_store[train$Store == 256,])+geom_line(aes(x= Date, y = Sales))+
  scale_y_continuous(breaks = seq(0,100000,4000))+xlab("Timeline")+
  ggtitle("Sales trend of a chosen store only for open days")


ggplot(train_store, aes(x = StateHoliday, y = Sales)) + 
  
  geom_boxplot() + scale_y_continuous(breaks = seq(0,100000,5000))


#sales for december 
tapply(train$Sales,train$DayOfWeek,mean)
plot(tapply(train$Sales,train$DayOfWeek,mean),xlab="Day",ylab="Sales mean")

plot(tapply(train$Sales,train$DateDay,mean),xlab="Month",ylab="Sales mean")

tapply(train$Sales,train$DateMonth,mean)
train[train$Sales==0,]

train_sales <- as.numeric(train$Sales == 0) && train$Open == 1
train_sales

train[train$Open == 1 && train$Sales == 0]

table(ifelse(train$Open == 1, "Opened"),
      ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))

install.packages("ggplot2", dependencies = TRUE)

View(train_store)

str(train_store)

sapply(train,function(x) sum(is.na(x)))

sapply(test,function(x) sum(is.na(x)))

sapply(train_store,function(x) sum(is.na(x)))

sum(is.na(train_store))

#Converting the date variable, the strftime function extracts a string from the date, so this must 
#be transformed to a numeric value in train and test data
traindata$DYear <- as.numeric(strftime(traindata$Date, format="%y"))
traindata$DMonth <- as.numeric(strftime(traindata$Date, format="%m"))
traindata$DDay <- as.numeric(strftime(traindata$Date, format="%d"))
traindata$DWeek <- as.numeric(strftime(traindata$Date, format="%W"))

testdata$DYear <- as.numeric(strftime(testdata$Date, format="%y"))
testdata$DMonth <- as.numeric(strftime(testdata$Date, format="%m"))
testdata$DDay <- as.numeric(strftime(testdata$Date, format="%d"))
testdata$DWeek <- as.numeric(strftime(testdata$Date, format="%W"))

#removing date column from the train data
traindata <- traindata[c(1,2,4:22)]
testdata <- testdata[c(1,2,4:22)]

#Finding the mode to replace the NA values in the dataset
Mode = function(x){ 
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

modePromoInterval <- Mode(train_store[!is.na(train_store$PromoInterval),17])
train_store[is.na(train_store$PromoInterval),17] <- modePromoInterval
train_store[is.na(train_store)] <- 0

#Converting the non-numeric variables to numeric
train_store$PromoInterval <- as.numeric(train_store$PromoInterval)
train_store$Open <- as.numeric(train_store$Open)
train_store$Promo <- as.numeric(train_store$Promo)
train_store$SchoolHoliday <- as.numeric(train_store$SchoolHoliday)
train_store$StoreType <- as.numeric(train_store$StoreType)
train_store$Assortment <- as.numeric(train_store$Assortment)
train_store$StateHoliday <- as.numeric(train_store$StateHoliday)
train_store$Store <- as.numeric(train_store$Store)
train_store$DayOfWeek <- as.numeric(train_store$DayOfWeek)
train_store$Sales <- as.numeric(train_store$Sales)
train_store$Customers <- as.numeric(train_store$Customers)
train_store$Promo2 <- as.numeric(train_store$Promo2)

# model the train data 
smp_size <- floor(0.60 * nrow(train_store))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train_store)), size = smp_size)

traindata <- train_store[train_ind, ]
testdata <- train_store[-train_ind, ]
View(traindata)
View(testdata)

#Merging the test and store datasets
test_store <- merge(test, store, by = "Store")
test_store$Customers <- 0
#test_store<- na.omit(test_store)

#Converting the date variable, the strftime function extracts a string from the date, so this must 
#be transformed to a numeric value in test_store dataset
test_store$DYear <-
  as.numeric(strftime(test_store$Date, format = "%y"))
test_store$DMonth <-
  as.numeric(strftime(test_store$Date, format = "%m"))
test_store$DDay <-
  as.numeric(strftime(test_store$Date, format = "%d"))
test_store$DWeek <-
  as.numeric(strftime(test_store$Date, format = "%W"))

#Removing the date column from test_store
test_store <- test_store[c(1:3,5:22)]

#Converting the non-numeric variables to numeric
test_store$Customers <- as.numeric(test_store$Customers)
test_store$Open <- as.numeric(test_store$Open)
test_store$Promo <- as.numeric(test_store$Promo)
test_store$SchoolHoliday <- as.numeric(test_store$SchoolHoliday)
test_store$StoreType <- as.numeric(test_store$StoreType)
test_store$Assortment <- as.numeric(test_store$Assortment)
test_store$StateHoliday <- as.numeric(test_store$StateHoliday)
test_store$Store <- as.numeric(test_store$Store)
test_store$DayOfWeek <- as.numeric(test_store$DayOfWeek)
test_store$Sales <- as.numeric(test_store$Sales)
test_store$Customers <- as.numeric(test_store$Customers)
test_store$Promo2 <- as.numeric(test_store$Promo2)

#Reordering the columns in test_store
test_store<- test_store[c(1,2,3,17,4:16,18:21)]

#Replacing the NA values with mode
modePromoInterval <- Mode(test_store[!is.na(test_store$PromoInterval),17])
test_store[is.na(test_store$PromoInterval),17] <- modePromoInterval
test_store[is.na(test_store)] <- 0

#Making the levels of stateholiday in both the test_store and train data equal
levels(test_store$StateHoliday) <- levels(traindata$StateHoliday)

memory.limit(size=107125)

###Applying Linear Regression model on the dataset

trainmodel <- lm(Sales ~ ., data = traindata)
options(warn = -1)
#Predicting the model
predictmodel <- predict(trainmodel, testdata)
options(warn = 1)
#Finding the error rate
sqrt(mean((testdata$Sales - predictmodel) ^ 2))

#Plot showing the sales predicted by  linear regression
plot(testdata$Sales, predictmodel, col= "blue", pch = 16, ylab= "Predicted sales for Linear Regression", xlab= "Actual Sales")


### Applying Random Forest on the dataset

library(randomForest)

set.seed(32)
#Fitting the model
rf <- randomForest(
    Sales ~ .,
    data = traindata,
    mtry = 2,
    importance = TRUE,
    ntree = 20
  )
print(rf)

#Predicting the model
pred <- predict(rf, testdata)
summary(pred)
str(pred)
#Finding the error rate
sqrt(mean((testdata$Sales - pred) ^ 2))

#plot for the sales predicted
plot(testdata$Sales, pred, col= "blue", pch = 16, ylab= "Predicted sales for Random Forest", xlab= "Actual Sales")


###Applying Logistic regression on the dataset

library(dplyr)

#Converting Sales variable from continuous to categorical

traindata$Category <- cut(traindata$Sales, breaks = c(0,16000,Inf), labels = c("low","high"))

#fitting logistic regression
model <- glm (Category ~ ., data = traindata, family = binomial)
summary(model)

#predicting the model
predict <- predict(model,type = 'response')
predict
View(traindata)
str(traindata)

#evaluating the accuracy using confusion matrix
traindata <- na.omit(traindata)
table(traindata$Category, predict > 0.5)

#evaluating Logistic Regression
library(pscl)
pR2(model)
#Consider the McFadden value, as it is close to 1, the predictive power of this model is high.

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(predict, traindata$Category)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#plot glm
library(ggplot2)
ggplot(traindata, aes(x=Sales, y=Category)) + geom_point() + 
stat_smooth(method="glm", family="binomial", se=FALSE)

###Applying Neural Network on the dataset

library(neuralnet)

#split train_store dataset into 60% train and 40% test
smp_size <- floor(0.60 * nrow(train_store))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train_store)), size = smp_size)

traindata <- train_store[train_ind, ]
testdata <- train_store[-train_ind, ]

#Normalizing data using min max normalization
maximum <- apply(traindata, 2, max)
minimum <- apply(traindata, 2, min)
scaled <- as.data.frame(scale(traindata, center = minimum, scale = maximum - minimum)) 

#creating train and test data from the scaled dataset
trainNN <- scaled[train_ind,]
testNN <- scaled[-train_ind,]

#Creating Neural Network Model
set.seed(2)
NN = neuralnet(Sales ~ Store + DayOfWeek + Customers + CompetitionDistance + StoreType + CompetitionOpenSinceMonth + Promo2 + Promo + DYear + DMonth + DDay + DWeek, trainNN, hidden = 3, threshold = 0.1)

#plot Neural Network
plot(NN)

#computing sales for test data
predict_testNN = compute(NN, testNN[, c(1,2,4,11,9,12,13,15,6,18,19,20, 21)])
predict_testNN = (predict_testNN$net.result* (max(traindata$Sales) - min(traindata$Sales))) + min(traindata$Sales)

#plot for predicted sales and actual sales
plot(testdata$Sales, predict_testNN, col = "blue", pch = 16, ylab = "Predicted Sales NN", xlab = "Real Sales")

abline(0,1)

#calculating root mean square error
RMSE.NN = sqrt(mean((testdata$Sales - predict_testNN)^2))

#Applying best model i.e., Random forest to the test dataset to predict the sales
test_store$predittest <- predict(rf, test_store[c(1,3:21)])



