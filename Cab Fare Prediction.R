#Clean the environment nd delete all th variables

rm(list = ls())

#Set working Directory
setwd("C:/Users/singhank/Downloads")
getwd()

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_211')
#library(rJava)
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine')
lapply(x, require, character.only = TRUE)
rm(x)


# Reading the data
df=read.csv("train_cab.csv", na.strings =c("", " ", NA),header=T)
df_test=read.csv("test.csv", header=T)
str(df)
hist(df$fare_amount)
#Reading the date and extracting infromation from datetime
library("lubridate")

#Changing to the correct data type
df$pickup_datetime= parse_date_time(df$pickup_datetime, orders="ymd HMS")
df$fare_amount=as.numeric(levels(df$fare_amount))[df$fare_amount]
df$passenger_count=as.integer(df$passenger_count)
df$month= as.integer(strftime(df$pickup_datetime, "%m"))
df$year= as.integer(strftime(df$pickup_datetime, "%Y"))
df$hour= as.integer(strftime(df$pickup_datetime, "%H"))
df$date= as.integer(strftime(df$pickup_datetime, "%d"))
df$wd= as.integer(strftime(df$pickup_datetime, "%u"))

df_test$pickup_datetime= parse_date_time(df_test$pickup_datetime, orders="ymd HMS")
df_test$passenger_count=as.integer(df_test$passenger_count)
df_test$month= as.integer(strftime(df_test$pickup_datetime, "%m"))
df_test$year= as.integer(strftime(df_test$pickup_datetime, "%Y"))
df_test$hour= as.integer(strftime(df_test$pickup_datetime, "%H"))
df_test$date= as.integer(strftime(df_test$pickup_datetime, "%d"))
df_test$wd= as.integer(strftime(df_test$pickup_datetime, "%u"))


----------------#Missing value analysis--------------------------------------
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"

#To calculate percentage missing value
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100

# Sorting missing_val in Descending order
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL


# Rearranging columns
missing_val = missing_val[,c(2,1)]
missing_val


#Ploting missing value
library(ggplot2)
ggplot(data = missing_val[1:21,], aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "grey")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + theme_bw()

# As missing values form a very insignificant portion of total values (less than 1%), we can remove these rows.
df=na.omit(df)


#--------------------------------#EDA and Outlier Analysis--------------------------------------------

# 1.  Analyze fare amount
summary(df$fare_amount)

df=df[df$fare_amount>=2.5,]
df=df[df$fare_amount<400,]

hist(df$fare_amount)


# 1. Analyzing passenger count
# Looking at the test data as well as intuitively, more than 6 passengers in a cab doesnt seem reasonable.

df=df[df$passenger_count<=6,]
df=df[df$passenger_count>0,]
hist(df$passenger_count)
unique(df_test$passenger_count)



#2. Analyzing latitude and longitude

#As Latitude and longitude vary alot , we have tried to restrict them to the range in test data.
long_min=min(min(df_test$pickup_longitude),min(df_test$dropoff_longitude))
long_max= max(max(df_test$pickup_longitude),max(df_test$dropoff_longitude))
lat_min=min(min(df_test$pickup_latitude),min(df_test$dropoff_latitude))
lat_max= max(max(df_test$pickup_latitude),max(df_test$dropoff_latitude))

df = df[df$pickup_longitude>long_min & (df$pickup_longitude<long_max),]
df = df[df$dropoff_longitude>long_min & (df$dropoff_longitude<long_max),]
df = df[df$dropoff_latitude>lat_min & (df$dropoff_latitude<lat_max),]
df = df[df$pickup_latitude>lat_min & (df$pickup_latitude<lat_max),]

# Calculating distance
dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c*0.621371
  return(d)
}

df$distance=dist(df$pickup_longitude,df$pickup_latitude,df$dropoff_longitude,df$dropoff_latitude)
df_test$distance=dist(df_test$pickup_longitude,df_test$pickup_latitude,df_test$dropoff_longitude,df_test$dropoff_latitude)
summary(df$distance)
summary(df_test$distance)
distance

cont= c( "fare_amount","pickup_longitude","pickup_latitude","dropoff_longitude",,"distance")

attach(df)
plot(distance, fare_amount, main="Scatterplot", 
     xlab="Distance in miles ", ylab="Fare amount", pch=19)

# Distance could be 0 due to cancellation of tripsresulting in cancalletion charges or due ot incorrect noises. Taking the avergae of all such fares.
# I also crosschecked and seems like Uber charges $10 cancelling fees which is similar with our results.
df$fare_amount[df$distance==0]=mean(df$fare_amount[df$distance==0])

#For higher distance ( around 60 kms), we see that expenses are quite low which doesnt seem reasonable. Thus we have imputed the fare amount
df$fare_amount[df$distance>40]=NA
df = subset(df,select = -c(pickup_datetime))
df_test = subset(df_test,select = -c(pickup_datetime))
df = knnImputation(df, k=3)
sum(is.na(df))

df=df[df$fare_amount<150,]

attach(df)
plot(hour, farepermile, main="Scatterplot", 
     xlab="hour", ylab="Fare amount", pch=19)


#------------------fARE AMOUNT VS INDEPENDENT VARIABLES PLOT------------
# Fare amount vs Distance
attach(df)
plot(distance, fare_amount, main="Scatterplot", 
     xlab="Distance in miles ", ylab="Fare amount", pch=19)

# # Fare amount vs month
attach(df)
plot(month, fare_amount, main="Scatterplot", 
     xlab="month ", ylab="Fare amount", pch=19)

# # Fare amount vs hour
attach(df)
plot(hour, fare_amount, main="Scatterplot", 
     xlab="hour ", ylab="Fare amount", pch=19)

# # Fare amount vs year
attach(df)
plot(year, fare_amount, main="Scatterplot", 
     xlab="year ", ylab="Fare amount", pch=19)

# # Fare amount vs date
attach(df)
plot(date, fare_amount, main="Scatterplot", 
     xlab="date ", ylab="Fare amount", pch=19)

# # Fare amount vs DAy of week
attach(df)
plot(wd, fare_amount, main="Scatterplot", 
     xlab="Day of week ", ylab="Fare amount", pch=19)

aggregate(df$fare_amount, list(df$hour), mean)


df$session <- with(df,  ifelse(hour >= 4 & hour<12, "morning",
                           ifelse(hour>11 & hour<17, "afternoon", 
                                  ifelse(hour>17 & hour<24, "eve",   "night"))))

df_test$session <- with(df_test,  ifelse(hour >= 4 & hour<12, "morning",
                               ifelse(hour>11 & hour<17, "afternoon", 
                                      ifelse(hour>17 & hour<24, "eve",   "night"))))
df$session=as.factor(df$session)
df_test$session=as.factor(df_test$session)

# Calculating distance from airport
ewr=cbind(-74.175,40.69)
lgr=cbind(-73.87,40.77)
jfk=cbind(-73.782222222,40.64416666666667)

df$pickup_distance_to_jfk=dist(jfk[1],jfk[2],df$pickup_longitude,df$pickup_latitude)
df$dropff_distance_to_jfk=dist(jfk[1],jfk[2],df$dropoff_longitude,df$dropoff_latitude)
df$pickup_distance_to_lgr=dist(lgr[1],lgr[2],df$pickup_longitude,df$pickup_latitude)
df$dropff_distance_to_lgr=dist(lgr[1],lgr[2],df$dropoff_longitude,df$dropoff_latitude)
df$pickup_distance_to_ewr=dist(ewr[1],ewr[2],df$pickup_longitude,df$pickup_latitude)
df$dropff_distance_to_ewr=dist(ewr[1],ewr[2],df$dropoff_longitude,df$dropoff_latitude)

df_test$pickup_distance_to_jfk=dist(jfk[1],jfk[2],df_test$pickup_longitude,df_test$pickup_latitude)
df_test$dropff_distance_to_jfk=dist(jfk[1],jfk[2],df_test$dropoff_longitude,df_test$dropoff_latitude)
df_test$pickup_distance_to_lgr=dist(lgr[1],lgr[2],df_test$pickup_longitude,df_test$pickup_latitude)
df_test$dropff_distance_to_lgr=dist(lgr[1],lgr[2],df_test$dropoff_longitude,df_test$dropoff_latitude)
df_test$pickup_distance_to_ewr=dist(ewr[1],ewr[2],df_test$pickup_longitude,df_test$pickup_latitude)
df_test$dropff_distance_to_ewr=dist(ewr[1],ewr[2],df_test$dropoff_longitude,df_test$dropoff_latitude)




library(gplots)
# Plot the mean of cost per mile by hours
plotmeans(dpm ~ hour, data = df, frame = FALSE, bars=FALSE, n.label=FALSE, main="Cost per mile vs HOur", xlab="hours ", ylab="Fare amount per mile")

plotmeans(dpm ~ wd, data = df, frame = FALSE, bars=FALSE, n.label=FALSE, main="Cost per mile vs HOur", xlab="hours ", ylab="Fare amount per mile")


#---------------------------Feature Selection-------------------------------
colnames(df)
cont=c("fare_amount", "pickup_longitude", "pickup_latitude","dropoff_longitude","dropoff_latitude" ,"distance", "pickup_distance_to_jfk" ,"dropff_distance_to_jfk","pickup_distance_to_lgr", "dropff_distance_to_lgr", "pickup_distance_to_ewr", "dropff_distance_to_ewr")
## Correlation Plot 
corrgram(df[,cont], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

 
cat=c("passenger_count","month","year","hour","date","wd","session")

anova_test = aov(fare_amount ~passenger_count+month+year+hour+date+wd+session, data = df)
summary(anova_test)

df=subset(df,select=-c(passenger_count,date,wd))

df_test=subset(df_test,select=-c(passenger_count,date,wd))
colnames(df)
colnames(df_test)
str(df)
str(df_test)
#-------------------Modelling-----------------
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train = df[train_index,]
test  = df[-train_index,]


###________________ LINEAR REGRESSION _______________________________

library("usdm")


#Model development
LR_model = lm(fare_amount ~ ., data = train)

colnames(train)
#Prediction for training and testdata
LR_train = predict(LR_model)
LR_test = predict(LR_model,test[,-1])

# Metrics for  training data 
print(postResample(pred = LR_train, obs = train$fare_amount))
#RMSE          Rsquared       MAE 
#4.6526384    0.7517427   2.4239999
print(postResample(pred = LR_test, obs = test$fare_amount))
#RMSE          Rsquared       MAE 
#5.0319433 0.7267836     2.5107735 

#---------------------------------------Descion Tree Regression-------------------

library(rpart)

#Model development
DT_model = rpart(fare_amount ~., data = train, method = "anova")


#Prediction for training and testdata
DT_train= predict(DT_model)
DT_test= predict(DT_model,test[,-1])

# Metrics for  training data 
print(postResample(pred = DT_train, obs = train$fare_amount))
#RMSE       Rsquared        MAE 
#4.3640812  0.7815817   2.5160316  

# Metrics for testing data 
print(postResample(pred = DT_test, obs =  test$fare_amount))
#RMSE        Rsquared        MAE 
#4.7096866   0.7607238   2.5713934 

#--------------------------Random Forest Model
library("randomForest")

#Model development
RF_model = randomForest(fare_amount ~., data=train,  ntree=500)


 #Prediction for training and testdata
RF_train = predict(RF_model,train[,-1])
RF_test=predict(RF_model,test[,-1])

# Metrics for  training data  
print(postResample(pred = RF_train, obs = train$fare_amount))
#RMSE          R-squared       MAE
#1.6339130      0.9714253  0.7886843 

## Metrics for testing data 
print(postResample(pred = RF_test, obs =test$fare_amount))

#RMSE          R-squared       MAE
#4.1603199    0.8138908        1.9460753




#--------------------------------------------------Gradient Boosting-----------------------

#Model development
GBM_model = gbm(fare_amount~., data = train, n.trees = 600, interaction.depth = 2)
df$session=as.factor(df$session)
#Prediction for training and testdata
GBM_train = predict(GBM_model, train[,-1], n.trees = 500)
GBM_test = predict(GBM_model,test[,-1], n.trees = 500)

# Metrics for  training data  
print(postResample(pred = GBM_train, obs = train$fare_amount))
#RMSE       Rsquared       MAE 
#3.2394152  0.8798695 1.8000165 

## Metrics for testing data 
print(postResample(pred = GBM_test, obs = test$fare_amount))

#RMSE          Rsquared       MAE 
#3.9801129      0.8288344 1.9837323


# Prediction for test data
N = predict(GBM_model,df_test, n.trees = 200)
df_test$fare_amount=N





