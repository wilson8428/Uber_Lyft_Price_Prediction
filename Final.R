
library(lubridate)
library(dplyr)
library(caret)
library(ggplot2)



cab_price = read.csv("/home/chen1614/cab_rides.csv",header = T)
weather = read.csv("/home/chen1614/weather.csv",header = T)


#Need to divide it by 1000 first in order to convert timestamp into correct value 
cab_price$time_stamp = cab_price$time_stamp/1000
cab_price$dateTime = as.POSIXct(cab_price$time_stamp, origin = "1970-01-01")
weather$dateTime = as.POSIXct(weather$time_stamp, origin = "1970-01-01")

#check for and replace missing values
sum(is.na(cab_price$price))
cab_price = cab_price[!is.na(cab_price$price), ]


weather$rain = ifelse(is.na(weather$rain),0,weather$rain)
weather$rain = ifelse(weather$rain > 0,1,0)  #turning rain into binary 
weather$rain = as.factor(weather$rain)       #convert rain into factor
colnames(weather)[1] = "temp"               #rename weird column name to temp

#create a new column to have a merge criteria
cab_price$merge_date = paste(as.character(cab_price$source), 
                             as.character(date(cab_price$dateTime)),  
                             as.character(hour(cab_price$dateTime)))
weather$merge_date = paste(as.character(weather$location), 
                           as.character(date(weather$dateTime)),  
                           as.character(hour(weather$dateTime)))

#merging the two table and add suffix for same column names
mergeData = merge(cab_price, weather, by = "merge_date", suffixes = c("_c", "_w"))

#group by id because after merge creates additional rows with repeating id 
grouped = aggregate(cbind(temp, clouds,pressure, humidity, wind) ~ id, data = mergeData, FUN = mean)
library(sqldf)
data = sqldf("select * from mergeData inner join grouped on grouped.id = mergeData.id group by grouped.id")
names(data)

# create new column that shows day of week
data$Day = as.factor(weekdays(data$dateTime_c))
data$hour = as.factor(hour(data$dateTime_c))


#change column names and dropping columns 
colnames(data)[colnames(data) == "name" ] = "car_type"
colnames(data)[colnames(data) == "price"] = "y"
data$merge_date = NULL
data$id = NULL 
data$product_id = NULL 
data$location = NULL 
data$dateTime_c = NULL 
data$dateTime_w = NULL
data$time_stamp_c = NULL
data$time_stamp_w = NULL 
data$id..22 = NULL
data$temp..23 = NULL
data$clouds..24 = NULL
data$pressure..25 = NULL
data$humidity..26 = NULL
data$wind..27 = NULL
data$surge_multiplier = NULL 

rm(mergeData, cab_price, grouped, weather)

#move y to first column
data = data[ ,c(5,1:4,6:14)]

#check outliers
par(mfrow = c(2,4))

outval2 = boxplot(data$temp)$out
outval3 = boxplot(data$clouds)$out
outval4 = boxplot(data$pressure)$out
outval6 = boxplot(data$humidity)$out
outval7 = boxplot(data$wind)$out

rm(outval2, outval3, outval4, outval5, outval6, outval7)

# output and store grouped data

write.csv(data, "/home/chen1614/cab_rides_grouped.csv")

# read data back again

data = read.csv("/home/chen1614/cab_rides_grouped.csv", header = T)


# remove id col
data$X = NULL

data$hour = as.factor(data$hour)

#EDA
#checking number of missing value
sapply(data, function(x) sum(is.na(x)))

# X         distance         cab_type      destination           source            price 
# 0                0                0                0                0                0 
# surge_multiplier             name             temp           clouds         pressure             rain 
# 0                0                0                0                0                0 
# humidity             wind           p_by_d 
# 0                0                0 


#create histograms of all variables to check for distribution
library(plyr)
library(psych)
multi.hist(data[,sapply(data, is.numeric)])


#check correlation
num_feature = data[ , c(1,2,7,8,9,11,12)]
cor(num_feature)
rm(num_feature)


# seperate data into Uber and lyft
Uber = data[data$cab_type == "Uber", ]
Lyft = data[data$cab_type == "Lyft", ]
rm(data)


Uber$cab_type = NULL
Lyft$cab_type = NULL



#creating dummies for categorical variable 
dummies = dummyVars(y ~ ., data = Uber)
ex = data.frame(predict(dummies, newdata = Uber))
Uber = cbind(y = Uber$y, ex)
str(Uber)

dummies = dummyVars(y ~ ., data = Lyft)
ex = data.frame(predict(dummies, newdata = Lyft))
Lyft = cbind(y = Lyft$y, ex)
str(Lyft)

Uber$rain = as.factor(Uber$rain)
Lyft$rain = as.factor(Lyft$rain)


#write out
write.csv(Uber, '/home/chen1614/Uber.csv')
write.csv(Lyft, '/home/chen1614/Lyft.csv')





## using H2o package
library("h2o")
#install.packages("h2o")
#Create partition: tr and te 

h2o.init(nthreads = 12, max_mem_size="64g")
h2o.clusterInfo()

#convert to h2o frame
UberH = as.h2o(Uber)
LyftH = as.h2o(Lyft)


set.seed(123)
y = "y"
x = setdiff(names(UberH),y)
parts = h2o.splitFrame(data = UberH, ratios = 0.7, seed = 99)
Utrain = parts[[1]]
Utest = parts[[2]]

x = setdiff(names(LyftH),y)
parts = h2o.splitFrame(data = LyftH, ratios = 0.7, seed = 99)
Ltrain = parts[[1]]
Ltest = parts[[2]]




set.seed(122)
Uglm = h2o.glm(x, y ,training_frame =  Utrain)
h2o.performance(Uglm,Utest)
# MSE:  5.739724
# RMSE:  2.395772
# MAE:  1.635041
# RMSLE:  0.1448626
# Mean Residual Deviance :  5.739724
# R^2 :  0.9221832
# Null Deviance :7273919
# Null D.o.F. :98614
# Residual Deviance :566022.9
# Residual D.o.F. :98581
# AIC :452248.2 

Lglm = h2o.glm(x, y ,training_frame =  Ltrain)
h2o.performance(Lglm,Ltest)
# MSE:  12.04242
# RMSE:  3.470219
# MAE:  2.118654
# RMSLE:  0.2316625
# Mean Residual Deviance :  12.04242
# R^2 :  0.8794696
# Null Deviance :9159623
# Null D.o.F. :91675
# Residual Deviance :1104001
# Residual D.o.F. :91632
# AIC :488385


Ugbm = h2o.gbm(x, y ,training_frame =  Utrain)
h2o.performance(Ugbm,Utest)
# MSE:  3.645174
# RMSE:  1.909234
# MAE:  1.198116
# RMSLE:  0.1123472
# Mean Residual Deviance :  3.645174
# R^2 :  0.9505802

Lgbm = h2o.gbm(x, y, training_frame =  Ltrain)
h2o.performance(Lgbm,Ltest)
# MSE:  9.272342
# RMSE:  3.045052
# MAE:  1.603143
# RMSLE:  0.1442546
# Mean Residual Deviance :  9.272342
# R^2 :  0.9071948

Urf = h2o.randomForest(x, y, training_frame =  Utrain)
h2o.performance(Urf, Utest)
# MSE:  3.667256
# RMSE:  1.915008
# MAE:  1.182412
# RMSLE:  0.1112033
# Mean Residual Deviance :  3.667256
# R^2 :  0.9502809

Lrf = h2o.randomForest(x, y, training_frame =  Ltrain)
h2o.performance(Lrf, Ltest)
# MSE:  9.641499
# RMSE:  3.105076
# MAE:  1.602446
# RMSLE:  0.1435336
# Mean Residual Deviance :  9.641499
# R^2 :  0.9035


Uxgb = h2o.xgboost(x, y, training_frame =  Utrain,max_runtime_secs = 300)
h2o.performance(Uxgb, Utest)
# MSE:  3.432346
# RMSE:  1.852659
# MAE:  1.148484
# RMSLE:  0.1074489
# Mean Residual Deviance :  3.432346
Lxgb = h2o.xgboost(x, y, training_frame =  Ltrain)
h2o.performance(Lxgb, Ltest)
# MSE:  8.749921
# RMSE:  2.958027
# MAE:  1.560261
# RMSLE:  0.139333
# Mean Residual Deviance :  8.749921




