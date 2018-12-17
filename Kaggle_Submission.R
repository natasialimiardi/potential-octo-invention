# ensure analysisData.csv and scoringData.csv are in your working directory
setwd('c:/Users/natas/OneDrive/Documents/Columbia University/APAN 5200/workingdirectory')
getwd()

# following code will read data and construct a simple model
data = read.csv('analysisData.csv')
#names(data)

library(lattice)
library(ggplot2)
library(caret)
library(caTools)
library(ISLR)
library(tidyr)
library(tidyverse)
library(dplyr)
library(lm.beta)
library(corrplot)
library(car)
library(leaps)
library(mice)
library(dplyr)
library(lubridate)
library(AppliedPredictiveModeling)
library(rpart); library(rpart.plot)
library(randomForest)
library(corrplot)
library(leaps)
library(ggplot2)
library(qdapTools)


#variable 61 is the price
#variable 57 after NA < 3000

data_preprocess = data[,!colSums(is.na(data))>26000]

##########
#pp_no_nzv = preProcess(data[, -61], method = c("nzv", "YeoJohnson"))
#data_okNAamt = predict(pp_no_nzv, newdata = data[, -61])

data_okNAamt = data

colSums(is.na(data_okNAamt[,colSums(is.na(data_okNAamt))>0]))

##########

# numonly = unlist(lapply(data_preprocess,is.numeric))
# data_preprocess = data_preprocess[,numonly]
# 
# tempData <- complete(mice(data_preprocess,m=5,maxit=30,meth='pmm',seed=500))
# colSums(is.na(tempData[,colSums(is.na(tempData))>0]))

#summary(tempData)

# write.csv(tempData, 'tempData.csv')  

tempData = read.csv('tempData.csv')
tempDataSquare = read.csv('tempDataSquare.csv')

#variables to NOT RELY ON: weekly_price, security_deposit

data_okNAamt$beds = tempData$beds
data_okNAamt$security_deposit = tempData$security_deposit
data_okNAamt$cleaning_fee = tempData$cleaning_fee
data_okNAamt$weekly_price = tempData$weekly_price
data_okNAamt$square_feet = tempDataSquare$square_feet

colSums(is.na(data_okNAamt[,colSums(is.na(data_okNAamt))>0]))


#host_nbh_level
data_okNAamt<-data_okNAamt%>%mutate(host_nbh_level=as.numeric(factor(c(reorder(host_neighbourhood,price,mean)))))
summary(data_okNAamt$host_nbh_level)
plot(density(data_okNAamt$host_nbh_level), at = seq(0, 340, by = 5))
data_okNAamt$host_nbh_level = as.factor(data_okNAamt$host_nbh_level)
levels(data_okNAamt$host_nbh_level)
levels(data_okNAamt$host_nbh_level) = c(rep(1,151),rep(2,55),rep(3,40),rep(4,88))
levels(data_okNAamt$host_nbh_level)
data_okNAamt$host_nbh_level = as.numeric(data_okNAamt$host_nbh_level)

# #host_rr_level-No boxplot
# data_okNAamt<-data_okNAamt%>%mutate(host_rr_level=as.numeric(factor(c(reorder(host_response_rate,price,mean)))))
# plot(density(data_okNAamt$host_rr_level))
# plot(density(data_okNAamt$host_rr_level), at = seq(0, 90, by = 1))
# data_okNAamt$host_rr_level = as.factor(data_okNAamt$host_rr_level)
# levels(data_okNAamt$host_rr_level)
# levels(data_okNAamt$host_rr_level) = c(rep(1,12),rep(2,5),rep(3,7),rep(4,6),rep(5,8),rep(6,9),rep(7,6),rep(8,7),rep(9,3),rep(10,13))
# levels(data_okNAamt$host_rr_level)
# data_okNAamt$host_rr_level = as.numeric(data_okNAamt$host_rr_level)

#street_level
data_okNAamt<-data_okNAamt%>%mutate(street_level=as.numeric(factor(c(reorder(street,price,mean)))))
summary(data_okNAamt$street_level)
data_okNAamt$street_level = as.factor(data_okNAamt$street_level)
levels(data_okNAamt$street_level) = c(rep(1,185),rep(2,31),rep(3,23))
data_okNAamt$street_level = as.numeric(data_okNAamt$street_level)

#host_l_level
data_okNAamt<-data_okNAamt%>%mutate(host_l_level=as.numeric(factor(c(reorder(host_location,price,mean)))))
summary(data_okNAamt$host_l_level)
data_okNAamt$host_l_level = as.factor(data_okNAamt$host_l_level)
levels(data_okNAamt$host_l_level)
levels(data_okNAamt$host_l_level) = c(rep(1,565),rep(2,406))
levels(data_okNAamt$host_l_level)
data_okNAamt$host_l_level = as.numeric(data_okNAamt$host_l_level)

#neighbourhood_c_level
data_okNAamt<-data_okNAamt%>%mutate(neighbourhood_c_level=as.numeric(factor(c(reorder(neighbourhood_cleansed,price,mean)))))
summary(data_okNAamt$neighbourhood_c_level)
data_okNAamt$neighbourhood_c_level = as.factor(data_okNAamt$neighbourhood_c_level)
levels(data_okNAamt$neighbourhood_c_level) = c(rep(1,130),rep(2,26),rep(3,26),rep(4,30))
data_okNAamt$neighbourhood_c_level = as.numeric(data_okNAamt$neighbourhood_c_level)

#neighbourhood_gc_level
data_okNAamt<-data_okNAamt%>%mutate(neighbourhood_gc_level=as.numeric(factor(c(reorder(neighbourhood_group_cleansed,price,mean)))))

#host_rt_level
data_okNAamt<-data_okNAamt%>%mutate(host_rt_level=as.numeric(factor(c(reorder(host_response_time,price,mean)))))

#property_type_level 
data_okNAamt<-data_okNAamt%>%mutate(property_type_level=as.numeric(factor(c(reorder(property_type,price,mean)))))
plot(density(data_okNAamt$property_type_level))
plot(density(data_okNAamt$property_type_level), at = seq(0, 40, by = 5))
summary(data_okNAamt$property_type_level)
data_okNAamt$property_type_level = as.factor(data_okNAamt$property_type_level)
levels(data_okNAamt$property_type_level) = c(rep(1,21),rep(2,14))
data_okNAamt$property_type_level = as.numeric(data_okNAamt$property_type_level)

#smart_location_level
data_okNAamt<-data_okNAamt%>%mutate(smart_location_level=as.numeric(factor(c(reorder(smart_location,price,mean)))))
plot(density(data_okNAamt$smart_location_level))
plot(density(data_okNAamt$smart_location_level), at = seq(0, 270, by = 5))
summary(data_okNAamt$smart_location_level)
data_okNAamt$smart_location_level = as.factor(data_okNAamt$smart_location_level)
levels(data_okNAamt$smart_location_level)
levels(data_okNAamt$smart_location_level) = c(rep(1,185),rep(2,31),rep(3,23))
levels(data_okNAamt$smart_location_level)
data_okNAamt$smart_location_level = as.numeric(data_okNAamt$smart_location_level)

#room_type_level
data_okNAamt<-data_okNAamt%>%mutate(room_type_level=as.numeric(factor(c(reorder(room_type,price,mean)))))

#host_is_superhost
data_okNAamt$host_is_superhost<-as.numeric(data_okNAamt$host_is_superhost)

#host_identity_verified
data_okNAamt$host_identity_verified<-as.numeric(data_okNAamt$host_identity_verified)

#host_has_profile_pic
data_okNAamt$host_has_profile_pic<-as.numeric(data_okNAamt$host_has_profile_pic)

#is_location_exact
data_okNAamt$is_location_exact<-as.numeric(data_okNAamt$is_location_exact)

#instant_bookable
data_okNAamt$instant_bookable<-as.numeric(data_okNAamt$instant_bookable)

#is_business_travel_ready
data_okNAamt$is_business_travel_ready<-as.numeric(data_okNAamt$is_business_travel_ready)

#cancellation_policy
data_okNAamt$cancellation_policy<-as.numeric(data_okNAamt$cancellation_policy)
summary(data_okNAamt$cancellation_policy)

#require_guest_profile_picture
data_okNAamt$require_guest_profile_picture<-as.numeric(data_okNAamt$require_guest_profile_picture)

#cleaning_fee_level
data_okNAamt<-data_okNAamt%>%mutate(cleaning_fee_level=as.numeric(factor(c(reorder(cleaning_fee,price,mean)))))
boxplot(data_okNAamt$price~data_okNAamt$cleaning_fee_level)
summary(data_okNAamt$cleaning_fee_level)
data_okNAamt$cleaning_fee_level = as.factor(data_okNAamt$cleaning_fee_level)
levels(data_okNAamt$cleaning_fee_level)
levels(data_okNAamt$cleaning_fee_level) = c(rep(1,31),rep(2,26),rep(3,23),rep(4,81))
levels(data_okNAamt$cleaning_fee_level)
data_okNAamt$cleaning_fee_level = as.numeric(data_okNAamt$cleaning_fee_level)

#bedrooms 
data_okNAamt<-data_okNAamt%>%mutate(bedrooms_level=as.numeric(factor(c(reorder(bedrooms,price,mean)))))
boxplot(data_okNAamt$price~data_okNAamt$bedrooms_level)
summary(data_okNAamt$bedrooms_level)
data_okNAamt$bedrooms_level = as.factor(data_okNAamt$bedrooms_level)
levels(data_okNAamt$bedrooms_level)
levels(data_okNAamt$bedrooms_level) = c(rep(1,2),rep(2,1),rep(3,8))
levels(data_okNAamt$bedrooms_level)
data_okNAamt$bedrooms_level = as.numeric(data_okNAamt$bedrooms_level)

#bathrooms 
data_okNAamt<-data_okNAamt%>%mutate(bathrooms_level=as.numeric(factor(c(reorder(bathrooms,price,mean)))))
summary(data_okNAamt$bathrooms_level)
data_okNAamt$bathrooms_level = as.factor(data_okNAamt$bathrooms_level)
levels(data_okNAamt$bathrooms_level) = c(rep(1,9),rep(2,9))
data_okNAamt$bathrooms_level = as.numeric(data_okNAamt$bathrooms_level)

#weekly_price 
data_okNAamt<-data_okNAamt%>%mutate(weekly_price_level=as.numeric(factor(c(reorder(weekly_price,price,mean)))))
summary(data_okNAamt$weekly_price_level)
data_okNAamt$weekly_price_level = as.factor(data_okNAamt$weekly_price_level)
levels(data_okNAamt$weekly_price_level)
levels(data_okNAamt$weekly_price_level) = c(rep(1,160),rep(2,124),rep(3,50),rep(4,29))
levels(data_okNAamt$weekly_price_level)
data_okNAamt$weekly_price_level = as.numeric(data_okNAamt$weekly_price_level)


#bed_type_level
data_okNAamt<-data_okNAamt%>%mutate(bed_type_level=as.numeric(factor(c(reorder(bed_type,price,mean)))))
summary(data_okNAamt$bed_type_level)
data_okNAamt$bed_type_level = as.factor(data_okNAamt$bed_type_level)
levels(data_okNAamt$bed_type_level) = c(rep(1,4),rep(2,1))
data_okNAamt$bed_type_level = as.numeric(data_okNAamt$bed_type_level)

#neighbourhood_level 
data_okNAamt<-data_okNAamt%>%mutate(n_level=as.numeric(factor(c(reorder(neighbourhood,price,mean)))))
summary(data_okNAamt$n_level)
data_okNAamt$n_level = as.factor(data_okNAamt$n_level)
levels(data_okNAamt$n_level) = c(rep(1,111),rep(2,139-112),rep(3,1),rep(4,162-139),rep(5,198-162))
data_okNAamt$n_level = as.numeric(data_okNAamt$n_level)

#zipcode_level
data_okNAamt<-data_okNAamt%>%mutate(z_level=as.numeric(factor(c(reorder(zipcode,price,mean)))))
summary(data_okNAamt$z_level)
data_okNAamt$z_level = as.factor(data_okNAamt$z_level)
levels(data_okNAamt$z_level) = c(rep(1,99),rep(2,138-100),rep(3,1),rep(4,158-138),rep(5,190-158))
data_okNAamt$z_level = as.numeric(data_okNAamt$z_level)

#amenities
a_count = as.numeric(str_count(data_okNAamt$amenities,","))
data_okNAamt = cbind(data_okNAamt,a_count)
data_okNAamt<-data_okNAamt%>%mutate(a_level=as.numeric(factor(c(reorder(a_count,price,mean)))))
summary(data_okNAamt$a_level)
data_okNAamt$a_level = as.factor(data_okNAamt$a_level)
levels(data_okNAamt$a_level) = c(rep(1,15),rep(2,22-17),rep(3,1),rep(4,32-22),rep(5,73-32))
data_okNAamt$a_level = as.numeric(data_okNAamt$a_level)



################

#Scoring

scoringData = read.csv('scoringData.csv')

#names(scoringData) #scoringData had 1 fewer variable, price 

colSums(is.na(scoringData[,colSums(is.na(scoringData))>0]))

# scoringData_preprocess = scoringData[,!colSums(is.na(scoringData))>7000]
pp_no_nzv = preProcess(scoringData, method = c("nzv"))
scoringData_okNAamt = predict(pp_no_nzv, newdata = scoringData)

colSums(is.na(scoringData_okNAamt[,colSums(is.na(scoringData_okNAamt))>0]))

scoringData_okNAamt$beds[is.na(scoringData_okNAamt$beds)] = mean(scoringData_okNAamt$beds[!is.na(scoringData_okNAamt$beds)])
scoringData_okNAamt$zipcode[is.na(scoringData_okNAamt$zipcode)] = mode(scoringData_okNAamt$zipcode[!is.na(scoringData_okNAamt$zipcode)])
scoringData_okNAamt$reviews_per_month[is.na(scoringData_okNAamt$reviews_per_month)] = mean(scoringData_okNAamt$reviews_per_month[!is.na(scoringData_okNAamt$reviews_per_month)])

colSums(is.na(scoringData_okNAamt[,colSums(is.na(scoringData_okNAamt))>0]))


numonly = unlist(lapply(scoringData_okNAamt,is.numeric))
scoringData_okNAamt_num = scoringData_okNAamt[,numonly]

tempscoringData <- complete(mice(scoringData_okNAamt_num,m=5,maxit=50,meth='cart',seed=500))
colSums(is.na(tempscoringData[,colSums(is.na(tempscoringData))>0]))

write.csv(tempscoringData, 'tempscoringData.csv') #change for every model made

tempscoringData = read.csv('tempscoringData.csv')


pp_no_nzv = preProcess(scoringData, method = c("nzv"))
scoringData_okNAamt = predict(pp_no_nzv, newdata = scoringData)
scoringData_okNAamtsqft = scoringData_okNAamt[,c(53:56,64,65, 59)]
scoringData_okNAamtsqft$accommodates = as.numeric(scoringData_okNAamtsqft$accommodates)
scoringData_okNAamtsqft$bedrooms = as.numeric(scoringData_okNAamtsqft$bedrooms)
numonly = unlist(lapply(scoringData_okNAamtsqft,is.numeric))
scoringData_okNAamtsqft = scoringData_okNAamtsqft[,numonly]

tempDataSquare <- complete(mice(scoringData_okNAamtsqft,m=5,maxit=30,meth='pmm',seed=500))

write.csv(tempDataSquare, 'tempscoringDataSquare.csv')

tempscoringDataSquarefeet = read.csv('tempscoringDataSquare.csv')

#what variable not to use because of NAs in scoringData: squarefeet, monthly price, license

scoringData_okNAamt$security_deposit = tempscoringData$security_deposit
scoringData_okNAamt$cleaning_fee = tempscoringData$cleaning_fee
scoringData_okNAamt$weekly_price = tempscoringData$weekly_price
# scoringData_okNAamt$square_feet = tempscoringDataSquarefeet$square_feet
scoringData_okNAamt$square_feet = tempscoringDataSquarefeet$square_feet 

colSums(is.na(scoringData_okNAamt[,colSums(is.na(scoringData_okNAamt))>0]))


#host_nbh_level
host_nbh_level = as.numeric(" ")
scoringData_okNAamt = cbind(scoringData_okNAamt,host_nbh_level)
lookup_0 = aggregate(host_nbh_level~host_neighbourhood,data_okNAamt[,c(32,97)],mean)
scoringData_okNAamt$host_nbh_level = lookup(scoringData_okNAamt$host_neighbourhood,lookup_0[,c(1:2)])

# #host_rr_level
# host_rr_level<-as.numeric(" ")
# scoringData_okNAamt<-cbind(scoringData_okNAamt,host_rr_level)
# lookup_1<-aggregate(host_rr_level~host_response_rate,data_okNAamt[,c(27,98)],mean)
# scoringData_okNAamt$host_rr_level<-lookup(scoringData_okNAamt$host_response_rate,lookup_1[,c(1:2)])

#street_level
street_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,street_level)
lookup_2<-aggregate(street_level~street,data_okNAamt[,c(38,98)],mean)
scoringData_okNAamt$street_level<-lookup(scoringData_okNAamt$street,lookup_2[,c(1:2)])

#host_l_level
host_l_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,host_l_level)
lookup_3<-aggregate(host_l_level~host_location,data_okNAamt[,c(24,99)],mean)
scoringData_okNAamt$host_l_level<-lookup(scoringData_okNAamt$host_location,lookup_3[,c(1:2)])

#neighbourhood_c_level
neighbourhood_c_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,neighbourhood_c_level)
lookup_4<-aggregate(neighbourhood_c_level~neighbourhood_cleansed,data_okNAamt[,c(40,100)],mean)
scoringData_okNAamt$neighbourhood_c_level<-lookup(scoringData_okNAamt$neighbourhood_cleansed,lookup_4[,c(1:2)])

#neighbourhood_gc_level
neighbourhood_gc_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,neighbourhood_gc_level)
lookup_5<-aggregate(neighbourhood_gc_level~neighbourhood_group_cleansed,data_okNAamt[,c(41,101)],mean)
scoringData_okNAamt$neighbourhood_gc_level<-lookup(scoringData_okNAamt$neighbourhood_group_cleansed,lookup_5[,c(1:2)])

# #host_rt_level
host_rt_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,host_rt_level)
lookup_6<-aggregate(host_rt_level~host_response_time,data_okNAamt[,c(26,102)],mean)
scoringData_okNAamt$host_rt_level<-lookup(scoringData_okNAamt$host_response_time,lookup_6[,c(1:2)])

# #market_level
# market_level<-as.numeric(" ")
# scoringData_okNAamt<-cbind(scoringData_okNAamt,market_level)
# lookup_7<-aggregate(market_level~market,data_okNAamt[,c(45,104)],mean)
# scoringData_okNAamt$market_level<-lookup(scoringData_okNAamt$market,lookup_7[,c(1:2)])

#property_type_level
property_type_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,property_type_level)
lookup_8<-aggregate(property_type_level~property_type,data_okNAamt[,c(52,103)],mean)
scoringData_okNAamt$property_type_level<-lookup(scoringData_okNAamt$property_type,lookup_8[,c(1:2)])

#smart_location_level
smart_location_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,smart_location_level)
lookup_9<-aggregate(smart_location_level~smart_location,data_okNAamt[,c(46,104)],mean)
scoringData_okNAamt$smart_location_level<-lookup(scoringData_okNAamt$smart_location,lookup_9[,c(1:2)])

#room_type_level
room_type_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,room_type_level)
lookup_10<-aggregate(room_type_level~room_type,data_okNAamt[,c(53,105)],mean)
scoringData_okNAamt$room_type_level<-lookup(scoringData_okNAamt$room_type,lookup_10[,c(1:2)])

#cleaning_fee_level
cleaning_fee_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,cleaning_fee_level)
lookup_11<-aggregate(cleaning_fee_level~cleaning_fee,data_okNAamt[,c(65,106)],mean)
scoringData_okNAamt$cleaning_fee_level<-lookup(scoringData_okNAamt$cleaning_fee,lookup_11[,c(1:2)])

#bedrooms
bedrooms_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,bedrooms_level)
lookup_12<-aggregate(bedrooms_level~bedrooms,data_okNAamt[,c(56,107)],mean)
scoringData_okNAamt$bedrooms_level<-lookup(scoringData_okNAamt$bedrooms,lookup_12[,c(1:2)])

#bathrooms
bathrooms_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,bathrooms_level)
lookup_13<-aggregate(bathrooms_level~bathrooms,data_okNAamt[,c(55,108)],mean)
scoringData_okNAamt$bathrooms_level<-lookup(scoringData_okNAamt$bathrooms,lookup_13[,c(1:2)])

#weekly_price
scoringData_okNAamt$weekly_price = round(scoringData_okNAamt$weekly_price)

weekly_price_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,weekly_price_level)
lookup_14<-aggregate(weekly_price_level~weekly_price,data_okNAamt[,c(62,109)],mean)
scoringData_okNAamt$weekly_price_level<-lookup(scoringData_okNAamt$weekly_price,lookup_14[,c(1:2)])

#bed_type
bed_type_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,bed_type_level)
lookup_16<-aggregate(bed_type_level~bed_type,data_okNAamt[,c(58,110)],mean)
scoringData_okNAamt$bed_type_level<-lookup(scoringData_okNAamt$bed_type,lookup_16[,c(1:2)])

#neighbourhood
n_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,n_level)
lookup_17<-aggregate(n_level~neighbourhood,data_okNAamt[,c(39,111)],mean)
scoringData_okNAamt$n_level<-lookup(scoringData_okNAamt$neighbourhood,lookup_17[,c(1:2)])

#zipcode
z_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,z_level)
lookup_18<-aggregate(z_level~zipcode,data_okNAamt[,c(44,112)],mean)
scoringData_okNAamt$z_level<-lookup(scoringData_okNAamt$zipcode,lookup_18[,c(1:2)])

# #security_deposit
# security_deposit_level<-as.numeric(" ")
# scoringData_okNAamt<-cbind(scoringData_okNAamt,security_deposit_level)
# scoringData_okNAamt$security_deposit[is.na(scoringData_okNAamt$security_deposit)] = 0
# lookup_19<-aggregate(security_deposit_level~security_deposit,data_okNAamt[,c(64,115)],mean)
# scoringData_okNAamt$security_deposit_level<-lookup(scoringData_okNAamt$security_deposit,lookup_19[,c(1:2)])

#ammenities 
a_count<-as.numeric(str_count(scoringData_okNAamt$amenities,","))
a_level<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,a_level,a_count)
lookup_15<-aggregate(a_level~a_count,data_okNAamt[,c(113,114)],mean)
scoringData_okNAamt$a_level<-lookup(scoringData_okNAamt$a_count,lookup_15[,c(1:2)])


# scoringData_okNAamtsqft1 = scoringData_okNAamt[,c(54:55, 59, 64:65, 113)]
# numonly = unlist(lapply(scoringData_okNAamtsqft1,is.numeric))
# scoringData_okNAamtsqft1 = scoringData_okNAamtsqft1[,numonly]
# 
# tempDataSquare1 <- complete(mice(scoringData_okNAamtsqft1,m=5,maxit=30,meth='pmm',seed=500))
# 
# write.csv(tempDataSquare1, 'tempscoringDataSquare1.csv')
# 
# tempscoringDataSquarefeet1 = read.csv('tempscoringDataSquare1.csv')
# 
# scoringData_okNAamt$square_feet = tempscoringDataSquarefeet1$square_feet


#fix 11 elements

colSums(is.na(scoringData_okNAamt[,colSums(is.na(scoringData_okNAamt))>0]))

scoringData_okNAamt$host_nbh_level[is.na(scoringData_okNAamt$host_nbh_level)] = mean(scoringData_okNAamt$host_nbh_level[!is.na(scoringData_okNAamt$host_nbh_level)])
scoringData_okNAamt$street_level[is.na(scoringData_okNAamt$street_level)] = mean(scoringData_okNAamt$street_level[!is.na(scoringData_okNAamt$street_level)])
scoringData_okNAamt$host_l_level[is.na(scoringData_okNAamt$host_l_level)] = mean(scoringData_okNAamt$host_l_level[!is.na(scoringData_okNAamt$host_l_level)])
scoringData_okNAamt$neighbourhood_c_level[is.na(scoringData_okNAamt$neighbourhood_c_level)] = mean(scoringData_okNAamt$neighbourhood_c_level[!is.na(scoringData_okNAamt$neighbourhood_c_level)])
#scoringData_okNAamt$market_level[is.na(scoringData_okNAamt$market_level)] = mean(scoringData_okNAamt$market_level[!is.na(scoringData_okNAamt$market_level)])
scoringData_okNAamt$property_type_level[is.na(scoringData_okNAamt$property_type_level)] = mean(scoringData_okNAamt$property_type_level[!is.na(scoringData_okNAamt$property_type_level)])
scoringData_okNAamt$smart_location_level[is.na(scoringData_okNAamt$smart_location_level)] = mean(scoringData_okNAamt$smart_location_level[!is.na(scoringData_okNAamt$smart_location_level)])
scoringData_okNAamt$cleaning_fee_level[is.na(scoringData_okNAamt$cleaning_fee_level)] = mean(scoringData_okNAamt$cleaning_fee_level[!is.na(scoringData_okNAamt$cleaning_fee_level)])
#scoringData_okNAamt$bathrooms_level[is.na(scoringData_okNAamt$bathrooms_level)] = mean(scoringData_okNAamt$bathrooms_level[!is.na(scoringData_okNAamt$bathrooms_level)])
scoringData_okNAamt$bedrooms_level[is.na(scoringData_okNAamt$bedrooms_level)] = mean(scoringData_okNAamt$bedrooms_level[!is.na(scoringData_okNAamt$bedrooms_level)])
scoringData_okNAamt$n_level[is.na(scoringData_okNAamt$n_level)] = mean(scoringData_okNAamt$n_level[!is.na(scoringData_okNAamt$n_level)])
scoringData_okNAamt$z_level[is.na(scoringData_okNAamt$z_level)] = mean(scoringData_okNAamt$z_level[!is.na(scoringData_okNAamt$z_level)])
#scoringData_okNAamt$square_feet[is.na(scoringData_okNAamt$square_feet)] = mean(scoringData_okNAamt$square_feet[!is.na(scoringData_okNAamt$square_feet)])

# sum(is.na(scoringData_okNAamt$weekly_price_level))


# weekly_price_level2<-as.numeric(" ")
# #scoringData_okNAamt<-cbind(scoringData_okNAamt,cleaning_fee_level2)
# weekly_price_no_level = filter(scoringData_okNAamt, is.na(weekly_price_level))
# weekly_price_no_level<-cbind(weekly_price_no_level,weekly_price_level2)

weeklypricelevel1 = filter(data_okNAamt, weekly_price_level == 1)
summary(weeklypricelevel1$weekly_price)
weeklypricelevel2 = filter(data_okNAamt, weekly_price_level == 2)
summary(weeklypricelevel2$weekly_price)
weeklypricelevel3 = filter(data_okNAamt, weekly_price_level == 3)
summary(weeklypricelevel3$weekly_price)
weeklypricelevel4 = filter(data_okNAamt, weekly_price_level == 4)
summary(weeklypricelevel4$weekly_price)

lookup_w = data.frame( weekly_price = c(0:449), weekly_price_level = 1)
lookup_x = data.frame( weekly_price = c(450:599), weekly_price_level = 2)
lookup_y = data.frame( weekly_price = c(600:849), weekly_price_level = 3)
lookup_z = data.frame( weekly_price = c(850:999), weekly_price_level = 4)

lookup_all = rbind(lookup_w, lookup_x, lookup_y,lookup_z)

weekly_price_no_level$weekly_price_level2<-lookup(weekly_price_no_level$weekly_price,lookup_all[,c(1:2)])
weekly_price_no_level$weekly_price_level3 = weekly_price_no_level$weekly_price_level2

weekly_price_level3<-as.numeric(" ")
scoringData_okNAamt<-cbind(scoringData_okNAamt,weekly_price_level3)
scoringData_okNAamt$weekly_price_level3<-lookup(weekly_price_no_level$id,weekly_price_no_level[,c(1,103)])


colSums(is.na(scoringData_okNAamtsqftNA[,colSums(is.na(scoringData_okNAamtsqftNA))>0]))



###########

#Model

set.seed(100)
forest8.2All= randomForest(price~
                                            room_type_level
                                          +cleaning_fee
                                          +bedrooms
                                          +bathrooms
                                          +weekly_price
                                          +accommodates
                                          #+neighbourhood_c_level
                                          +neighbourhood_gc_level
                                          +host_nbh_level
                                          +smart_location_level
                                          #+street_level
                                          +property_type_level
                                          +host_listings_count
                                          +availability_30
                                          +availability_90
                                          +availability_365
                                          +beds
                                          +host_l_level
                                          +security_deposit
                                          +review_scores_location
                                          +host_rt_level
                                          +guests_included
                                          +extra_people
                                          +minimum_nights
                                          +maximum_nights
                                          +number_of_reviews
                                          +review_scores_rating
                                          +review_scores_accuracy
                                          +review_scores_cleanliness
                                          +review_scores_checkin
                                          +review_scores_communication
                                          +review_scores_value
                                          +calculated_host_listings_count
                                          #+reviews_per_month
                                          +bed_type_level
                                          +a_count
                                          +z_level
                                          +n_level
                                          +square_feet
                                          +neighbourhood_gc_level*longitude*accommodates
                                          +smart_location_level*longitude
                                          +neighbourhood_c_level*longitude
                                          +host_nbh_level*longitude
                                          +street_level*longitude
                                          +cleaning_fee*longitude
                                          +security_deposit*longitude
                                          +weekly_price*longitude
                                          +host_l_level*longitude
                                          +review_scores_location*longitude
                                          +room_type_level*longitude
                                          +property_type_level*longitude
                                          +z_level*longitude
                                          +n_level*longitude
                                            # +availability_30*host_nbh_level
                                            # +availability_30*street_level
                                            # +availability_30*neighbourhood_gc_level
                                            # +availability_30*smart_location_level
                                            # +availability_30*room_type_level
                                            # +availability_30*z_level
                                            # +availability_30*n_level
                                            # 
                                            # +availability_90*host_nbh_level
                                            # +availability_90*street_level
                                            # +availability_90*neighbourhood_gc_level
                                            # +availability_90*smart_location_level
                                            # +availability_90*room_type_level
                                            # +availability_90*z_level
                                            # +availability_90*n_level
                                            # +availability_365*host_nbh_level
                                            # +availability_365*street_level
                                            # +availability_365*neighbourhood_gc_level
                                            # +availability_365*smart_location_level
                                            # +availability_365*room_type_level
                                            # +availability_365*z_level
                                            # +availability_365*n_level
                                            +calculated_host_listings_count*weekly_price
                                            +reviews_per_month*security_deposit
                                            +room_type_level*calculated_host_listings_count
                                          ,data = data_okNAamt,
                                          ntree=300)
predForest8.2All = predict(forest8.2All,newdata=scoringData_okNAamt) 

First10 = data.frame(id = scoringData_okNAamt$id[1:10] , price =predForest8.2All[1:10]) ; First10   #use our model to predict the first 10 
# construct submision from predictions
submissionFile = data.frame(id = scoringData_okNAamt$id, price = predForest8.2All)

colSums(is.na(submissionFile[,colSums(is.na(submissionFile))>0]))

write.csv(submissionFile, 'sample_submission15.csv',row.names = F) #change for every model made
