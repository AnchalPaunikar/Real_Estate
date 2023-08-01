
library(dplyr)
library(tidyr)
library(car)

#IMPORTING DATA INTO R
h_train = read.csv("housing_train.csv")
h_test = read.csv("housing_test.csv")

#Adding target variable "Price" in test data
h_test$Price =  NA

#Adding new column to differentiate between train and test data after combining
h_test$data = "test"
h_train$data = "train"

#Combining train and test data
housing_all = rbind(h_train, h_test)
housing_all = housing_all %>% 
  select(-Suburb,-Address)
glimpse(housing_all)

h2 = housing_all %>% select(Rooms, Bedroom2, Bathroom)
sum(is.na(housing_all)) #19733
sum(is.na(housing_all$YearBuilt)) #1885

View(h2)
table(housing_all$SellerG)

#data cleaning------------------------------
glimpse(housing_all)
#Fetching categorical columns
cat_col = names(housing_all)[sapply(housing_all, is.character)]; cat_col
source("createDummies.r")
#creating dummies for categorical columns
housing_all = createDummies(housing_all, "Type", 100)
housing_all = createDummies(housing_all, "CouncilArea", 50)
housing_all = createDummies(housing_all, "Method", 50)
housing_all = createDummies(housing_all, "SellerG", 20)
glimpse(housing_all)

#data preparation----------------------------------

for (col in names(housing_all)) {
  if(sum(is.na(housing_all[,col])) > 0 & col != "Price"){
    
    housing_all[is.na(housing_all[, col]), col] = median(housing_all[,col], na.rm = T)
  }
  
}
View(h_test)
h_train = housing_all %>% filter(data == "train") %>% select(-data)
h_test = housing_all %>% filter(data == "test") %>% select(-data, - Price)


# separating train data for testing------------------------

set.seed(13)
s = sample(1:nrow(h_train), 0.8 * nrow(h_train))
h_train1 = h_train[s,]
h_train2 = h_train[-s, ]

#Model building-----------------------------------------------------
fit_h2 = lm(Price~.-CouncilArea_ -Postcode-Bedroom2-Type_u-
              Method_S-Distance-SellerG_Burnham-SellerG_Gary-SellerG_McDonald-
              SellerG_Hodges-SellerG_Peter-SellerG_C21-SellerG_Village-SellerG_Rendina  , 
            data = h_train1)
round(sort(summary(fit_h2)$coefficient[,4]),4)

sort(vif(fit_h2), decreasing = T)[1:5]

step_fit2 = step(fit_h2)
formula(step_fit2)

round(sort(summary(step_fit2)$coefficient[,4]),4)

step_fit2 = update(step_fit2, .~.-SellerG_McGrath, data = h_train1)
round(sort(summary(step_fit2)$coefficient[,4]),4)
#---------------------------------------------

val.pred = predict(step_fit2, newdata = h_train2)
val.error = val.pred - h_train2$Price
rmse.val =  val.error ** 2 %>% mean() %>% sqrt(); rmse.val


train.pred = predict(step_fit2, newdata = h_train1)
train.error = train.pred - h_train1$Price
rmse.train = train.error **2 %>% mean() %>% sqrt(); rmse.train



