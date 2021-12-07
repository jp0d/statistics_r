
cars<- read_csv("cars.csv")

str(cars)  
head(cars)

#How many Rows are in the cars dataset?
  
nrow(cars)

#How many Columns are in the car's data set?

ncol(cars)
#How many unique numbers of cylinders we have in the cars dataset?
colnames(cars)
unique(cars$cylenders)
table(cars$cylenders)
#what is the average horsepower of cars?

summary(cars)

#what is the maximum horsepower?
summary(cars$horsepower)
#what is the maximum price of a car?

summary(cars$Price)
#change the name of the column "name" to "car name"
colnames(cars)
names(cars)[1]<-"car name"
## how many sports car we have in the dataset ?
cars

table(cars$sports_car)











###import the cars dataset

library(readr)

cars<- read_csv("cars.csv")

#### head 

head(cars)

colnames(cars)
#subsetting

car_pricing<-cars[,c(1,9)]




price_category<- function(price){
  
  if (price < 20000){
    "Budget Car"
  } else if (price <=35000){
    "Suitable Car"
  }else {
    "Expensive Car"
  }
}


#### testing 

price_category(50000)



###for loops 

for (i in 1: nrow(car_pricing)){
  
  car_pricing$category[i]<- price_category(car_pricing$Price[i])
  
}

car_pricing

##### table 

table(car_pricing$category)











