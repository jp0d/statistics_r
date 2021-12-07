

## when we write a hashtag, it means that we do not run the code , only foor explanation.


####doing arithmetic calculations

5+5
5*5
5/5

addittion<- 5+5
addittion
multiplication<- 5*5

multiplication

multiplication/addittion






#### creating a vector

length_class<- c(1.7,1.75,1.78,1.8,1.9)

length_class

name_classs<- c("Mark","Jeffery","Linda","Ross","Leo")

#Bringing the first two elements
name_classs[1:2]

##Bringing the shortest and the tallest Person

name_classs[c(1,5)]






#### creating a list

class_list<- list(Member1= c("Mark",1.7),
                  Member2= c("Jeffery",1.75),
                  Member3=c("Linda",1.78),
                  Member4= c("Ross",1.8),
                  Meember5=c("Leo",1.9))

###Getting an item inside my list
class_list$Member1

###Getting the height of Mark

class_list[[1]][2]

### Getting the name of Member 1
class_list[[1]][1]

class_list





















### source of data :https://www.kaggle.com/mashlyn/online-retail-ii-uci
#### importing data in R
library(readr)
retail_trans<-read_csv("online_retail2.csv")

### to have general info about the structre of the dataset/dataframe

str(retail_trans)

### a couple of lines

head(retail_trans,n=3)

tail(retail_trans,n=3)

### number of row

nrow(retail_trans)

### number of columns

ncol(retail_trans)

#### names of the columns

names(retail_trans)

### changing column names
names(retail_trans)[7]<-"Customer_id"


#### Summary statistics of the data

summary(retail_trans)


#### how to bring one column

retail_trans$Country


##### get unique values

unique(retail_trans$Country)


#### information on categorical variables

table(retail_trans$Country)










#### Selecting Data in A dataframe


##retail_trans[row,column]

### get the first five rows

retail_trans[1:5,]

retail_trans[c(1,2,3,4,5),]

retail_trans[1:5,] == retail_trans[c(1,2,3,4,5),]


five_rows<- retail_trans[1:5,]

###  Subeseting columns
invoice_quantity<- retail_trans[,c(1,4)]

invoice_quantity


retail_trans[,c("Invoice","Quantity")]== invoice_quantity


### subset both columns and rows

retail_trans[1:10,c("Country")]

### how to get the observation of france

unique(retail_trans$Country)

france<-retail_trans[retail_trans$Country== "France",]

### Negative Quantities 

negative_data<-retail_trans[retail_trans$Quantity <= 0 ,]

summary(retail_trans)

###removing negatives
retail_trans$Quantity[retail_trans$Quantity<= 0]<-0

summary(retail_trans)

retail_trans[retail_trans$Quantity < 0 ,]




### operations function

operations_fuction<- function(x,y){
  addition<- x+y
  multiplication<- x*y
  devision <- x/y
  subtract<- x-y
  power1<- x^y
  
  return(c(addition=addition,
           multiplication=multiplication,
           devision=devision,
           subtract=subtract,
           power1= power1))
  
}

operations_fuction(5,2)


unique(retail_trans$Country)

### creating a new column called united kingdom


retail_trans$united_kingdom<- ifelse(retail_trans$Country=="United Kingdom" ,TRUE,FALSE)

table(retail_trans$united_kingdom)


### conditions

haytham<- 32

sarah<- 25

raj <- 21

mike <- 16

brad <- 2
haytham < 32
haytham > 32
haytham <= 32
haytham >= 32

haytham ==32

haytham < 40 & raj > 15 & brad ==2
mike > 15 | haytham < 40

### and adult child function

person <- function(x){
  if(x <=15){
    "child"
  } else if(x <= 20){
    "teenager"
  }else { " adult"}
}

person(haytham)
person(brad)
person(mike)

people<- list(haytham,brad,raj,sarah,mike)
people[3]
length(people)
 for (i in 1: length(people)){
   print(i)
 }

for (i in 1: length(people)){
  print(people[i])
}

###nrow
### ncol
### length


for (i in 1: length(people)){
  print(person(people[i]))
}


head(retail_trans)

first_10_rows<- retail_trans[1:10,c(1,8)]

first_10_rows$ukornot<- ifelse(first_10_rows$Country== "United Kingdom",TRUE,
                               FALSE)

first_10_rows

for (i in 1: nrow(first_10_rows)){
  print(first_10_rows$Invoice[i])
}



uk_function<- function(x){
  if(x== "United Kingdom"){
    TRUE
  } else {
    FALSE
  }
}

uk_function("france")
uk_function("United_Kingdom")


first_10_rows


for (i in 1:nrow(first_10_rows)){
  first_10_rows$uk[i]<-uk_function(first_10_rows$Country[i])
}





