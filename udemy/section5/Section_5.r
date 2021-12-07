
library(readr)
skus<- read_csv("Story_of_three_Skus.csv")

library(modeest)

head(skus)

###  Calculated the average
average<- apply(skus[,2:4],2,mean)

average
colMeans(skus[,2:4])

#### calculate th median

median1<- apply(skus[,2:4],2,median)


#### calculate the mode

mode1<- apply(skus[,2:4],2,mlv)

### sd

SD<- apply(skus[,2:4],2,sd)
variance<-apply(skus[,2:4],2,var)
range1<- apply(skus[,2:4],2,function(column){max(column)-min(column)})
max1<-apply(skus[,2:4],2,max)
min1<-apply(skus[,2:4],2,min)
IQR<- apply(skus[,2:4],2,IQR)


###Putting it all together
statistical_data<- data.frame(SD=SD,
                              Variance=variance,
                              range=range1,
                              max=max1,
                              min=min1,
                              IQR=IQR,
                              mean=average,
                              mode=mode1,
                              median=median1)

statistical_data$cv<- statistical_data$SD / statistical_data$mean


statistical_data













####detecting outliers 

sales<- c(5,8,10,20,100,2,65,18,32,25,200,9,15)

iqr<-IQR(sales)

first<- quantile(sales,probs = 0.25)
third<- quantile(sales,probs = 0.75)

upper_threshold<- third+ 1.5*iqr
lower_threshold<- first- 1.5*iqr

x<- sales
outlier_function<-function(x){
  iqr<- IQR(x)
  first<- quantile(x,probs = 0.25)
  third<- quantile(x,probs = 0.75)
  
  upper_threshold<- third+ 1.5*iqr
  lower_threshold<- first- 1.5*iqr
  outliers<- list(upper_outliers= x[x> upper_threshold],lower_outliers= x[x< lower_threshold])
  return(outliers)
  
}

outlier_function(sales)





























































