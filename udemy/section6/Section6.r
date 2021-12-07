library(readr)


retail<-read_csv("online_retail2.csv")

head(retail)

library(dplyr)

colnames(retail)

### How many units were sold per country?

### %>% : From this do that -chain-

retail %>% select(Description,Country,Quantity) %>% group_by(Country) %>% 
  summarise(Total_sales= sum(Quantity,na.rm = TRUE)) %>% arrange(desc(Total_sales))
summary(retail)

### Lets Invistage 

retail %>% filter(Quantity <= -40000 | Quantity >= 40000)

retail<- retail %>% filter(Quantity >= -40000 & Quantity <= 40000)

summary(retail)

retail %>% filter(Quantity <=0) %>% arrange(Quantity)

retail<- retail %>% filter(Quantity >= 0)

retail %>% filter(Price == -53594.36) %>% select(Description)

retail %>% filter(Price <= -500)

retail<- retail %>%  filter(Price >= 0)

summary(retail)

retail <- retail %>% filter(Description != "Manual" | Description != "AMAZON FEE")

write_csv(retail,"retail_clean.csv")


retail_clean<- read_csv("retail_clean.csv")

head(retail_clean)

length(unique(retail$Invoice))







#### the average  invoice revenue per every country


retail_clean$revenue<- retail_clean$Price *retail_clean$Quantity

retail_clean<- retail_clean %>% mutate(revenue= Price *Quantity)


invoice_value<-retail_clean %>% group_by(Country,Invoice) %>%
  summarise(total_revenue=sum(revenue,na.rm = TRUE))

average_bucket_value<- invoice_value %>% group_by(Country) %>%
  summarise(average_bucket_revenue= mean(total_revenue))

###top 10
average_bucket_value %>% arrange(desc(average_bucket_revenue)) %>% slice(1:10)

retail_clean %>% group_by(Country) %>% summarise(average_revenue=mean(revenue,na.rm=TRUE)) %>% 
  arrange(desc(average_revenue))












### On average , how many items are there per an invoice 

invoice_count<-retail_clean %>% group_by(Invoice) %>% summarise(count=n())

summary_stat<-invoice_count %>% summarise(mean_items= mean(count,na.rm = TRUE),
                                          median= median(count,na.rm=TRUE),
                                          iqr=IQR(count,na.rm=TRUE),
                                          sd=sd(count,na.rm=TRUE),
                                          min=min(count,na.rm=TRUE),
                                          max= max(count,na.rm = TRUE))

summary_stat


#### case_when

retail_clean %>% mutate(uk_or_not= case_when(Country=="United Kingdom"~ TRUE,
                                             Country != "United Kingdom"~ FALSE)) %>% 
  group_by(uk_or_not) %>% summarise(count=n())




#### Joining

sales_Data<- data.frame(skus= c("Shoes_red","Shoes_blue",
                                "Shoes_yellow","Beach_ball",
                                "Sandals_green","Sandals_pink"),
                        average_sales= c(50,60,90,120,110,150))

Stocks_data<- data.frame(skus= c("Shoes_blue",
                                 "Shoes_yellow","Beach_ball","Paddle",
                                 "Swimming_suit"),
                         Current_stock= c(100,150,5,85,70))
###left_join
names(Stocks_data)[1]<-"article"
sales_Data %>% left_join(Stocks_data,by=c("skus"="article"))



###inner_join, what items we have stock for ?

sales_Data %>% inner_join(Stocks_data,by= c("skus"="article"))


#### what items we dont have stock for ?

sales_Data %>% anti_join(Stocks_data,by=c("skus"="article"))




### Full Join , what is everything that we have either sold or in stock?


sales_Data %>% full_join(Stocks_data,by=c("skus"="article"))


#### data restructuring or data reshaping
library(tidyr)
library(dplyr)

### changing from date time to date
retail_clean$date<- as.Date(retail_clean$InvoiceDate)

### sales per day

sales_per_day_clean<- retail_clean %>% group_by(Description,date) %>% 
  summarise(total_sales= sum(Quantity,na.rm = TRUE))


sales_per_day_clean<- sales_per_day_clean %>% filter(Description != "?")

####pivot wider
### shape 1
sales_per_day_clean %>% pivot_wider(id_cols = date,names_from=Description,
                                    values_from= total_sales, values_fill=0)

####shape 2
sales_per_day_clean %>% pivot_wider(id_cols = Description,names_from=date,
                                    values_from= total_sales, values_fill=0)




time_series_like<-sales_per_day_clean %>% pivot_wider(id_cols = Description,names_from=date,
                                                      values_from= total_sales, values_fill=0)


#### pivot longer

time_series_like %>% pivot_longer(cols = -Description,names_to= 'Date',
                                  values_to= 'total_sales')










### separating and pasting columns
retail_clean<- read_csv("retail_clean.csv")

retail_clean


retail_clean<- retail_clean %>% separate(col = InvoiceDate,
                                         into = c("date","time"),
                                         sep = " ")

head(retail_clean)

retail_clean$invoice_date<- paste0(retail_clean$date,retail_clean$time,sep=" ")

retail_clean$invoice_date


####### I want to find the percentiles from 0.1 till 1 of the quantity that
# customers buy from each country

quartiles<- seq(0.1,1,0.1)
retail_clean %>% group_by(Country) %>% 
  summarise(percentile= quantile(Quantity,quartiles)) %>% 
  mutate(name= rep(paste0("pecentile_",quartiles),length(unique(Country)))) %>% 
  pivot_wider(id_cols = Country,names_from=name,values_from=percentile)












































