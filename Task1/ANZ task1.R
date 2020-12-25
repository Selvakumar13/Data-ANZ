#calling the libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(tidyr)
library(lubridate)
#Reading and checking data
data<- read_xlsx("C:/Users/DELL/Documents/R/win-library/4.0/data_ANZ.xlsx")
head(data)
summary(data)
colnames(data)
sapply(data,function(x)sum(is.na(x)))
#Creating a Data frame
df<-as.data.frame(data)
df<-df%>%  select(-currency, -country, -bpay_biller_code,-card_present_flag, -merchant_code)
colnames(df)
summary(df)
str(df)
colnames(df)
#Creating factors
df$status<-factor(df$status,levels=c("authorized", "posted"))
df$txn_description<-factor(df$txn_description,levels=c("POS","SALES-POS","PAYMENT","INTER BANK","PAY/SALARY","PHONE BANK"))
df$gender <- factor(df$gender, levels=c("M","F"))
df$movement<- factor(df$movement,levels = c('debit','credit'))
#Extracting the various time variables.
df$date<- as.Date(df$date,format="%y%m%d")
df$month<-month(df$date,label=F)
df$extraction<-ymd_hms(df$extraction)
df$hour<-hour(df$extraction)
df$weekdays<-weekdays(df$date)
#Creating new columns and tidying the data
df <- df %>% separate(long_lat, into = c("longitude","latitude"), sep=6)
df <- df %>% separate(merchant_long_lat, into = c("m_longitude","m_latitude"), sep=6)

#Comparing male and female customers
avg<-df %>% group_by(gender)%>% summarise(mean(amount))
avg
#Creating new columns
gaps <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
ageterms <- c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85+")

setDT(df)[ , agegroups := cut(age, 
                                breaks = gaps, 
                                right = FALSE, 
                                labels = ageterms)]
head(df$agegroups)


df<- mutate(df,transaction= ifelse(txn_description=="INTER BANK" | txn_description=="PAYMENT" |
                                          txn_description=="PHONE BANK" | txn_description=="POS" | 
                                          txn_description == "SALES-POS", df$amount,0))
IQR = 42.78 - 12.77
UpperLimit = 1.5 *(IQR) + 42.78

boxplot(df$transaction)

df<- df %>% mutate(transaction_vol=
                     case_when(transaction >= 0 & transaction <= 12.87 ~ "Low",
                               transaction > 12.87 & transaction <= 24.44 ~ "Low-Medium",
                               transaction > 24.44 & transaction <= 48.72 ~ "Medium",
                               transaction > 48.72 & transaction <= 94.495 ~ "Medium-High",
                               transaction > 94.495 & transaction <= 7081.09 ~ "High"))
df$transaction_vol<-factor(df$transaction_vol)

Salary_Table <- df %>% 
  group_by(customer_id) %>% 
  filter(txn_description=="PAY/SALARY") %>%   
  mutate(monthlysalary = (sum(amount))/3) %>%  
  select(account, customer_id, merchant_id, txn_description,
         first_name, gender,balance, date, age, merchant_suburb,
         amount, agegroups, monthlysalary)
summary(Salary_Table)

Salary_Table <- Salary_Table %>%
  mutate(month = 
           case_when(date >= "2018-08-01" & date <= "2018-08-31" ~ "August",
                     date >= "2018-09-01" & date <= "2018-09-30" ~ "September",
                     date >= "2018-10-01" & date <= "2018-10-31" ~ "October"))
Salary_Table$month <- factor(Salary_Table$month)

summary(df$txn_description)
df$txn_description

IQR_Age = 40 - 22
UpperLimit_Age = 1.5 *(IQR) + 22
UpperLimit_Age

boxplot(Salary_Table$age)
plot(Salary_Table$monthlysalary~Salary_Table$agegroups)
boxplot(Salary_Table$monthlysalary)

cap <- function(x){
  quantiles <- quantile( x, c(.05, 0.25, 0.75, .95 ) )
  x[ x < quantiles[2] - 1.5*IQR(x) ] <- quantiles[1]
  x[ x > quantiles[3] + 1.5*IQR(x) ] <- quantiles[4]
  x
}
Salary_Table$monthlysalary <-  Salary_Table$monthlysalary  %>%  cap()
boxplot(Salary_Table$monthlysalary)

#Data visualisation
hist(Salary_Table$monthlysalary, main="Original Salary Histogram")

df2<-df%>% group_by(customer_id)%>%summarise(mon_avg_col=round(n()/3,0))
hist(df2$mon_avg_col,xlab="MOnthly transaction",ylab = "No.of customers",main = 'Customers monthly transaction volume')

df3 <- df %>%
  select(date,weekdays) %>%
  group_by(date,weekdays) %>%
  summarise(daily_avg_vol = n()) %>%
  group_by(weekdays) %>%
  summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
df3$weekdays <- factor(df3$weekdays, levels=c( "Monday","Tuesday","Wednesday",
                                             "Thursday","Friday","Saturday","Sunday"))
ggplot(df3,aes(x=weekdays, y=avg_vol)) +geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by weekday') +
  labs(x='Weekday',y='Transaction volume')

df4 <- df %>%
  select(date,hour) %>%
  group_by(date,hour) %>%
  summarize(trans_vol=n()) %>%
  group_by(hour) %>%
  summarize(trans_vol_per_hr = mean(trans_vol,na.rm=TRUE))
ggplot(df4,aes(x=hour,y=trans_vol_per_hr))+geom_point()+geom_line(aes(group = 1))+
  ggtitle('Average transaction volume by hour') +
  labs(x='Hour',y='Transaction volume') + expand_limits( y = 0)



