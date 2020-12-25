#calling the libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(data.table)
library(tidyr)
library(modelr)
library(lubridate)
#Reading and checking data
data<- read_xlsx("C:/Users/DELL/Documents/R/win-library/4.0/ANZ synthesised transaction dataset.xlsx")
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

df<- mutate(df,transaction= ifelse(txn_description=="INTER BANK" | txn_description=="PAYMENT" |
                                     txn_description=="PHONE BANK" | txn_description=="POS" | 
                                     txn_description == "SALES-POS", df$amount,0))

df<- df %>% mutate(transaction_vol=
                     case_when(transaction >= 0 & transaction <= 12.87 ~ "Low",
                               transaction > 12.87 & transaction <= 24.44 ~ "Low-Medium",
                               transaction > 24.44 & transaction <= 48.72 ~ "Medium",
                               transaction > 48.72 & transaction <= 94.495 ~ "Medium-High",
                               transaction > 94.495 & transaction <= 7081.09 ~ "High"))
df$transaction_vol<-factor(df$transaction_vol)

df_sal = data.frame(customer_id= unique(df$customer_id)) #create a data frame to store result
# create a mode function that will be used to find out what is the salary payment frequency
Md <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Salary payment frequency

for (i in seq(nrow(df_sal))){
  trans_data <- df[df$customer_id == df_sal$customer_id[i]
                   & df$txn_description=='PAY/SALARY',c("amount","date")] %>%
    group_by(date) %>%
    summarise(amount = sum(amount))
  total_s <- sum(trans_data$amount)
  count = dim(trans_data)[1]
  if ( count == 0){
    df_sal$freq[i] = NA
    df_sal$level[i] = NA
  } else {
    s=c()
    lvl = c()
    for (j in seq(count-1)){
      s = c(s,(trans_data$date[j+1]-trans_data$date[j]))
      lvl = c(lvl,trans_data$amount[j])
    }
    lvl = c(lvl,tail(trans_data$amount,n=1))
    df_sal$freq[i] = Md(s)
    df_sal$level[i] = Md(lvl)
  }
}
df_sal$annual_salary= df_sal$level / df_sal$freq *365.25

df_sal$age<-ifelse(df_sal$customer_id%in%df$customer_id,df$age)

# visualise the distribution of customers' annual salary
hist(df_sal$annual_salary[!is.na(df_sal$annual_salary)],breaks=c(seq(28000,140000,by = 10000)),
     main = "Histogram of customers' annual salary", xlab= 'Income($)')

plt<-ggplot(data=df_sal,aes(x=annual_salary,y=age))
pl<-ggplot(data=df_sal,aes(x=annual_salary))
plt2<-plt + geom_point()
pl2<-pl+ geom_histogram(aes(fill)) 
plt2
pl2
# create a dataframe to store relevant features for customers
df_cst <-df%>% 
  select (customer_id,gender,age,amount,date,balance) %>%
  group_by(customer_id) %>%
  mutate(avg_no_weekly_trans= round(7*n()/length(unique(df$date)),0),max_amt = max(amount),
         no_large_trans = sum(amount>100),

                  use_no_day=length(unique(date)),
         avg_trans_amt = mean(amount, na.rm =TRUE),
         med_bal = median(balance,na.rm=TRUE)) %>%
  select(-c("amount","date","balance")) %>%
  unique()

df_cst$age_below20 <- ifelse(df_cst$age<20,1,0)
df_cst$age_btw20n40 <- ifelse(df_cst$age>=20 & df_cst$age <40,1,0)
df_cst$age_btw40n60 <- ifelse(df_cst$age>=40 & df_cst$age <60,1,0)


df_reg <-df %>%
  group_by(customer_id,merchant_state) %>%
  summarize(trans_count=n()) %>%
  group_by(customer_id) %>%
  mutate (no_state = n()) %>%
  filter(trans_count == max(trans_count))

n_occur = data.frame(table(df_reg$customer_id))
cus_id_rep = n_occur$Var1[n_occur$Freq > 1]
state_by_cust_no <- rev(names(sort(table(df_reg$merchant_state),rev = TRUE)))
t = data.frame(customer_id = cus_id_rep)
for (i in seq(length(cus_id_rep))){
  s = df_reg$merchant_state[df_reg$customer_id == cus_id_rep[i]]
  for (state in state_by_cust_no){
    if (state %in% s){
      t[i,2] = state
      break
    }
  }
}
df_reg <- df_reg[!(df_reg$customer_id %in% cus_id_rep), c(1,2)] %>%
  as.data.frame() %>%
  rbind(t) %>%
  rename( State = merchant_state)

df_cst <- df_cst %>% merge(df_sal) %>% 
  merge(df_reg)

df_cus_attr <- df_cst %>%
  select("gender","annual_salary","age","avg_no_weekly_trans","max_amt",
         "no_large_trans", "use_no_day","avg_trans_amt","med_bal","State")
plot(df_cus_attr)

#regression model

fit_first <- lm(annual_salary ~.-customer_id - level-freq,data=df_cst)
summary(fit_first)
MASS::stepAIC(fit_first)

fit_scnd <- lm(formula = annual_salary ~ age + avg_trans_amt + med_bal +
             age_below20 + age_btw20n40 + age_btw40n60, data = df_cst)
summary(fit_scnd)

rmse(fit_scnd,df_cst)

#residual plot
plot(fit_scnd$residuals, ylab = 'Residual')

