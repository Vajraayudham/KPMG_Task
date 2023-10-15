library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(xlsx)
library(anytime)
library(eeptools)


#Read particular sheet from excel file using read_xlsx function from readxl Package
cust_transaction <- read_xlsx("/cloud/project/kpmgT.xlsx",sheet = 'Transactions')
cust_demographic <- read_csv("/cloud/project/CustomerDemographic.csv")
cust_address     <- read_xlsx("/cloud/project/kpmgT.xlsx",sheet = 'CustomerAddress')
cust_new_list    <- read_xlsx("/cloud/project/kpmgT.xlsx",sheet = 'NewCustomerList')

#Remove rows which contain null or empty values
cust_transaction <- na.omit(cust_transaction)
cust_demographic <- na.omit(cust_demographic)
cust_address     <- na.omit(cust_address)
cust_new_list    <- na.omit(cust_new_list)

#Remove rows which column containing n/a value
cust_demographic <- subset(cust_demographic,job_industry_category != 'n/a')
cust_new_list <- subset(cust_new_list,job_industry_category != 'n/a')
View(cust_demographic)

#replacing inconsistent data of address dataset
cust_address <- cust_address %>% 
           mutate(state= replace(state, state == "New South Wales","NSW"))
cust_address <- cust_address %>% 
  mutate(state= replace(state, state == "Victoria","VIC"))
#cust_address cleaned 

#cust_demographic start
#replacing inconsistent data of address dataset
cust_demographic <- cust_demographic %>% 
  mutate(gender= replace(gender, gender == "F","Female"))
cust_demographic <- cust_demographic %>% 
  mutate(gender= replace(gender, gender == "Femal","Female"))
cust_demographic <- cust_demographic %>% 
  mutate(gender= replace(gender, gender == "M","Male"))

#droping default column contains unrelated data
cust_demographic <- cust_demographic %>% select(-default)
View(head(cust_demographic))

#converting dob column to specified format
cust_demographic$DOB <- as.character(cust_demographic$DOB)
cust_demographic$DOB <- mdy(cust_demographic$DOB)
View(head(cust_demographic))

#calculating age from DOB column 
cust_demographic$Age <- age_calc(cust_demographic$DOB, units = "years")
cust_demographic$Age <- round(cust_demographic$Age)

#Viewing unique values from gender column
View(unique(cust_demographic$gender))
#cust_demographic cleaned 

# cust_transaction start cleaning
View(cust_transaction)
cust_transaction <- subset(cust_transaction,cust_transaction$order_status == "Approved")

abc <- cust_transaction
glimpse(cust_transaction)
View(abc)

abc$profit <- abc$list_price - abc$standard_cost

snapshot_date <- lubridate::ymd(max(abc$transaction_date) + lubridate::days(1))

View(snapshot_date)

rmf <- abc %>%
  group_by(customer_id) %>%
  summarise(
    recency = as.POSIXct(snapshot_date) - max(transaction_date),
    frequency = n(),
    monetary_value = sum(profit)
  )

View(rmf)

rmf$recency <- as.numeric(rmf$recency)

rmf$f_quartile <- cut(rmf$frequency, 4, labels = c("1", "2", "3", "4"))
rmf$m_quartile <- cut(rmf$monetary_value, 4, labels = c("1", "2", "3", "4"))
rmf$r_quartile <- cut(rmf$recency, 4, labels = c("4", "3", "2", "1"))
View(rmf)
glimpse(rmf)

rmf$RMF_Score <- paste0(rmf$r_quartile,rmf$f_quartile,rmf$m_quartile) 
rmf$RMF_Score <- as.numeric(rmf$RMF_Score,type ="float16")
  
quantiles <- c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
quantiles <- as.double(quantiles,type= "float16")
rmf %>%
  reframe(RMF_Score_quantiles = quantile(rmf$RMF_Score, quantiles),quantiles)


# Sort the rmf data frame by the RMF_Score variable in descending order

rmf_level_r <- function(df) {
  if (df$RMF_Score == 444) {
    return('Platinum Customer')
  } else if ((df$RMF_Score >= 433) & (df$RMF_Score < 444)) {
    return('Very Loyal')
  } else if ((df$RMF_Score >= 413) & (df$RMF_Score < 433)) {
    return('Becoming Loyal')
  } else if ((df$RMF_Score >= 344) & (df$RMF_Score < 413)) {
    return('Recent Customer')
  } else if ((df$RMF_Score >= 324) & (df$RMF_Score < 344)) {
    return('Potential Customer')
  } else if ((df$RMF_Score >= 311) & (df$RMF_Score < 324)) {
    return('Late Bloomer')
  } else if ((df$RMF_Score >= 233) & (df$RMF_Score < 311)) {
    return('Losing Customer')
  } else if ((df$RMF_Score >= 212) & (df$RMF_Score < 233)) {
    return('High Risk Customer')
  } else if ((df$RMF_Score >= 132) & (df$RMF_Score < 212)) {
    return('Almost Lost Customer')
  } else if ((df$RMF_Score >= 112) & (df$RMF_Score < 132)) {
    return('Evasive Customer')
  } else {
    return('Lost Customer')
  }
}

# Create a new variable called rmf_Level using the dplyr::rowwise() function
rmf <- dplyr::rowwise(rmf) %>%
  mutate(rmf_Level = rmf_level_r(data_frame(RMF_Score)))

# Sort the rmf data frame by the RMF_Score variable in descending order
rmf <- dplyr::arrange(rmf, desc(RMF_Score))
# Head of the rmf data frame
View(rmf)


rmf_agg <- rmf %>% 
    group_by(rmf_Level) %>% 
  summarise(
    Recency_Mean = mean(recency),
    Frequency_Mean = mean(frequency),
    Monetary_Mean = mean(monetary_value),
    Monetary_Count = n()
  ) %>%
  dplyr::mutate_if(is.numeric, round, 1)
View(rmf_agg)
write_csv(rmf_agg,"rmf_agg.csv")
rfm_agg <- data.frame(Count = c(10, 20, 30, 40, 50),
                      RecencyMean = c(10.0, 20.0, 30.0, 40.0, 50.0),
                      FrequencyMean = c(1.0, 2.0, 3.0, 4.0, 5.0),
                      MonetaryMean = c(100.0, 200.0, 300.0, 400.0, 500.0))

# Rename the columns
View(head(rmf))
target <- rmf %>% head(1000) %>% select(customer_id,RMF_Score,rmf_Level)
View(target)
target_customer <-rmf[1:1000,1:5]
View(rmf[1:1000,1:5])
target$RMF_Score <-as.integer(target$RMF_Score) 

write_csv(target ,"target_customer.csv")

write_csv(cust_demographic ,"customer_demographic_cleaned.csv")
write_csv(cust_transaction ,"customer_transaction_cleaned.csv") 
write_csv(cust_address ,"customer_address_cleaned.csv")  
  
