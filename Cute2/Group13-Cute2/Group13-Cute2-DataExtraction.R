# Clear the environment
rm(list = ls(all=T))

# Setup working directory
setwd("/Users/nirdoshagarwal/Documents/INSOFE/CUTe/CSE7302c_CUTe01_Exam-Files/data")

# Loadup all necessary libraries used in code
library(lubridate)
library(dplyr)
library(sqldf)
library(MASS)
library(caret)
library(DMwR)

############ stg_bgdt_cust_ownr.txt ############
# Read file stg_bgdt_cust_ownr.txt and store data in data frame
# Using quote parameter to not end file reading on quotes within the data
cust_ownr <- read.csv("stg_bgdt_cust_ownr.txt", header = T, sep = "\t", fill = TRUE, quote = "")
str(cust_ownr)

# Converting date into date time yyyy-mm-dd format
cust_ownr$X_NOMIN_DT = as.Date(mdy_hm(cust_ownr$X_NOMIN_DT))
max(cust_ownr$X_NOMIN_DT)
min(cust_ownr$X_NOMIN_DT)

# Checking for NA in date
sum(is.na(cust_ownr$X_NOMIN_DT))

# Filtering data based on condition given
# 2012-12-24 < X_NOMIN_DT < 2013-01-27 and
# X_EDW_INTEGRATION_ID = kutti or PAD2
cust_owner_filtered <- cust_ownr[((cust_ownr$X_NOMIN_DT > "2012-12-24" & cust_ownr$X_NOMIN_DT < "2013-01-27") 
                                & (cust_ownr$X_EDW_INTEGRATION_ID == "kutti" | 
                                     cust_ownr$X_EDW_INTEGRATION_ID == "PAD2")),
                                c('CONTACT_WID', 'X_NOMIN_DT', 'X_CNTRY')]

# Renaming column names as per names given in sheet
colnames(cust_owner_filtered) <- c('CONTACT_WID','Nomination Date','Country')
summary(cust_owner_filtered)
sum(is.na(cust_owner_filtered))

# Taking out unique of complete data set
unique_cust_ownr_filtered <- unique(cust_owner_filtered)

# Unique data set again has multiple rows for same CONTACT_WID
# because single CONTACT_WID has multiple Nomination Date
# Getting unique rows in final dataframe by taking min of Nomination Date for each CONTACT_WID
# Using sqldf package getting minimum of Nomination Date for each CONTACT_WID and storing in a data frame
cust_final <- sqldf("select CONTACT_WID, min([Nomination Date]) as NominationDate, Country from unique_cust_ownr_filtered group by CONTACT_WID")
cust_final$NominationDate <- as.Date(cust_final$NominationDate, origin = '1970-01-01')
str(cust_final)

# Removing unnecessary data frames to clean up memory on system
rm(cust_ownr)
rm(cust_owner_filtered)
rm(unique_cust_ownr_filtered)

############ stg_bgdt_cust_chld.txt ############
# Read file stg_bgdt_cust_chld.txt and store data in data frame
cust_chld <- read.csv("stg_bgdt_cust_chld.txt", header = T, sep = "\t", fill = TRUE, quote = "")
str(cust_chld)
summary(cust_chld)

# There are NAs in data frame. Omitting them from the data frame.
cust_chld <- na.omit(cust_chld)

# Column ordering is not correct in the original file. Renaming the columns as per domain understanding.
colnames(cust_chld) <- c('CONTACT_WID','CHILD_ROW_WID','PR_HOUSEHOLD_WID','X_CRM_CUST_KEY','CRM_CHLD_KEY','CHILD_FIRST_NAME','CHILD_BIRTH_DATE','CHILD_AGE','SEX_MF_CODE','X_GRADE','CHILD_CREATION_DATE')

# Getting maximum of age of all children in a household by using aggregate function
max_age <- aggregate(cust_chld$CHILD_AGE, by = list(cust_chld$PR_HOUSEHOLD_WID), max)

# Renaming columns
colnames(max_age) <- c('PR_HOUSEHOLD_WID','MAX_AGE')

# Getting minimum of age of all children in a household by using aggregate function
min_age <- aggregate(cust_chld$CHILD_AGE, by = list(cust_chld$PR_HOUSEHOLD_WID), min)
colnames(min_age) <- c('PR_HOUSEHOLD_WID','MIN_AGE')

# Getting total number of children per household by using sql group by
noofchildren <- sqldf("select PR_HOUSEHOLD_WID, count(CRM_CHLD_KEY) as NumHouseChildren from cust_chld group by PR_HOUSEHOLD_WID")

# Getting total number of male children per household by using sql group by
noofMChildren <- sqldf("select PR_HOUSEHOLD_WID, count(SEX_MF_CODE) as NumMaleChildrenHousehold from cust_chld where SEX_MF_CODE == 'M' group by PR_HOUSEHOLD_WID")

# Getting total number of female children per household by using sql group by
noofFChildren <- sqldf("select PR_HOUSEHOLD_WID, count(SEX_MF_CODE) as NumFemaleChildrenHousehold from cust_chld where SEX_MF_CODE == 'F' group by PR_HOUSEHOLD_WID")

# Merging max_age data frame into cust_chld dataframe
cust_chld <- merge(cust_chld, max_age, by = 'PR_HOUSEHOLD_WID')

# Merging min_age data frame into cust_chld dataframe
cust_chld <- merge(cust_chld, min_age, by = 'PR_HOUSEHOLD_WID')

# Calculating range of age by subtracting MIN_AGE from MAX_AGE and storing in new column
cust_chld$CHILD_AGE_RANGE <- cust_chld$MAX_AGE - cust_chld$MIN_AGE

# Merging noofchildren data frame into cust_chld dataframe
cust_chld <- merge(cust_chld, noofchildren, by = 'PR_HOUSEHOLD_WID', all.x = T)

# Merging noofMchildren data frame into cust_chld dataframe
cust_chld <- merge(cust_chld, noofMChildren, by = 'PR_HOUSEHOLD_WID', all.x = T)

# Merging noofFchildren data frame into cust_chld dataframe
cust_chld <- merge(cust_chld, noofFChildren, by = 'PR_HOUSEHOLD_WID', all.x = T)

# There are few CONTACT_WID where Male or Female children are not there
# hence noofMchildren or noofFchildren column has NA for them
# Converting those NAs to 0 as here NA literally means 0 children of that group
summary(cust_chld)
cust_chld[is.na(cust_chld)] <- 0
summary(cust_chld)

# Filtering only few essential columns from the cust_chld data frame
cust_chld_filtered <- subset(cust_chld, select = c('CONTACT_WID','MIN_AGE','MAX_AGE','CHILD_AGE_RANGE','NumHouseChildren','NumMaleChildrenHousehold','NumFemaleChildrenHousehold'))

# Taking out unique of all columns
unique_cust_chld_filtered <- unique(cust_chld_filtered)

# Merging this unique_cust_chld_filtered data frame containing child information
# into original data frame cust_final
cust_final <- merge(cust_final, unique_cust_chld_filtered, by = 'CONTACT_WID', all.x = T)

# Removing unnecessary data frames to clean up memory on system
rm(max_age)
rm(min_age)
rm(noofchildren)
rm(noofMChildren)
rm(noofFChildren)
rm(cust_chld)
rm(cust_chld_filtered)

############ stg_bgdt_cust_purc_app.txt and stg_bgdt_cust_purc_lf.txt ############
# Read file stg_bgdt_cust_purc_app.txt and store data in data frame
cust_purc_app <- read.csv("stg_bgdt_cust_purc_app.txt", header = T, sep = "\t", fill = TRUE, quote = "")
str(cust_purc_app)

# Converting date into date time yyyy-mm-dd format
cust_purc_app$TRANSACTION_DATE = as.Date(mdy_hm(cust_purc_app$TRANSACTION_DATE))
str(cust_purc_app)
summary(cust_purc_app)

# Checking if there are any NAs in the data frame
sum(is.na(cust_purc_app))

# Read file stg_bgdt_cust_purc_lf.txt and store data in data frame
cust_purc_lf <- read.csv("stg_bgdt_cust_purc_lf.txt", header = T,sep = "\t", fill = TRUE, quote = "")
str(cust_purc_lf)
summary(cust_purc_lf)

# Converting date into date time yyyy-mm-dd format
cust_purc_lf$TRANSACTION_DT = as.Date(dmy_hm(cust_purc_lf$TRANSACTION_DT))
str(cust_purc_lf)

# Filtering data based on condition given
# TRANSACTION_DATE > 2011-12-25
cust_purc_app_filtered <- cust_purc_app[cust_purc_app$TRANSACTION_DATE > '2011-12-25', c('CONTACT_WID','TRANSACTION_DATE','UNITS','AMOUNT_USD','ITEM_NUMBER','CHANNEL_DESCRIPTION','SOURCE_OF_PURCHASE')]

# Filtering data based on condition given
# TRANSACTION_DATE > 2011-12-25
cust_purc_lf_filtered <- cust_purc_lf[cust_purc_lf$TRANSACTION_DT > '2011-12-25', c('CONTACT_WID','TRANSACTION_DT','UNITS','AMOUNT')]

# Converting date to character and store in different variable 
# This is needed to query using sqldf
cust_purc_lf_filtered$TRANSACTION_DT1 <- as.character(cust_purc_lf_filtered$TRANSACTION_DT)
cust_purc_app_filtered$TRANSACTION_DATE1 <- as.character(cust_purc_app_filtered$TRANSACTION_DATE)

# Removing unnecessary data frames to clean up memory on system
rm(cust_purc_app)
rm(cust_purc_lf)

# Checking if there are any NAs in the data frame
sum(is.na(cust_purc_app_filtered))
sum(is.na(cust_purc_lf_filtered))

############ Function to get Units
GetSumOfUnits <- function(date, tableName1, tableName2) {
  if (date != '') {
    # Condition if date is blank. This is when we are getting aggregation on whole data
    x1 <- sprintf("select CONTACT_WID, sum(Units) as Units from '%s' where TRANSACTION_DATE1 < '%s' group by CONTACT_WID",tableName1,date)
    xdf1 <- sqldf(x1)
    y1 <- sprintf("select CONTACT_WID, sum(Units) as Units from '%s' where TRANSACTION_DT1 < '%s' group by CONTACT_WID",tableName2,date)
    ydf1 <- sqldf(y1)
  }
  else {
    # Else when date is not blank. This is when we are getting aggregation based on dates
    x1 <- sprintf("select CONTACT_WID, sum(Units) as Units from '%s' group by CONTACT_WID",tableName1)
    xdf1 <- sqldf(x1)
    y1 <- sprintf("select CONTACT_WID, sum(Units) as Units from '%s' group by CONTACT_WID",tableName2)
    ydf1 <- sqldf(y1)
  }
  # Merging the 2 data frames which came as above aggregations
  units_new <- merge(x = xdf1, y = ydf1, by='CONTACT_WID', all = T)
  # Setting NAs to 0 because NA in units mean no unit purchased
  units_new[is.na(units_new)] <- 0
  # Adding units from both data sources cust_purc_app_filtered and cust_purc_lf_filtered
  units_new$UNITS <- units_new$Units.x + units_new$Units.y
  # After adding units, individual columns become redundant.
  # Removing unnecessary columns from data frame
  units_new$Units.x <- NULL
  units_new$Units.y <- NULL
  # Return new data_frame
  return(units_new)
}

# Getting Units data by calling function GetSumOfUnits with blank date
units_df <- GetSumOfUnits('', 'cust_purc_app_filtered', 'cust_purc_lf_filtered')
# Renaming column
colnames(units_df)[ncol(units_df)] <- 'Units'
# Getting Units7 data by calling function GetSumOfUnits
units_df <- merge(units_df, GetSumOfUnits('2012-01-02', 'cust_purc_app_filtered', 'cust_purc_lf_filtered'),
                  by = 'CONTACT_WID', all.x = T)
# Renaming column
colnames(units_df)[ncol(units_df)] <- 'Units7'
# Getting Units30 data by calling function GetSumOfUnits
units_df <- merge(units_df, GetSumOfUnits('2012-01-26', 'cust_purc_app_filtered', 'cust_purc_lf_filtered'),
                  by = 'CONTACT_WID', all.x = T)
# Renaming column
colnames(units_df)[ncol(units_df)] <- 'Units30'
# Getting Units90 data by calling function GetSumOfUnits
units_df <- merge(units_df, GetSumOfUnits('2012-03-26', 'cust_purc_app_filtered', 'cust_purc_lf_filtered'),
                  by = 'CONTACT_WID', all.x = T)
# Renaming column
colnames(units_df)[ncol(units_df)] <- 'Units90'
# Getting Units180 data by calling function GetSumOfUnits
units_df <- merge(units_df, GetSumOfUnits('2012-06-23', 'cust_purc_app_filtered', 'cust_purc_lf_filtered'),
                  by = 'CONTACT_WID', all.x = T)
# Renaming column
colnames(units_df)[ncol(units_df)] <- 'Units180'
# Getting Units360 data by calling function GetSumOfUnits
units_df <- merge(units_df, GetSumOfUnits('2012-12-26', 'cust_purc_app_filtered', 'cust_purc_lf_filtered'),
                  by = 'CONTACT_WID', all.x = T)
# Renaming column
colnames(units_df)[ncol(units_df)] <- 'Units360'

# Merging this units_df data frame containing Units information
# into original data frame cust_final
cust_final1 <- merge(cust_final, units_df, by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final1[is.na(cust_final1)] <- 0

# Removing unnecessary data frames to clean up memory on system
rm(units_df)

# Get Max Transaction date from cust_purc_lf_filtered data frame using aggregate function
cust_purc_lf_max_tdate <- aggregate(cust_purc_lf_filtered$TRANSACTION_DT, by=list(cust_purc_lf_filtered$CONTACT_WID), max)

# Renaming column name as per name given in sheet
colnames(cust_purc_lf_max_tdate) <- c('CONTACT_WID','OveralllastTransaction')
summary(cust_purc_lf_max_tdate)

# Merging this cust_purc_lf_max_tdate data frame containing OveralllastTransaction information
# into original data frame
cust_final2 <- merge(cust_final1, cust_purc_lf_max_tdate, by='CONTACT_WID', all.x = T)
summary(cust_final2)

# Get TenureDays by subtracting OveralllastTransaction with NominationDate
cust_final2$TenureDays <- cust_final2$OveralllastTransaction - cust_final2$NominationDate

# Replacing NAs with 0. This means someone hasnt done any Transaction at all
cust_final2$TenureDays[is.na(cust_final2$TenureDays)] <- 0

# Take subset of only CONTACT_WID, OveralllastTransaction and TenureDays.
# We will merge this data later in final dataframe
tenureDays_df <- subset(cust_final2, select = c('CONTACT_WID','OveralllastTransaction','TenureDays'))

# Removing unnecessary data frames to clean up memory on system
rm(cust_purc_lf_max_tdate)

############ stg_bgdt_cust_gam_actv.csv ############
# Read file stg_bgdt_cust_gam_actv.csv and store data in data frame
cust_gam_actv <- read.csv("stg_bgdt_cust_gam_actv.csv", header = T, sep = ",")
str(cust_gam_actv)

# Converting date into date time yyyy-mm-dd format
cust_gam_actv$TITLE_NOMIN_DT <- as.Date(cust_gam_actv$TITLE_NOMIN_DT, "%d-%B-%y")

# Converting date to character and store in different variable 
# This is needed to query using sqldf
cust_gam_actv$TITLE_NOMIN_DT1 <- as.character(cust_gam_actv$TITLE_NOMIN_DT)

# Filtering data based on condition given
# TITLE_NOMIN_DT < 2011-12-25
cust_gam_actv_filtered <- cust_gam_actv[cust_gam_actv$TITLE_NOMIN_DT > '2011-12-25',]

# Removing unnecessary data frames to clean up memory on system
rm(cust_gam_actv)

############ Function to get Frequency
GetFrequency <- function(date, tableName1, tableName2, tableName3, colName1, colName2, colName3) {
  if (date != '') {
    # Condition if date is blank. This is when we are getting aggregation on whole data
    x1 <- sprintf("select CONTACT_WID, count(TRANSACTION_DT1) as %s from '%s' where TRANSACTION_DT1 < '%s' group by CONTACT_WID",colName1,tableName1,date)
    xdf1 <- sqldf(x1)
    y1 <- sprintf("select CONTACT_WID, count(TRANSACTION_DATE1) as %s from '%s' where TRANSACTION_DATE1 < '%s' group by CONTACT_WID",colName2,tableName2,date)
    ydf1 <- sqldf(y1)
    z1 <- sprintf("select CONTACT_WID, sum(ATMP_CNT) as %s from '%s' where TITLE_NOMIN_DT1 < '%s' group by CONTACT_WID",colName3,tableName3,date)
    zdf1 <- sqldf(z1)
  }
  else {
    # Else when date is not blank. This is when we are getting aggregation based on dates
    x1 <- sprintf("select CONTACT_WID, count(TRANSACTION_DT1) as %s from '%s' group by CONTACT_WID",colName1,tableName1)
    xdf1 <- sqldf(x1)
    y1 <- sprintf("select CONTACT_WID, count(TRANSACTION_DATE1) as %s from '%s' group by CONTACT_WID",colName2,tableName2)
    ydf1 <- sqldf(y1)
    z1 <- sprintf("select CONTACT_WID, sum(ATMP_CNT) as %s from '%s' group by CONTACT_WID",colName3,tableName3)
    zdf1 <- sqldf(z1)
  }
  # Merging the 2 data frames which came as above aggregations
  freq_new <- merge(x = xdf1, y = ydf1, by='CONTACT_WID', all = T)
  # Setting NAs to 0 because NA in units mean no unit purchased
  freq_new[is.na(freq_new)] <- 0
  # Merging the 3rd data frame which came as above aggregations
  freq_new <- merge(x = freq_new, y = zdf1, by='CONTACT_WID', all = T)
  # Setting NAs to 0 because NA in units mean no unit purchased
  freq_new[is.na(freq_new)] <- 0
  return(freq_new)
}

# Getting Frequency data by calling function GetFrequency with blank date
freq_df <- GetFrequency('','cust_purc_lf_filtered','cust_purc_app_filtered',
                     'cust_gam_actv_filtered','FrequencyLF','FrequencyApp','FreqGamePlay')
# Getting Frequency7 data by calling function GetFrequency
freq_df <- merge(freq_df, GetFrequency('2012-01-02','cust_purc_lf_filtered','cust_purc_app_filtered',
                      'cust_gam_actv_filtered','FrequencyLF7','FrequencyApp7','FreqGamePlay7'),
                      by = 'CONTACT_WID', all.x = T)
# Getting Frequency30 data by calling function GetFrequency
freq_df <- merge(freq_df, GetFrequency('2012-01-26','cust_purc_lf_filtered','cust_purc_app_filtered',
                      'cust_gam_actv_filtered','FrequencyLF30','FrequencyApp30','FreqGamePlay30'),
                      by = 'CONTACT_WID', all.x = T)
# Getting Frequency90 data by calling function GetFrequency
freq_df <- merge(freq_df, GetFrequency('2012-03-26','cust_purc_lf_filtered','cust_purc_app_filtered',
                      'cust_gam_actv_filtered','FrequencyLF90','FrequencyApp90','FreqGamePlay90'),
                      by = 'CONTACT_WID', all.x = T)
# Getting Frequency180 data by calling function GetFrequency
freq_df <- merge(freq_df, GetFrequency('2012-06-23','cust_purc_lf_filtered','cust_purc_app_filtered',
                      'cust_gam_actv_filtered','FrequencyLF180','FrequencyApp180','FreqGamePlay180'),
                      by = 'CONTACT_WID', all.x = T)
# Getting Frequency360 data by calling function GetFrequency
freq_df <- merge(freq_df, GetFrequency('2012-12-26','cust_purc_lf_filtered','cust_purc_app_filtered',
                       'cust_gam_actv_filtered','FrequencyLF360','FrequencyApp360','FreqGamePlay360'),
                      by = 'CONTACT_WID', all.x = T)

# Merging this freq_df data frame containing Frequency information
# into original data frame
cust_final3 <- merge(cust_final1, freq_df, by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final3[is.na(cust_final3)] <- 0

# Removing unnecessary data frames to clean up memory on system
rm(freq_df)

############ Function to get Revenue
GetRevenue <- function(date, tableName1, tableName2, colName) {
  if (date != '') {
    # Condition if date is blank. This is when we are getting aggregation on whole data
    xrvn <- sqldf(sprintf("select CONTACT_WID, sum(AMOUNT) as xAmount from %s where TRANSACTION_DT1 < '%s' group by CONTACT_WID",tableName1,date))
    yrvn <- sqldf(sprintf("select CONTACT_WID, sum(AMOUNT_USD) as yAmount from %s where TRANSACTION_DATE1 < '%s' group by CONTACT_WID",tableName2,date))
  }
  else {
    # Else when date is not blank. This is when we are getting aggregation based on dates
    xrvn <- sqldf(sprintf('select CONTACT_WID, sum(AMOUNT) as xAmount from %s group by CONTACT_WID',tableName1))
    yrvn <- sqldf(sprintf('select CONTACT_WID, sum(AMOUNT_USD) as yAmount from %s group by CONTACT_WID',tableName2))
  }
  rvn <- merge(xrvn, yrvn, by = 'CONTACT_WID', all = T)
  rvn[is.na(rvn)] <- 0
  sqldf(sprintf("select CONTACT_WID, xAmount + yAmount as '%s' from rvn", colName))
}

# Getting Revenue data by calling function GetRevenue with blank date
revenue_df <- GetRevenue('','cust_purc_lf_filtered','cust_purc_app_filtered','TotalRevenueGenerated')
# Getting Revenue7 data by calling function GetRevenue
revenue_df <- merge(revenue_df, GetRevenue('2012-01-02','cust_purc_lf_filtered','cust_purc_app_filtered','Revenue7'),
                    by = 'CONTACT_WID', all.x = T)
# Getting Revenue30 data by calling function GetRevenue
revenue_df <- merge(revenue_df, GetRevenue('2012-01-26','cust_purc_lf_filtered','cust_purc_app_filtered','Revenue30'),
                    by = 'CONTACT_WID', all.x = T)
# Getting Revenue90 data by calling function GetRevenue
revenue_df <- merge(revenue_df, GetRevenue('2012-03-26','cust_purc_lf_filtered','cust_purc_app_filtered','Revenue90'),
                    by = 'CONTACT_WID', all.x = T)
# Getting Revenue180 data by calling function GetRevenue
revenue_df <- merge(revenue_df, GetRevenue('2012-06-23','cust_purc_lf_filtered','cust_purc_app_filtered','Revenue180'),
                    by = 'CONTACT_WID', all.x = T)
# Getting Revenue360 data by calling function GetRevenue
revenue_df <- merge(revenue_df, GetRevenue('2012-12-26','cust_purc_lf_filtered','cust_purc_app_filtered','Revenue360'),
                    by = 'CONTACT_WID', all.x = T)

# Merging this revenue_df data frame containing Revenue information
# into original data frame
cust_final4 <- merge(cust_final3, revenue_df, by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final4[is.na(cust_final4)] <- 0
colnames(cust_final4)
# Removing unnecessary data frames to clean up memory on system
rm(revenue_df)

############ stg_bgdt_cust_app_dwnld.txt ############
# Read file stg_bgdt_cust_app_dwnld.txt and store data in data frame
cust_app_dwn <- read.table("stg_bgdt_cust_app_dwnld.txt", header = T, sep = "\t", quote = "")
str(cust_app_dwn)

# Converting date into date time yyyy-mm-dd format
cust_app_dwn$NOMIN_DT <- as.Date(mdy_hm(cust_app_dwn$NOMIN_DT))

# Converting date to character and store in different variable 
# This is needed to query using sqldf
cust_app_dwn$NOMIN_DT1 <- as.character(cust_app_dwn$NOMIN_DT)

# Filtering data based on condition given
# NOMIN_DT > 2011-12-25
cust_app_dwn_filtered <- cust_app_dwn[cust_app_dwn$NOMIN_DT > '2011-12-25', ]

# Removing unnecessary data frames to clean up memory on system
rm(cust_app_dwn)

############ Functions to get Recency
GetRecencyRaw <- function(date, tableName) {
  if (date != '') {
    # Condition if date is blank. This is when we are getting aggregation on whole data
    if (tableName == 'cust_purc_app_filtered') {
      sqldf(sprintf("select CONTACT_WID, MAX(TRANSACTION_DATE1) as MaxTransactionDate from '%s' where TRANSACTION_DATE1 < '%s' group by CONTACT_WID", tableName, date))
    }
    else if (tableName == 'cust_purc_lf_filtered') {
      sqldf(sprintf("select CONTACT_WID, MAX(TRANSACTION_DT1) as MaxTransactionDate from '%s'  where TRANSACTION_DT1 < '%s'  group by CONTACT_WID", tableName,date))
    }
    else if(tableName =='cust_app_dwn_filtered'){
      sqldf(sprintf("select CONTACT_WID, MAX(NOMIN_DT1) as MaxTransactionDate from '%s'  where NOMIN_DT1 < '%s'  group by CONTACT_WID", tableName,date))
    }
  }
  else {
    # Else when date is not blank. This is when we are getting aggregation based on dates
    if (tableName == 'cust_purc_app_filtered') {
      sqldf(sprintf("select CONTACT_WID, MAX(TRANSACTION_DATE1) as MaxTransactionDate from '%s' group by CONTACT_WID", tableName))
    }
    else if (tableName == 'cust_purc_lf_filtered') {
      sqldf(sprintf("select CONTACT_WID, MAX(TRANSACTION_DT1) as MaxTransactionDate from '%s' group by CONTACT_WID", tableName))
    }
    else if(tableName =='cust_app_dwn_filtered'){
      sqldf(sprintf("select CONTACT_WID, MAX(NOMIN_DT1) as MaxTransactionDate from '%s' group by CONTACT_WID", tableName))
    }
  }
}

############ Functions to get Recency data
GetRecencyForDays <- function(RecencyFrame, names) {
  tmp <- RecencyFrame
  dates <- c("2013-04-11", "2012-01-02", "2012-01-26", "2012-03-23", "2012-06-23", "2012-12-26")
  transactionDate <- c("", "2012-01-02", "2012-01-26", "2012-03-23", "2012-06-23", "2012-12-26")
  for (i in 1:length(dates)) {
    abc <- GetRecencyRaw(transactionDate[i], RecencyFrame)
    name <- names[i]
    if (i == 1) {
      tmp <- cbind(tmp, data.frame(abc$CONTACT_WID))
      colnames(tmp) <- c('filename','CONTACT_WID')
    }
    x <- data.frame(name = sapply(abc$MaxTransactionDate, function(arg) {
      difftime(dates[i], arg, units = "days")
    }), CONTACT_WID = abc$CONTACT_WID)
    colnames(x) <- c(names[i], 'CONTACT_WID')
    tmp <- merge(tmp, x, by = 'CONTACT_WID', all.x = T)
  }
  rownames(tmp) <-  NULL
  return(tmp[,-2])
}

# Getting Receny for App data
RecencyAppHeaderNames<- c("RecencyApp", "RecencyApp7", "RecencyApp30", "RecencyApp90", "RecencyApp180", "RecencyApp360")
RecencyAppDaysData <- GetRecencyForDays('cust_purc_app_filtered', RecencyAppHeaderNames)

# Getting Receny for LF data
RecencyLFHeaderNames <- c("RecencyLF", "RecencyLF7", "RecencyLF30", "RecencyLF90", "RecencyLF180", "RecencyLF360")
RecencyLFDaysData <- GetRecencyForDays('cust_purc_lf_filtered', RecencyLFHeaderNames)

# Getting Receny for App Download data
RecencyDownHeaderNames <- c("Recencydown", "Recencydown7", "Recencydown30", "Recencydown90", "Recencydown180", "Recencydown360")
RecencyDownDaysData <- GetRecencyForDays('cust_app_dwn_filtered', RecencyDownHeaderNames)

# Merging data frames containing Recency information
recency_df <- data.frame(cust_final4$CONTACT_WID)
colnames(recency_df) <- c('CONTACT_WID')
recency_df <- merge(recency_df, RecencyAppDaysData, by = 'CONTACT_WID', all.x = T)
recency_df <- merge(recency_df, RecencyLFDaysData, by = 'CONTACT_WID', all.x = T)
recency_df <- merge(recency_df, RecencyDownDaysData, by = 'CONTACT_WID', all.x = T)

# Removing unnecessary data frames to clean up memory on system
rm(RecencyAppDaysData)
rm(RecencyLFDaysData)
rm(RecencyDownDaysData)

############ Function to get TotalTimeGamePlay
GetTimeGamePlay <- function(date, tableName, colName) {
  if (date != '') {
    # Condition if date is blank. This is when we are getting aggregation on whole data
    x1 <- sprintf("select CONTACT_WID, sum(ACT_TME_SPN_QTY) as %s from '%s' where TITLE_NOMIN_DT1 < '%s' group by CONTACT_WID",colName,tableName,date)
    xdf1 <- sqldf(x1)
  }
  else {
    # Else when date is not blank. This is when we are getting aggregation based on dates
    x1 <- sprintf("select CONTACT_WID, sum(ACT_TME_SPN_QTY) as %s from '%s' group by CONTACT_WID",colName,tableName)
    xdf1 <- sqldf(x1)
  }
  return(xdf1)
}

# Getting TimeGamePlay data by calling function GetTimeGamePlay with blank date
timeGamePlay <- GetTimeGamePlay('','cust_gam_actv_filtered','TotalTimeGamePlay')
# Getting TimeGamePlay7 data by calling function GetTimeGamePlay
timeGamePlay <- merge(timeGamePlay, GetTimeGamePlay('2012-01-02','cust_gam_actv_filtered','TotalTimeGamePlay7'), 
                       by = 'CONTACT_WID', all.x = T)
# Getting TimeGamePlay30 data by calling function GetTimeGamePlay
timeGamePlay <- merge(timeGamePlay, GetTimeGamePlay('2012-01-26','cust_gam_actv_filtered','TotalTimeGamePlay30'), 
                       by = 'CONTACT_WID', all.x = T)
# Getting TimeGamePlay90 data by calling function GetTimeGamePlay
timeGamePlay <- merge(timeGamePlay, GetTimeGamePlay('2012-03-26','cust_gam_actv_filtered','TotalTimeGamePlay90'), 
                       by = 'CONTACT_WID', all.x = T)
# Getting TimeGamePlay180 data by calling function GetTimeGamePlay
timeGamePlay <- merge(timeGamePlay, GetTimeGamePlay('2012-06-23','cust_gam_actv_filtered','TotalTimeGamePlay180'), 
                       by = 'CONTACT_WID', all.x = T)
# Getting TimeGamePlay360 data by calling function GetTimeGamePlay
timeGamePlay <- merge(timeGamePlay, GetTimeGamePlay('2012-12-26','cust_gam_actv_filtered','TotalTimeGamePlay360'), 
                       by = 'CONTACT_WID', all.x = T)

# Merging this timeGamePlay data frame containing Units information
# into original data frame
cust_final5 <- merge(cust_final4, timeGamePlay, by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final5[is.na(cust_final5)] <- 0

# Removing unnecessary data frames to clean up memory on system
rm(timeGamePlay)

# Adding NumGamesBought into original data frame
cust_final6 <- merge(cust_final5, sqldf("select CONTACT_WID, count(ITEM_NUMBER) as NumGamesBought from cust_purc_app_filtered group by CONTACT_WID"),
                     by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final6[is.na(cust_final6)] <- 0

############ Function to get NumGamesPlayed
GetNumGamesPlayed <- function(date, tableName, colName) {
  if (date != '') {
    x1 <- sprintf("select CONTACT_WID, count(X_GAME_NM) as %s from '%s' where TITLE_NOMIN_DT1 < '%s' group by CONTACT_WID",colName,tableName,date)
    xdf1 <- sqldf(x1)
  }
  else {
    x1 <- sprintf("select CONTACT_WID, count(X_GAME_NM) as %s from '%s' group by CONTACT_WID",colName,tableName)
    xdf1 <- sqldf(x1)
  }
  return(xdf1)
}

# Getting games data by calling function GetNumGamesPlayed
numGamesPlayed <- GetNumGamesPlayed('','cust_gam_actv_filtered','NumGamesPlayed')
numGamesPlayed <- merge(numGamesPlayed, GetNumGamesPlayed('2012-01-02','cust_gam_actv_filtered','NumGamesPlayed7'), 
                       by = 'CONTACT_WID', all.x = T)
numGamesPlayed <- merge(numGamesPlayed, GetNumGamesPlayed('2012-01-26','cust_gam_actv_filtered','NumGamesPlayed30'), 
                       by = 'CONTACT_WID', all.x = T)
numGamesPlayed <- merge(numGamesPlayed, GetNumGamesPlayed('2012-03-26','cust_gam_actv_filtered','NumGamesPlayed90'), 
                       by = 'CONTACT_WID', all.x = T)
numGamesPlayed <- merge(numGamesPlayed, GetNumGamesPlayed('2012-06-23','cust_gam_actv_filtered','NumGamesPlayed180'), 
                       by = 'CONTACT_WID', all.x = T)
numGamesPlayed <- merge(numGamesPlayed, GetNumGamesPlayed('2012-12-26','cust_gam_actv_filtered','NumGamesPlayed360'), 
                       by = 'CONTACT_WID', all.x = T)

# Merging this numGamesPlayed data frame containing Units information
# into original data frame
cust_final7 <- merge(cust_final6, numGamesPlayed, by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final7[is.na(cust_final7)] <- 0

# Removing unnecessary data frames to clean up memory on system
rm(numGamesPlayed)

############ Function to get Max by ignoring NAs
getMax <- function(x) {
  return (ifelse(!all(is.na(x)), max(x, na.rm=T), NA))
}

############ Function to get Max by ignoring NAs
getMin <- function(x) {
  return (ifelse(!all(is.na(x)), min(x, na.rm=T), NA))
}

############ Function to get Max, Min Recency metrics
GetRecencyMetrics <- function(df) {
  temp_df <- data.frame(CONTACT_WID = df$CONTACT_WID)
  temp_df <- data.frame(temp_df, maxRecencyCum = apply(subset(df, select = c("RecencyApp", "RecencyLF", "Recencydown")), 1, getMax))
  temp_df <- data.frame(temp_df, minRecencyCum = apply(subset(df, select = c("RecencyApp", "RecencyLF", "Recencydown")), 1, getMin))
  temp_df <- data.frame(temp_df, maxRecencyCum7 = apply(subset(df, select = c("RecencyApp7", "RecencyLF7", "Recencydown7")), 1, getMax))
  temp_df <- data.frame(temp_df, minRecencyCum7 = apply(subset(df, select = c("RecencyApp7", "RecencyLF7", "Recencydown7")), 1, getMin))
  temp_df <- data.frame(temp_df, maxRecencyCum30 = apply(subset(df, select = c("RecencyApp30", "RecencyLF30", "Recencydown30")), 1, getMax))
  temp_df <- data.frame(temp_df, minRecencyCum30 = apply(subset(df, select = c("RecencyApp30", "RecencyLF30", "Recencydown30")), 1, getMin))
  temp_df <- data.frame(temp_df, maxRecencyCum90 = apply(subset(df, select = c("RecencyApp90", "RecencyLF90", "Recencydown90")), 1, getMax))
  temp_df <- data.frame(temp_df, minRecencyCum90 = apply(subset(df, select = c("RecencyApp90", "RecencyLF90", "Recencydown90")), 1, getMin))
  temp_df <- data.frame(temp_df, maxRecencyCum180 = apply(subset(df, select = c("RecencyApp180", "RecencyLF180", "Recencydown180")), 1, getMax))
  temp_df <- data.frame(temp_df, minRecencyCum180 = apply(subset(df, select = c("RecencyApp180", "RecencyLF180", "Recencydown180")), 1, getMin))
  temp_df <- data.frame(temp_df, maxRecencyCum360 = apply(subset(df, select = c("RecencyApp360", "RecencyLF360", "Recencydown360")), 1, getMax))
  temp_df <- data.frame(temp_df, minRecencyCum360 = apply(subset(df, select = c("RecencyApp360", "RecencyLF360", "Recencydown360")), 1, getMin))
  return(temp_df)
}

# Merging max, min, recency data frame containing Units information
# into original data frame
cust_final8 <- merge(cust_final7, recency_df, by = 'CONTACT_WID', all.x = T)
cust_final8 <- merge(cust_final8, GetRecencyMetrics(recency_df), by = 'CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final8[is.na(cust_final8)] <- 0

# Removing unnecessary data frames to clean up memory on system
rm(recency_df)

############ Function to get data to find favorite game, source and channel
GetFavFilteredDataRaw <- function(date, tablename) {
  if (tablename == 'cust_purc_app_filtered') {
    if (date != '') {
      sqldf(sprintf("select * from '%s' where TRANSACTION_DATE1 < '%s' ", tablename,date))
    }
    else {
      sqldf(sprintf("select * from '%s' ", tablename))
    }
  }
  else if (tablename == 'cust_gam_actv_filtered') {
    if (date != '') {
      sqldf(sprintf("select * from '%s' where TITLE_NOMIN_DT1 < '%s' ", tablename,date))
    }
    else {
      sqldf(sprintf("select * from '%s' ", tablename))
    }
  }
}

############ Function to get Favorite Channel
GetFavoritesChannel <- function(df){
  temp <- data.frame(count(df, CONTACT_WID))
  temp1 <- data.frame(count(df, CONTACT_WID, CHANNEL_DESCRIPTION))
  temp2 <- merge(temp1, temp, by = 'CONTACT_WID', all = T)
  temp2$PercentageSourceUsage <- temp2$n.x * 100 / temp2$n.y
  temp3 <- sqldf("select CONTACT_WID, CHANNEL_DESCRIPTION, max(PercentageSourceUsage) from temp2 group by CONTACT_WID")
  temp3$output <- ifelse(temp3$`max(PercentageSourceUsage)` < 50, NA, as.character(temp3$CHANNEL_DESCRIPTION))
  temp3$output1 <- ifelse(temp3$`max(PercentageSourceUsage)` < 50, 'Uniform', 'Favorite')
  temp4 <- subset(temp3, select = c(CONTACT_WID, output, output1))
  return(temp4)
}

############ Function to get Favorite Source
GetFavoritesSource <- function(df){
  temp <- data.frame(count(df, CONTACT_WID))
  temp1 <- data.frame(count(df, CONTACT_WID, SOURCE_OF_PURCHASE))
  temp2 <- merge(temp1, temp, by = 'CONTACT_WID', all = T)
  temp2$PercentageSourceUsage <- temp2$n.x * 100 / temp2$n.y
  temp3 <- sqldf("select CONTACT_WID, SOURCE_OF_PURCHASE, max(PercentageSourceUsage) from temp2 group by CONTACT_WID")
  temp3$output <- ifelse(temp3$`max(PercentageSourceUsage)` < 50, NA, as.character(temp3$SOURCE_OF_PURCHASE))
  temp3$output1 <- ifelse(temp3$`max(PercentageSourceUsage)` < 50, 'Uniform', 'Favorite')
  temp4 <- subset(temp3, select = c(CONTACT_WID, output, output1))
  return(temp4)
}

############ Function to get Favorite Game
GetFavoritesGameBin <- function(df){
  temp <- aggregate(df$ACT_TME_SPN_QTY, by=list(df$CONTACT_WID), sum)
  colnames(temp) <- c('CONTACT_WID', 'Total')
  temp1 <- aggregate(df$ACT_TME_SPN_QTY, by=list(df$CONTACT_WID, df$X_GAME_NM), sum)
  colnames(temp1) <- c('CONTACT_WID','X_GAME_NM','SourceWise')
  temp2 <- merge(temp1, temp, by = 'CONTACT_WID', all = T)
  temp2$PercentageSourceUsage <- temp2$SourceWise * 100 / temp2$Total
  temp3 <- sqldf("select CONTACT_WID, X_GAME_NM, max(PercentageSourceUsage) from temp2 group by CONTACT_WID")
  temp3$output <- ifelse(temp3$`max(PercentageSourceUsage)` < 50, NA, as.character(temp3$X_GAME_NM))
  temp3$output1 <- ifelse(temp3$`max(PercentageSourceUsage)` < 50, 'Uniform', 'Favorite')
  temp4 <- subset(temp3, select = c(CONTACT_WID, output, output1))
  return(temp4)
}

# Calling GetFavFilteredDataRaw and GetFavoritesChannel functions
# and storing output in data frame to capture Favorite source, channel and game 
df <- GetFavFilteredDataRaw('', 'cust_purc_app_filtered')
favorite_df <- GetFavoritesChannel(df)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteSource"
colnames(favorite_df)[ncol(favorite_df)] <- "FavSourceBin"
favorite_df <- merge(favorite_df, GetFavoritesSource(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- 'FavoriteChannel'
colnames(favorite_df)[ncol(favorite_df)] <- 'FavChannelBin'
df <- GetFavFilteredDataRaw('2012-01-02', 'cust_purc_app_filtered')
favorite_df <- merge(favorite_df, GetFavoritesChannel(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteChannel7"
colnames(favorite_df)[ncol(favorite_df)] <- "FavChannel7Bin"
favorite_df <- merge(favorite_df, GetFavoritesSource(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteSource7"
colnames(favorite_df)[ncol(favorite_df)] <- "FavSource7Bin"
df <- GetFavFilteredDataRaw('2012-01-26', 'cust_purc_app_filtered')
favorite_df <- merge(favorite_df, GetFavoritesChannel(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteChannel30"
colnames(favorite_df)[ncol(favorite_df)] <- "FavChannel30Bin"
favorite_df <- merge(favorite_df, GetFavoritesSource(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteSource30"
colnames(favorite_df)[ncol(favorite_df)] <- "FavSource30Bin"
df <- GetFavFilteredDataRaw('2012-03-26', 'cust_purc_app_filtered')
favorite_df <- merge(favorite_df, GetFavoritesChannel(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteChannel90"
colnames(favorite_df)[ncol(favorite_df)] <- "FavChannel90Bin"
favorite_df <- merge(favorite_df, GetFavoritesSource(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteSource90"
colnames(favorite_df)[ncol(favorite_df)] <- "FavSource90Bin"
df <- GetFavFilteredDataRaw('2012-06-23', 'cust_purc_app_filtered')
favorite_df <- merge(favorite_df, GetFavoritesChannel(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteChannel180"
colnames(favorite_df)[ncol(favorite_df)] <- "FavChannel180Bin"
favorite_df <- merge(favorite_df, GetFavoritesSource(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteSource180"
colnames(favorite_df)[ncol(favorite_df)] <- "FavSource180Bin"
df <- GetFavFilteredDataRaw('2012-12-26', 'cust_purc_app_filtered')
favorite_df <- merge(favorite_df, GetFavoritesChannel(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteChannel360"
colnames(favorite_df)[ncol(favorite_df)] <- "FavChannel360Bin"
favorite_df <- merge(favorite_df, GetFavoritesSource(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteSource360"
colnames(favorite_df)[ncol(favorite_df)] <- "FavSource360Bin"
df <- GetFavFilteredDataRaw('', 'cust_gam_actv_filtered')
favorite_df <- merge(favorite_df, GetFavoritesGameBin(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteGame"
colnames(favorite_df)[ncol(favorite_df)] <- "FavGameBin"
df <- GetFavFilteredDataRaw('2012-01-02', 'cust_gam_actv_filtered')
favorite_df <- merge(favorite_df, GetFavoritesGameBin(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteGame7"
colnames(favorite_df)[ncol(favorite_df)] <- "FavGame7Bin"
df <- GetFavFilteredDataRaw('2012-01-26', 'cust_gam_actv_filtered')
favorite_df <- merge(favorite_df, GetFavoritesGameBin(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteGame30"
colnames(favorite_df)[ncol(favorite_df)] <- "FavGame30Bin"
df <- GetFavFilteredDataRaw('2012-03-26', 'cust_gam_actv_filtered')
favorite_df <- merge(favorite_df, GetFavoritesGameBin(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteGame90"
colnames(favorite_df)[ncol(favorite_df)] <- "FavGame90Bin"
df <- GetFavFilteredDataRaw('2012-06-23', 'cust_gam_actv_filtered')
favorite_df <- merge(favorite_df, GetFavoritesGameBin(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteGame180"
colnames(favorite_df)[ncol(favorite_df)] <- "FavGame180Bin"
df <- GetFavFilteredDataRaw('2012-12-26', 'cust_gam_actv_filtered')
favorite_df <- merge(favorite_df, GetFavoritesGameBin(df), by='CONTACT_WID', all.x = T)
colnames(favorite_df)[ncol(favorite_df)-1] <- "FavoriteGame360"
colnames(favorite_df)[ncol(favorite_df)] <- "FavGame360Bin"

# Merging this favorite_df data frame containing Units information
# into original data frame
cust_final9 <- merge(cust_final8, favorite_df, by = 'CONTACT_WID', all.x = T)

# Removing unnecessary data frames to clean up memory on system
rm(favorite_df)

# Get StrengthOfFavoriteGame
# Aggregate Time spend per CONTACT_WID on games and get sum of time spent
temp <- aggregate(cust_gam_actv_filtered$ACT_TME_SPN_QTY, by=list(cust_gam_actv_filtered$CONTACT_WID), sum)
colnames(temp) <- c('CONTACT_WID', 'Total')
# Aggregate Time spend per CONTACT_WID and X_GAME_NM and get sum of time spent on each game by each contact
temp1 <- aggregate(cust_gam_actv_filtered$ACT_TME_SPN_QTY, by=list(cust_gam_actv_filtered$CONTACT_WID, cust_gam_actv_filtered$X_GAME_NM), sum)
colnames(temp1) <- c('CONTACT_WID','X_GAME_NM','SourceWise')
# Merge two data frames
temp2 <- merge(temp1, temp, by = 'CONTACT_WID', all = T)
# Get PercentageSourceUsage
temp2$PercentageSourceUsage <- temp2$SourceWise * 100 / temp2$Total
# Get Max of PercentageSourceUsage
temp3 <- sqldf("select CONTACT_WID, X_GAME_NM, max(PercentageSourceUsage) as MaxPercentageSourceUsage from temp2 group by CONTACT_WID")
# Get StrengthOfFavoriteGame based on conditions givem
# Weekly favorite (20% or more on one game)
# Medium favorite (40% ore more on one game)
# Favorite (60% or more)
# Strongly favorite (80% or more)
# Addicted (95% or more)
temp3$StrengthOfFavoriteGame <- ifelse(temp3$MaxPercentageSourceUsage >= 95, 'Addicted', 
                                        ifelse(temp3$MaxPercentageSourceUsage >= 80, 'Strongly Favorite', 
                                        ifelse(temp3$MaxPercentageSourceUsage >= 60, 'Favorite',
                                        ifelse(temp3$MaxPercentageSourceUsage >= 40, 'Medium Favorite',
                                        ifelse(temp3$MaxPercentageSourceUsage >= 20, 'Weakly Favorite',NA)))))
temp4 <- subset(temp3, select = c('CONTACT_WID','StrengthOfFavoriteGame'))

# Merging this temp4 data frame containing GameStrength information
# into original data frame
cust_final10 <- merge(cust_final9, temp4, by='CONTACT_WID', all.x = T)

# Replacing NAs with 0 in final dataframe
cust_final10[is.na(cust_final10)] <- 'None'

# Merging this temp4 data frame containing tenureDays_df information
# into original data frame
cust_final11 <- merge(cust_final10, tenureDays_df, by='CONTACT_WID', all.x = T)

write.csv(cust_final11, "Group13-Cute2-Final-Data.csv", row.names = F)

# Removing unnecessary data frames to clean up memory on system
rm(df)
rm(temp)
rm(temp1)
rm(temp2)
rm(temp3)
rm(temp4)
rm(tenureDays_df)

# Removing finally all data frames to clean up memory on system
rm(cust_app_dwn_filtered)
rm(cust_gam_actv_filtered)
rm(cust_purc_app_filtered)
rm(cust_purc_lf_filtered)
rm(unique_cust_chld_filtered)
rm(cust_final)
rm(cust_final1)
rm(cust_final2)
rm(cust_final3)
rm(cust_final4)
rm(cust_final5)
rm(cust_final6)
rm(cust_final7)
rm(cust_final8)
rm(cust_final9)
rm(cust_final10)