#Importing the libraries
library('lubridate')
library('ggplot2')
library('ISOweek')
#Setting the working directory
setwd('C:/Users/balag/OneDrive/Desktop/Airdata-R')

#Adding functions
source('Months_NA.R')
source('Weekly_NA.R')
source('Days_NA.R')
source('Sites_NA.R')

#Reading lines from the csv file
file.info('2013_raw.csv')$size/2^30
readLines('2013_raw.csv',n=5)
data_file='2013_raw.csv'

#Chunk size
chunk_size=100000
#Creating connection to the csv file and reading data as chunks
con=file(description = data_file,open = 'r')
data=read.table(con,nrows = chunk_size,header=TRUE,fill=TRUE,sep=',')
dim(data)
str(data)
df=data
head(data)
#Converting the factor type val column into a numeric list 
raw_2013_col_name=colnames(data)
col_names_NA=list()
i=1
for (col_no in 1:length(raw_2013_col_name)){
  df[,raw_2013_col_name[col_no ]]=as.vector(df[,raw_2013_col_name[col_no]])
  df[df==""]  <- NA
  df[df=="NULL"]  <- NA
  if(anyNA(df[,raw_2013_col_name[col_no]])) {
    print(raw_2013_col_name[col_no])
    col_names_NA[i]=raw_2013_col_name[col_no]
  }
  i=i+1
}
col_names_NA
col_names_NA[col_names_NA=="NULL"]  <- NA
col_names_NA=col_names_NA[!is.na(col_names_NA)]

df[,'epoch']= as.POSIXct(df[,'epoch'], origin="1970-01-01")
date=apply(df[,'epoch',drop=F],2,function(x) substr(x,0,10))

site=df$site
#----------------sites-----------------#

dframe_new_site=NA_per_sites(site,col_names_NA)

#--------------Monthly----------------#

dframe_new_month=NA_per_months(date,col_names_NA)

#--------------Weekly----------------#


dframe_new_week=NA_per_weeks(date,col_names_NA)


#--------------Daily----------------#


dframe_new_day=NA_per_days(date,col_names_NA)