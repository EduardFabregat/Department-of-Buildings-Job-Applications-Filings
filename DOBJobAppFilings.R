library(tidyverse)

df <- Historical_DOB_Permit_Issuance

rm(Historical_DOB_Permit_Issuance)

#What fraction of all construction permits in this data set correspond to renewed permits? 
#The filing type is recorded under column 'Filing Status'. 
#For first-time permits the value in this column is 'INITIAL' and for 
#renewed permits it is 'RENEWAL'.

colnames(df)

table(df$`Filing Status`)

all_num <- nrow(df)

ren_num <- nrow(df[df$`Filing Status`== "RENEWAL",])

ren_num/all

ren <- df[df$`Filing Status`=="RENEWAL", ]

#The filing type of a permit is recorded under column 'Filing Status'. 
#For first-time permits the value in this column is 'INITIAL' and for renewed permits 
#it is 'RENEWAL'. What kind of an owner the permit was issued to is recorded in 
#the column "Owner's Business Type", for corporations the value is 'CORPORATION', 
#for individuals it is 'INDIVIDUAL'. If you consider only renewed permits, 
#what is the ratio of the number of permits issued to corporations to the number of 
#permits issued to individuals?

table(ren$`Owner's Business Type`)

corp <- nrow(ren[ren$`Owner's Business Type`=="CORPORATION",])

indi <- nrow(ren[ren$`Owner's Business Type`=="INDIVIDUAL", ])

all_ren <- nrow(ren)

corp/indi

#Consider only those permits that were issued for more than 365 days. 
#The date on which a permit was issued is recorded in column 'Issuance Date', 
#the date the permit expires is in column 'Expiration Date'. 
#What fraction of these at least year-long permits were issued in the 
#borough with the highest number of such permits? 
#The borough where the construction will take place is in column 'BOROUGH'.


df$diff_days <- as.Date(df$`Expiration Date`, "%m/%d/%Y")-
  as.Date(df$`Issuance Date`, "%m/%d/%Y")

more_year <- df[df$diff_days > 365,]

table(more_year$BOROUGH)

max_permit <- max(table(more_year$BOROUGH))

max_permit/nrow(more_year)


#Limit your analysis to permits that were filed in 2010. 
#The column recording the date of permit filing is called 'Filing Date'. 
#For each ZIP code compute the ratio between the number of permits issued in 2010 
#and number of residents in that ZIP code. Make sure to discard any ZIP codes with 
#population lower than 1000 people. Note: you will need to use both the DOB permits 
#dataset and the US Census dataset. In the DOB permits dataset, the ZIP code is 
#recorded under column 'Postcode'. How many ZIP codes are outliers in terms of number 
#of construction permits per number of people - more specificaly, for how many ZIP 
#codes does this computed ratio exceed the mean by 
#more than twice the standard deviation?

df$year <- format(as.Date(df$`Filing Date`, format="%d/%m/%Y"),"%Y")

twenty_ten <- df[df$year == 2010, ]

census <- X2010_Census_Population_By_Zipcode_ZCTA_

census2 <- census[census$`2010 Census Population` >= 1000,]

twenty_ten2 <- twenty_ten[which(twenty_ten$Postcode %in% census2$`Zip Code ZCTA`),]

twenty_ten2$forsum <- 1

zip_codes <- aggregate(x = twenty_ten2[,"forsum"],
                  by = list(twenty_ten2$Postcode), FUN = "sum")

colnames(census2)[1] <- "postcode"
colnames(zip_codes)[1] <- "postcode"
colnames(zip_codes)[2] <- "permits"

zip_codes$postcode <- as.character(zip_codes$postcode)

zip_codes2 <- left_join(zip_codes, census2)

zip_codes2$ratio <- zip_codes2$permits/zip_codes2$`2010 Census Population`

mean_zipcodes <- mean(zip_codes2$ratio)
sd_zipcodes <- sd(zip_codes2$ratio)*2

outliers_finder <- mean_zipcodes+sd_zipcodes


outliers <- zip_codes2[zip_codes2$ratio > outliers_finder,]

nrow(outliers)

#Observe how the number of issued permits changes accross the years. 
#Limit your analysis to years 1990-2012 (both inclusive). 
#What is the coefficient of determination (R squared) between the year 
#a permit was issued and the number of issued permits that year? 
#The column recording the date when a permit was issued is called 'Issuance Date'. 

ninety_twenty <- df[df$year <= 2012 & df$year >= 1990, ]

ninety_twenty$forsum <- 1

num_permits <- aggregate(x = ninety_twenty[,"forsum"],
                       by = list(ninety_twenty$year), FUN = "sum")

num_permits$Group.1 <- as.numeric(num_permits$Group.1)

summary(lm(forsum~Group.1, num_permits))$r.squared

#Let's investigate how the number of construction jobs that start in 
#the summer vs the winter changes across the years. 
#The date of construction job start is recorded in column 'Job Start Date'. 
#For every year, compute the ratio between the number of construction jobs that 
#start in the peak of summer (in July and August) and the number of jobs that 
#start in the peak of winter (in January and February). 
#Again limit your analysis to years 1990-2012 (both inclusive). 
#Find the year when this ratio was maximal - what was the value of this ratio for 
#that year?

ninety_twenty$month <- format(as.Date(ninety_twenty$`Filing Date`, format="%d/%m/%Y"),"%m")


summer <- ninety_twenty[ninety_twenty$month == "07" | ninety_twenty$month == "08", ]
winter <- ninety_twenty[ninety_twenty$month == "01" | ninety_twenty$month == "02", ]

summer_year <- aggregate(x = summer[,"forsum"],
                         by = list(summer$year), FUN = "sum")

winter_year <- aggregate(x = winter[,"forsum"],
                         by = list(winter$year), FUN = "sum")

colnames(summer_year)[1] <- "year"
colnames(winter_year)[1] <- "year"
colnames(summer_year)[2] <- "summer_permits"
colnames(winter_year)[2] <- "winter_permits"

sum_win <- left_join(summer_year, winter_year)

sum_win$ratio <- sum_win$summer_permits/sum_win$winter_permits

max(sum_win$ratio)


#If we look at how permit duration varies with the construction start date, 
#it appears like jobs that start in November tend to have shorter permit duration. 
#The date of construction job start is recorded in column 'Job Start Date'. 
#The date on which a permit was issued is recorded in column 'Issuance Date', 
#the date the permit expires is in column 'Expiration Date'. 
#Calculate the chi-square test statistic for testing whether a permit is more 
#likely to be issued for less than 60 days when construction job start date is November. 
#Again limit your analysis to data from years 1990-2012 (both inclusive).

ninety_twenty2 <- ninety_twenty[,c(26,27,28)]

ninety_twenty2$month_start <- format(as.Date(ninety_twenty2$`Job Start Date`, format="%d/%m/%Y"),"%m")

ninety_twenty2$diff_days <- as.Date(ninety_twenty2$`Expiration Date`, "%m/%d/%Y")-
  as.Date(ninety_twenty2$`Issuance Date`, "%m/%d/%Y")

ninety_twenty3 <- ninety_twenty2[ninety_twenty2$diff_days >= 0, ]

ninety_twenty3 <- ninety_twenty3 %>% 
  mutate(days = 
           ifelse(ninety_twenty3$diff_days < 60, "sixty", "not_sixty"))

ninety_twenty3 <- ninety_twenty3 %>% 
  mutate(month = 
           ifelse(ninety_twenty3$month_start == "11", "november", "not_november"))


tbl <- table(ninety_twenty3$days, ninety_twenty3$month)

chisq.test(tbl)



options(stringsAsFactors = FALSE)

DOB1 <- DOB_Job_Application_Filings[DOB_Job_Application_Filings$`Doc #` == "01", ]

DOB1$DOBRunDate <- as.Date(DOB1$DOBRunDate, '%m/%d/%Y')

DOB3 <- cbind(DOB1, DOB2)

library(dplyr)
DOB2 <- DOB1 %>% 
  group_by(`Job #`) %>%
  slice(which.max(DOBRunDate))

DOB2$`Pre- Filing Date` <- as.Date(DOB2$`Pre- Filing Date`, "%m/%d/%Y")

DOB18 <- DOB2[DOB2$`Pre- Filing Date` >= "2018-01-01" & DOB2$`Pre- Filing Date` <= "2018-12-31", ]

man <- DOB2[DOB2$Borough == "MANHATTAN", ]

occ <- man %>% 
  group_by(`Existing Occupancy`) %>% 
  count(`Existing Occupancy`)

(178930 + 1 + 6224 + 52300 + 2656)/647264

DOB1318 <- DOB2[DOB2$`Pre- Filing Date` >= "2013-01-01" & DOB2$`Pre- Filing Date` <= "2018-12-31",]

DOB1318_fp <- filter(DOB1318, !`Fully Permitted` == "NA")

borough <- DOB1318_fp %>% 
  group_by(Borough) %>% 
  count(Borough)

QB <- borough[borough$Borough == "QUEENS" | borough$Borough == "BRONX", ]



chisq <- chisq.test(QB$n)
chisq

man <- DOB2[DOB2$Borough == "MANHATTAN", ]
brx <- DOB2[DOB2$Borough == "BRONX", ]
queens <- DOB2[DOB2$Borough == "QUEENS", ]
brk <- DOB2[DOB2$Borough == "BROOKLYN", ]
si <- DOB2[DOB2$Borough == "STATEN ISLAND", ]

owner_man <- man %>% 
  group_by(`Owner Type`) %>% 
  count(`Owner Type`)

(280967 + 231929)/647264 #0.7924062

owner_brx <- brx %>% 
  group_by(`Owner Type`) %>% 
  count(`Owner Type`)

(37292 + 25915)/124696 #0.5068888

owner_queens <- queens %>% 
  group_by(`Owner Type`) %>% 
  count(`Owner Type`)

(78454 + 43230)/312790 #0.3890278

owner_brk <- brk %>% 
  group_by(`Owner Type`) %>% 
  count(`Owner Type`)

(86297 + 70760)/351453 #0.4468791

owner_si <- si %>% 
  group_by(`Owner Type`) %>% 
  count(`Owner Type`)

(23902 + 6788)/90737 #0.3382303


#ratio highest to second highest
(512896/647264)/(63207/124696) #1.563274

brx <- 42.10
brk <- 70.82
man <- 22.83
queens <- 108.53
si <- 58.37

#Obtain the area in square miles for each borough in New York City. 
#For each borough, and for entries with a pre- filing date in 2018, 
#compute the number of job applications for constructing new buildings per square mile. 
#The Job Type column contains information regarding which jobs are for new buildings. 
#Report the ratio between the highest and second highest of these values.

DOB18_new <- DOB18[DOB18$`Job Type`== "NB", ]

new <- DOB18_new %>% 
  group_by(Borough) %>% 
  count(Borough)

new[1,2]/brx #6.199525
new[2,2]/brk #8.669867
new[3,2]/man #3.942181
new[4,2]/queens #7.960932
new[5,2]/si #11.10159

#ratio highest to second highest
(new[5,2]/si)/(new[2,2]/brk) #1.28048

#Consider all DOB job applications with residential 'Existing Occupancy' types 
#and with job type A1. For this subset, 
#what proportion of job applications involve an increase 
#from the number of existing dwelling units to the number of proposed dwelling units? 
#Ignore entries with missing values. 

DOB_A1 <- DOB2[DOB2$`Job Type` == "A1" & DOB2$`Existing Occupancy` == "RES",  ]

DOB_A1 <- subset(DOB_A1, !`Existing Dwelling Units` == "NA")
DOB_A1 <- subset(DOB_A1, !`Proposed Dwelling Units` == "NA")

DOB_A1$`Existing Dwelling Units` <- as.integer(DOB_A1$`Existing Dwelling Units`)
DOB_A1$`Proposed Dwelling Units` <- as.integer(DOB_A1$`Proposed Dwelling Units`)

DOB_A1_diff <- DOB_A1$`Proposed Dwelling Units` - DOB_A1$`Existing Dwelling Units`

DOB_A1_diff <- as.data.frame(DOB_A1_diff)
DOB_A1 <- cbind.data.frame(DOB_A1, DOB_A1_diff)

DOB_A1_new <- DOB_A1[which(DOB_A1$DOB_A1_diff > 0), ]

7951/24226 #0.3282011

#Compute the number of days it takes to obtain a permit for each DOB application 
#in Brooklyn from 2013 to 2018. Perform a linear regression on these differences against 
#the applications' pre-filing years, then report the R^2 value. 
#Ignore missing values, i.e. applications that have not been approved. 


DOB1318_brk <- DOB1318[DOB1318$Borough == "BROOKLYN", ]

library(lubridate)

DOB1318_brk$`Fully Permitted` <- mdy(DOB1318_brk$`Fully Permitted`)

DOB1318_brk$`Fully Paid` <- mdy(DOB1318_brk$`Fully Paid`)

DOB1318_brk$time <- ymd(DOB1318_brk$`Fully Permitted`) - ymd(DOB1318_brk$`Fully Paid`)

DOB1318_brk2 <- filter(DOB1318_brk, !time == "NA")

DOB1318_brk2$time <- as.integer(DOB1318_brk2$time)

DOB1318_brk2$day.numeric <- as.numeric(DOB1318_brk2$`Pre- Filing Date`)/(24*60*60)

model <- lm(time ~ `Pre- Filing Date`, DOB1318_brk2)

stargazer::stargazer(model, type = "text")

summary(model)
