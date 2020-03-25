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
