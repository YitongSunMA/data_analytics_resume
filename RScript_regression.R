install.packages("jtools")  
install.packages("huxtable")      
library(huxtable)
library(plyr)
library(dplyr)
library(readr)
library(jtools)


#Question 1

  #Part A

asec2014 <- read_fwf(file="asec2014_pubuse.dat",fwf_positions(c(1,2,25,39,42,49,53),c(1,6,25,39,43,51,53),col_names = c("type","hid","htype","region","state","country","metro_status")))
save(asec2014, file = "cps.Rdata") #save as R data set

cps14_house <- asec2014 %>% #read in the household level data
  dplyr::filter(type == 1) %>% #keep if type = 1 and leave only type 1 data in a way to drop variable type
  dplyr::arrange(hid) #sort the observations by hid.

readr::write_csv(x = cps14_house, path = "cps14_house.csv")
save(cps14_house, file = "cps14_house.Rdata") #save as r data set
rm(list=ls())#clear

asec2014 <- read_fwf(file="asec2014_pubuse.dat",fwf_positions(c(1,2,15,19,21,24,25,27,48,364),c(1,6,16,20,21,24,26,28,49,370),col_names = c("type","hid","relationship","age","marital_status","sex","educ","race","fid","wsal"))) #read in person level data
cps14_person <- asec2014 %>%
  dplyr::filter(type == 3) %>% #keep people observations
  dplyr::arrange(hid) #sort the observation by hid

readr::write_csv(x = cps14_person, path = "cps14_person.csv")
save(cps14_person, file = "cps14_person.Rdata")
rm(list=ls())#clear

load("cps14_house.Rdata")
load("cps14_person.Rdata")
load("cps_2014.Rdata")
cps_2014 <- merge(cps14_house,cps14_person,by="hid") #merge housefuld and person level dataset
save(cps_2014, file = "cps_2014.Rdata") #save cps_2014 data

  #Part B

cps_2014 <- cps_2014%>%
  dplyr::filter(age >= 25)%>% #keep if age >=25 or <=64
  dplyr::filter(age <=64)%>% 
  dplyr::filter(metro_status != 3)%>% #drop if metro_status = 3
  dplyr::filter(wsal>10000) #rule out part time or non working workers
cps_2014$wsal <- as.numeric(as.character(cps_2014$wsal))/1000 #put wsal in 1000 for easy viewing

  #Part C

cps_2014 <- mutate(cps_2014,hs = 0) #generate variables hs, col and female
cps_2014 <- mutate(cps_2014,col = 0)
cps_2014 <- mutate(cps_2014,female = 0)

cps_2014 <- cps_2014%>%
  mutate(hs = case_when(educ>=39 & educ<=42 ~ 1, #gen hs = 1 if educ >=39 and educ <=42
                        educ<39 ~ 0,
                        educ>42 ~ 0))%>%
  mutate(col = case_when(educ>=43 ~ 1,  #col = educ >=43
                         educ<43 ~ 0))%>%
  mutate(female = case_when(sex==2 ~ 1, #female = 1 if sex = 2
                            sex != 2 ~ 0))

  #Part D

#create summary statistics for hs, col, female and wsal.
a <- as.numeric(format(round(summary(cps_2014$hs),3),nsmall = 3)) #generate a table of summary statistics and to 3 digits
b <- as.numeric(format(round(summary(cps_2014$col),3),nsmall = 3))
c <- as.numeric(format(round(summary(cps_2014$female),3),nsmall = 3))
d <- as.numeric(format(round(summary(cps_2014$wsal),3),nsmall = 3))
e <- as.numeric(format(round(sum_hs <- NROW(na.omit(cps_2014$hs)),3),nsmall = 3))
f <- as.numeric(format(round(sum_col <- NROW(na.omit(cps_2014$col)),3),nsmall = 3))
g <- as.numeric(format(round(sum_female <- NROW(na.omit(cps_2014$female)),3),nsmall = 3))
i <- as.numeric(format(round(sum_wsal <- NROW(na.omit(cps_2014$wsal)),3),nsmall = 3))

table <- rbind(a,b,c,d)
colnames(table) <- c("min","1st Qu","median","mean","3rd Qu", "max")
rownames(table) <- c("hs","col","female","wsal")
observations <- c(e,f,g,i)
table <- cbind(table,observations)
write.table(table, file = "table.txt", sep = ",", quote = FALSE, row.names = F)
