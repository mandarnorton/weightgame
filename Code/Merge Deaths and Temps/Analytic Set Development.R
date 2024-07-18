#Create Some Analytic Sets With Correlations 
#install.packages('metR')
#install.packages('psych')

#Load Library 
library(dplyr)
library(metR)
library(psych)

#Turn off Scientific Notation
options(scipen=999)

#Read in Data 
death_temps<-read.csv(file = '/Users/amandanorton/Desktop/Standardized Deaths/Deaths_Temps.csv')

#Create a Dataset of Correlations by NUTS3 
cor_death <- death_temps %>%
  group_by(NUTSID) %>%
  summarise(avg_death_c = cor(avg_temp, stn_death), wgt_death_c = cor(wgt_temp, stn_death), log_death_c = cor(wgt_logtemp, stn_death), age_death_c = cor(wgt_65plstemp, stn_death))

cor_temp <- death_temps %>%
  group_by(NUTSID) %>%
  summarise(avg_wgt = cor(wgt_temp, avg_temp),avg_log = cor(wgt_logtemp, avg_temp),avg_age = cor(wgt_65plstemp, avg_temp))

#Add Difference Measurements 
#add % difference measurements (wgt)
death_temps$wgt_dif_abs<-(abs(death_temps$avg_temp-death_temps$wgt_temp))
death_temps$wgt_dif_abs_pct<-((death_temps$wgt_dif_abs/(abs(death_temps$avg_temp)))*100)

#add % difference measurements (log)
death_temps$log_dif_abs<-(abs(death_temps$avg_temp-death_temps$wgt_logtemp))
death_temps$log_dif_abs_pct<-((death_temps$log_dif_abs/(abs(death_temps$avg_temp)))*100)

#add % difference measurements (age)
death_temps$age_dif_abs<-(abs(death_temps$avg_temp-death_temps$wgt_65plstemp))
death_temps$age_dif_abs_pct<-((death_temps$age_dif_abs/(abs(death_temps$avg_temp)))*100)

#Add Season 
death_temps$szn<-NA
death_temps$szn[death_temps$iso_week >= 50 |death_temps$iso_week<= 10]<-'Winter(ish)'
death_temps$szn[death_temps$iso_week >= 11 &death_temps$iso_week<= 23]<-'Spring(ish)'
death_temps$szn[death_temps$iso_week >= 24 &death_temps$iso_week<= 36]<-'Summer(ish)'
death_temps$szn[death_temps$iso_week >= 37 &death_temps$iso_week<= 49]<-'Fall(ish)'

#Ok Correlate Again 
cor_death_szn <- death_temps %>%
  group_by(NUTSID, szn) %>%
  summarise(avg_death_c = cor(avg_temp, stn_death), wgt_death_c = cor(wgt_temp, stn_death), log_death_c = cor(wgt_logtemp, stn_death), age_death_c = cor(wgt_65plstemp, stn_death))

cor_temp_szn <- death_temps %>%
  group_by(NUTSID, szn) %>%
  summarise(avg_wgt = cor(wgt_temp, avg_temp),avg_log = cor(wgt_logtemp, avg_temp),avg_age = cor(wgt_65plstemp, avg_temp))

#Interesting stuff is happening so do week #
cor_death_wk <- death_temps %>%
  group_by(NUTSID, iso_week) %>%
  summarise(avg_death_c = cor(avg_temp, stn_death), wgt_death_c = cor(wgt_temp, stn_death), log_death_c = cor(wgt_logtemp, stn_death), age_death_c = cor(wgt_65plstemp, stn_death))

cor_temp_wk <- death_temps %>%
  group_by(NUTSID, iso_week) %>%
  summarise(avg_wgt = cor(wgt_temp, avg_temp),avg_log = cor(wgt_logtemp, avg_temp),avg_age = cor(wgt_65plstemp, avg_temp))

#Export the Files 

#Repeat Cor_Temp for Daily Data 
#Read in Daily Temp Data
daily_temp<-read.csv(file = '/Users/amandanorton/Desktop/Temps/Daily_Temp.csv')

#daily correlations 
cor_temp_daily <- daily_temp %>%
  group_by(NUTSID) %>%
  summarise(avg_wgt = cor(wgt_temp, avg_Temp),avg_log = cor(wgt_logtemp, avg_Temp),avg_age = cor(wgt_65plstemp, avg_Temp))

#Add Difference Measurements 
#add % difference measurements (wgt)
daily_temp$wgt_dif_abs<-(abs(daily_temp$avg_Temp-daily_temp$wgt_temp))
daily_temp$wgt_dif_abs_pct<-((daily_temp$wgt_dif_abs/(abs(daily_temp$avg_Temp)))*100)

#add % difference measurements (log)
daily_temp$log_dif_abs<-(abs(daily_temp$avg_Temp-daily_temp$wgt_logtemp))
daily_temp$log_dif_abs_pct<-((daily_temp$log_dif_abs/(abs(daily_temp$avg_Temp)))*100)

#add % difference measurements (age)
daily_temp$age_dif_abs<-(abs(daily_temp$avg_Temp-daily_temp$wgt_65plstemp))
daily_temp$age_dif_abs_pct<-((daily_temp$age_dif_abs/(abs(daily_temp$avg_Temp)))*100)

#Add Seasons (this is just the month)
#Spring March 20 to June 20
#Summer June 21 to September 22 
#Fall September 23 to December 20
#Winter December 21 to March 19  

daily_temp$szn<-NA
daily_temp$szn<-season(daily_temp$Date)

#Daily 
#Now Correlate 
cor_temp_daily_szn <- daily_temp %>%
  group_by(NUTSID,szn) %>%
  summarise(avg_wgt = cor(wgt_temp, avg_Temp),avg_log = cor(wgt_logtemp, avg_Temp),avg_age = cor(wgt_65plstemp, avg_Temp))

#Interesting stuff is happening so do week #
cor_temp_daily_wk <- daily_temp %>%
  group_by(NUTSID,iso_week) %>%
  summarise(avg_wgt = cor(wgt_temp, avg_Temp),avg_log = cor(wgt_logtemp, avg_Temp),avg_age = cor(wgt_65plstemp, avg_Temp))

#Add on Land Type 



#Add on Season
dif_temp_daily_szn <- daily_temp %>%
  group_by(NUTSID,szn) %>%
  summarise(wgt_dif_abs_pct = mean(wgt_dif_abs_pct),log_dif_abs_pct = mean(log_dif_abs_pct), age_dif_abs_pct = mean(age_dif_abs_pct))

?boxplot
describeBy(dif_temp_daily_szn, group = dif_temp_daily_szn$szn)
boxplot(dif_temp_daily_szn$wgt_dif_abs_pct~dif_temp_daily_szn$szn, col ='lightblue',xlab = 'Season', ylab = 'Wgt Pct Dif', ylim = c(0,40))
boxplot(dif_temp_daily_szn$log_dif_abs_pct~dif_temp_daily_szn$szn, col ='cornflowerblue',xlab = 'Season', ylab = 'Log Pct Dif', ylim = c(0,40))
boxplot(dif_temp_daily_szn$age_dif_abs_pct~dif_temp_daily_szn$szn, col ='darkblue',xlab = 'Season', ylab = 'Age Pct Dif', ylim = c(0,80))

#Repeat with Land Type
#First Merge on NUTS_3P
nuts_land<-merge(daily_temp, NUTS_3P, by.x = 'NUTSID', by.y = 'NUTS_ID', all.x = TRUE)

#Add a land type variable name 
nuts_land$land<-NA
nuts_land$land[nuts_land$MOUNT_TYPE == 2 & nuts_land$URBN_TYPE == 1 & nuts_land$COAST_TYPE == 1]<-'211'
nuts_land$land[nuts_land$MOUNT_TYPE == 2 & nuts_land$URBN_TYPE == 1 & nuts_land$COAST_TYPE == 3]<-'213'
nuts_land$land[nuts_land$MOUNT_TYPE == 2 & nuts_land$URBN_TYPE == 2 & nuts_land$COAST_TYPE == 1]<-'221'
nuts_land$land[nuts_land$MOUNT_TYPE == 2 & nuts_land$URBN_TYPE == 2 & nuts_land$COAST_TYPE == 3]<-'223'

nuts_land$land[nuts_land$MOUNT_TYPE == 2 & nuts_land$URBN_TYPE == 3 & nuts_land$COAST_TYPE == 3]<-'233'
nuts_land$land[nuts_land$MOUNT_TYPE == 3 & nuts_land$URBN_TYPE == 2 & nuts_land$COAST_TYPE == 3]<-'323'
nuts_land$land[nuts_land$MOUNT_TYPE == 3 & nuts_land$URBN_TYPE == 3 & nuts_land$COAST_TYPE == 1]<-'331'
nuts_land$land[nuts_land$MOUNT_TYPE == 3 & nuts_land$URBN_TYPE == 3 & nuts_land$COAST_TYPE == 3]<-'333'

nuts_land$land[nuts_land$MOUNT_TYPE == 4 & nuts_land$URBN_TYPE == 1 & nuts_land$COAST_TYPE == 1]<-'411'
nuts_land$land[nuts_land$MOUNT_TYPE == 4 & nuts_land$URBN_TYPE == 1 & nuts_land$COAST_TYPE == 3]<-'413'
nuts_land$land[nuts_land$MOUNT_TYPE == 4 & nuts_land$URBN_TYPE == 2 & nuts_land$COAST_TYPE == 1]<-'421'
nuts_land$land[nuts_land$MOUNT_TYPE == 4 & nuts_land$URBN_TYPE == 2 & nuts_land$COAST_TYPE == 3]<-'423'

nuts_land$land[nuts_land$MOUNT_TYPE == 4 & nuts_land$URBN_TYPE == 3 & nuts_land$COAST_TYPE == 1]<-'431'
nuts_land$land[nuts_land$MOUNT_TYPE == 4 & nuts_land$URBN_TYPE == 3 & nuts_land$COAST_TYPE == 3]<-'433'

#Add on Season
dif_temp_daily_nuts <- nuts_land %>%
  group_by(NUTSID,land) %>%
  summarise(wgt_dif_abs_pct = mean(wgt_dif_abs_pct),log_dif_abs_pct = mean(log_dif_abs_pct), age_dif_abs_pct = mean(age_dif_abs_pct))

boxplot(dif_temp_daily_nuts$wgt_dif_abs_pct~dif_temp_daily_nuts$land, col ='lightblue',xlab = 'Land Descript', ylab = 'Wgt Pct Dif', ylim = c(0,80),cex=0.0001)
boxplot(dif_temp_daily_nuts$log_dif_abs_pct~dif_temp_daily_nuts$land, col ='cornflowerblue',xlab = 'Land Descript', ylab = 'Log Pct Dif', ylim = c(0,80),cex=0.0001)
boxplot(dif_temp_daily_nuts$age_dif_abs_pct~dif_temp_daily_nuts$land, col ='darkblue',xlab = 'Land Descript', ylab = 'Age Pct Dif', ylim = c(0,100),cex=0.0001)





death_temps_sb<-subset(death_temps, select=c(szn,stn_death,crude_death, wgt_dif_abs:age_dif_abs_pct))
daily_temps_sb<-subset(daily_temp, select=c(szn,avg_Temp:wgt_65plstemp,wgt_dif_abs:age_dif_abs_pct))



#Summary 
describe(death_temps_sb)
describe(daily_temps_sb)

describeBy(death_temps_sb, group = death_temps_sb$szn)


#Export the Files
write.csv(death_temps, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/death_temp_pctdif_upd.csv')
write.csv(cor_temp, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_temp_upd.csv')
write.csv(cor_death, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_death_upd.csv')
write.csv(cor_death_szn, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_death_szn_upd.csv')
write.csv(cor_death_wk, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_death_wk_upd.csv')

#Export the Files
write.csv(daily_temp, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/daily_temp_pctdif_upd.csv')
write.csv(cor_temp_daily, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_temp_daily_upd.csv')
write.csv(cor_temp_daily_szn, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_temp_daily_szn_upd.csv')
write.csv(cor_temp_daily_wk, file='/Users/amandanorton/Desktop/Standardized Deaths/Correlations/cor_temp_daily_wk_upd.csv')

