
# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# loading data
load("2011-08-ABB.ST")
# tq: 201285 obs. of 10 variables

# create a variable for time
time = strptime(tq$Time, format="%H:%M:%OS"); 

# add a column in tq with seconds
tq$Seconds = time$hour*3600 + time$min*60 + time$sec;

tq = tq[tq$Seconds > (9*3600 + 5*60) & tq$Seconds < (17*3600 + 25*60),]

tq$Abs.Spread = tq$Ask.Price-tq$Bid.Price
tq$Mid.Price = (tq$Ask.Price+tq$Bid.Price)/2
library(zoo)
tq$Mid.Price = na.locf(tq$Mid.Price);
head(tq)

tail(tq) 


head(tq[tq$Type == 'Trade' & tq$Date == '20110822' , c("Type", "Time", "mechanism")], 20)


tq[tq$mechanism == 'cont' & tq$Date == '20110822', c("Type", "Time", "mechanism")]


table(tq$Date)
table(tq$Type, useNA="always")
table(tq$mechanism, useNA="always")
table(tq$Type, tq$mechanism, useNA="always")



# unique values of column
trading.days = levels(factor(tq$Date))
trading.days[1]


trading.days

# if they can be used in loop
head(tq[tq$Date == '20110822', ] )


intervals=seq(9,17,0.5)
intervals
# define a data frame k
k <- data.frame(
    intervals=intervals,
    a=NA,
    b=NA
)
k

# define a data frame report
report <- data.frame(
  day = trading.days,
  obs = NA,
  vol.sek = NA, # trade volueme sek
  s.bsp = NA, # quoted spread bsp 
  w_s.bsp = NA, #  weighted quoted spread bsp
  se.bsp = NA, #  effective spread bsp
  w_se.bsp = NA, # weighted effective spread bsp
  ilr.bsp = NA # illiquidity ratio bsp
)
report
    

# install.packages('plyr')
library(plyr)


table(tq$Date)
for(day in trading.days){
  # save data for one day in td
  td = tq[tq$Date == day, ]
  
  # transform day to index
  i = which(trading.days %in% day)
  
  # observation per day
  report$obs[i] = count(td$Date)[2]
  
  # a. The quoted spread, reported in basis points
  report$s.bsp[i] = 10000 * mean((td$Abs.Spread / td$Mid.Price), na.rm = 'TRUE') 
  
  # a.* weighted quoted spread
  report$w_s.bsp[i] = 10000 * weighted.mean((td$Abs.Spread/td$Mid.Price)[1:NROW(td)-1], diff(td$Seconds), na.rm = 'TRUE')

  # b. The total trading volume, reported in million SEK
  report$vol.sek[i] = sum((td$Volume * td$Price), na.rm = 'TRUE')
  
  # c. The effective spread, reported in basis points
  report$se.bsp[i] = 10000 * mean(abs(td$Price - td$Mid.Price) / td$Mid.Price, na.rm = 'TRUE')
  
  # c.* weighted effective spread
  report$w_se.bsp[i] = 10000 * weighted.mean((abs(tq$Price - tq$Mid.Price) / tq$Mid.Price), tq$Volume, na.rm = 'TRUE')
  
  
  # d. The illiquidity ratio by Amihud (2002), reported in percent per million SEK

}
report

for(day.num in trading.days){
  # transform day to index
  i = which(trading.days %in% day)
  # observation per day
  report$obs[i] = count(tq$Date[tq$Date == day])[2]
  # a. The quoted spread, reported in basis points
  report$quoted.spread.bsp[i] = 
    10000 * mean((tq$Abs.Spread / tq$Mid.Price)[tq$Date == day], na.rm = 'TRUE') 
  # a.* weighted quoted spread
  #report$w.quoted.spread.bsp[i] = 
  #  10000 * weighted.mean((tq$Abs.Spread/tq$Mid.Price)[tq$Date == day][1:NROW(tq)-1], diff(tq$Seconds), na.rm = 'TRUE')
  # b. The total trading volume, reported in million SEK
  report$vol.sek[i] = 
    sum((tq$Volume * tq$Price)[tq$Date == day], na.rm = 'TRUE')
  # c. The effective spread, reported in basis points
  # c.* weighted effective spread
  # d. The illiquidity ratio by Amihud (2002), reported in percent per million SEK
}
report

for (i in seq_along(trading.days)){
  # transform day to index
  day = trading.days[i]
  report$vol.sek[i] = sum((tq$Volume * tq$Price)[tq$Date == day], na.rm = 'TRUE')
}

report[,1:2]


report

report$obs[i] =
  
count(tq$Date[tq$Date == 20110822,] )

count(tq$Date )

count(tq$Date[tq$Date == 20110822])[2]


mean(tq$Date )

x = count(tq )
x
