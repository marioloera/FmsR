
# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# loading data
load("2011-08-ABB.ST")
# tq: 201285 obs. of 10 variables

tq$Abs.Spread = tq$Ask.Price-tq$Bid.Price
tq$Mid.Price = (tq$Ask.Price+tq$Bid.Price)/2

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
  vol.sek = NA,
  quoted.spread.bsp = NA,
  eff.spread.bsp = NA,
  weight.eff.spread.bsp = NA,
  illiquidity.ratio.bsp = NA
)
report
    

# install.packages('plyr')
library(plyr)



table(tq$Date)
for(day in trading.days){
  # transform day to index
  i = which(trading.days %in% day)
  
  # observation per day
  count(tq$Date[tq$Date == day])[2]
  
  
  # a. The quoted spread, reported in basis points
  report$quoted.spread.bsp[i] = 10000 * mean((tq$Abs.Spread / tq$Mid.Price)[tq$Date == day], na.rm = 'TRUE') 
  
  # b. The total trading volume, reported in million SEK
  report$vol.sek[i] = sum((tq$Volume * tq$Price)[tq$Date == day], na.rm = 'TRUE')
  
  
  #report$vol.sek[i] = 5;
}



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
