# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/Data")

# packages needed
library(plyr) # install.packages('plyr')
library(zoo) # install.packages('zoo')

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# loading data
load("SKFb_Mar2020")
# tq: 810224 obs. of 9 variables

# create a variable for time
time = strptime(tq$Time, format="%H:%M:%OS"); 

# add a column in tq with seconds
tq$Seconds = time$hour*3600 + time$min*60 + time$sec;
tq = tq[tq$Seconds > (8*3600 + 5*60) & tq$Seconds < (16*3600 + 25*60),]
# tq: 780106 obs. of 10 variables

# add Absolute Spread
tq$Abs.Spread = tq$Ask.Price-tq$Bid.Price

# add Mid Price and Locf
tq$Mid.Price = (tq$Ask.Price + tq$Bid.Price) / 2
tq$Mid.Price = na.locf(tq$Mid.Price);

# get the direction of the trade
tq$D = sign(tq$Price - tq$Mid.Price)

# order flow
tq$Q = tq$Volume * tq$D

# add relative spread s
tq$s = tq$Abs.Spread / tq$Mid.Price

# add effective spread se
tq$se = abs(tq$Price - tq$Mid.Price) / tq$Mid.Price

# save data with need it columns
tq = tq[, 
        c("Date", "Seconds", "Price", "Volume", "D", "Q", "se", "Mid.Price", 
          "s", "Abs.Spread")];
save(tq, file ='tqAssigment2')
# tq: 780106 obs. of 10 variables