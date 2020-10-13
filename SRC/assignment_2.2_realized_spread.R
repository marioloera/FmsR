# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# 1 ---------------------------------
# https://youtu.be/KI0KBZcty18
# load data saved in tutorial 1
load("Data/tqAssigment2")
# tq: 789196 obs. of 16 variables

"
  make a supset using only Trades obsrvations
  and where the dirrecion of the trade different than 0
"

tt = tq[tq$Type == "Trade" & tq$D != 0, 
        c("Date", "Seconds", "se", "Price", "Volume", "D", "Q", "Mid.Price")];
# tt: 198893 obs. of 8 variables

# match trades with future quotes
# install.packages("data.table")
library(data.table)

lagPrices = function(trades = tt, quotes = tq, delay = 60 * 5, variable = "Mid.Price"){
  cq = c("Date", "Seconds", "Mid.Price", "Ask.Price", "Bid.Price");
  dq = data.table(quotes[quotes$Type == "Quote", cq]);
  
  ct = c("Date", "Seconds", "Price", "Volume", "D", "Mid.Price");
  dt = data.table(trades[, ct], key = c("Date", "Seconds"));
  
  setkey(dq, cols = Date, Seconds);
  ix = dq[dt, roll = T, which = T, mult = "last"];
  
  # eval(parse(text = paste0("dt$Delay.",delay,".",variable,"=dq$",variable,"[ix]")));
  # without the delay 
  eval(parse(text = paste0("dt$Delay.",variable,"=dq$",variable,"[ix]")));
  
  return(as.data.frame(dt));
}

tt_Lead = lagPrices(trades = tt, quotes = tq, delay = 60 * 1, variable = "Mid.Price")

# realize spread after one minute
tt$sr = tt$se - abs(tt_Lead$Delay.Mid.Price - tt_Lead$Price)/tt_Lead$Price
# 198893 obs, of 9 variables


# get traiding days in a vector
trading.days = levels(factor(tq$Date))

# define a data frame report
report <- data.frame(
  day = trading.days,
  obs = NaN,
  vol.sek = NaN, # trade volueme sek
  s.bsp = NaN, # quoted spread bsp 
  w_s.bsp = NaN, #  weighted quoted spread bsp
  se.bsp = NaN, #  effective spread bsp
  w_se.bsp = NaN, # weighted effective spread bsp
  ilr.bsp = NaN # illiquidity ratio bsp
)

# calculate meas for each traiding day
for(day in trading.days){
  
  # save data for one day in td
  td = tq[tq$Date == day, ]
  
  # transform day to index
  i = which(trading.days %in% day)
  
  # observation per day
  report$obs[i] = count(td$Date)[[2]]
  
  # b. The total trading volume, reported in million SEK
  report$vol.sek[i] = sum((td$Volume * td$Price), na.rm = 'TRUE')
  
  # a. The quoted spread, reported in basis points
  report$s.bsp[i] = 10000 * mean(td$s, na.rm = 'TRUE') 
  
  # a.* weighted quoted spread
  report$w_s.bsp[i] =
    10000 * weighted.mean(td$s, c(diff(td$Seconds), 0) , na.rm = T)

  # c. The effective spread, reported in basis points
  report$se.bsp[i] = 10000 * mean(td$se, na.rm = 'TRUE')
  
  # c.* weighted effective spread
  report$w_se.bsp[i] = 10000 * weighted.mean(td$se, td$Volume, na.rm = 'TRUE')
  
  # d. The illiquidity ratio by Amihud (2002), 
  # reported in percent per million SEK
  # TODO
  # report$ilr.bsp[i] =
}

report