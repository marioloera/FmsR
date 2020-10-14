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
"

tt = tq[tq$Type == "Trade", 
        c("Date", "Seconds", "se", "Price", "Volume", "D", "Q", "Mid.Price")];
# tt: 205405 obs. of 9 variables

# match trades with future quotes
# install.packages("data.table")
library(data.table)

lagPrices = function(trades = tt, quotes = tq, delay = 60 * 5, variable = "Mid.Price"){
  cq = c("Date", "Seconds", "Mid.Price", "Ask.Price", "Bid.Price");
  dq = data.table(quotes[quotes$Type == "Quote", cq]);
  
  ct = c("Date", "Seconds", "Price", "Volume", "D", "Mid.Price", "se");
  dt = data.table(trades[, ct], key = c("Date", "Seconds"));
  
  setkey(dq, cols = Date, Seconds);
  ix = dq[dt, roll = T, which = T, mult = "last"];
  
  #eval(parse(text = paste0("dt$Delay.",delay,".",variable,"=dq$",variable,"[ix]")));
  # use variable with out delay qty
  eval(parse(text = paste0("dt$Delay.",variable,"=dq$",variable,"[ix]")));
  
  return(as.data.frame(dt));
}

# get Mid.Price after one minute
tt_1min = lagPrices(trades = tt, quotes = tq, delay = 60 * 1, variable = "Mid.Price")

# realize spread after one minute
tt$sr = tt$D*(tt$Price - tt_1min$Delay.Mid.Price) / tt$Mid.Price

# get traiding days in a vector
trading.days = levels(factor(tt$Date))

# define a data frame report
report <- data.frame(
  day = NaN,
  date = trading.days,
  w_s.bsp = NaN, # weighted quoted spread bsp 
  w_se.bsp = NaN, # weighted effective spread bsp
  w_sr.bsp = NaN # weighted realized spread bsp
)

# calculate meas for each traiding day
for(day in trading.days){

  # transform day to index
  i = which(trading.days %in% day)
  report$day[i] = i
  
  # tt_day = quote data for one day from tq
  tq_day = tq[tq$Date == day, ]
  
  report$w_s.bsp[i] =
    10000 * weighted.mean(tq_day$s, c(diff(tq_day$Seconds), 0) , na.rm = T)

  # tt_day: trade data for one day from tt
  tt_day = tt[tt$Date == day, ]
  
  # weighted effective spread
  report$w_se.bsp[i] = 10000 * weighted.mean(tt_day$se, tt_day$Volume, na.rm = T)
  
  # weighted realized spread
  report$w_sr.bsp[i] = 10000 * weighted.mean(tt_day$sr, tt_day$Volume, na.rm = T)
  
}

report
library(lattice) # install.packages('lattice')

require(lattice)

# sugested figure 1)  w_s(green) & w_se (blue)
xyplot(
  w_s.bsp + w_se.bsp ~  day,
  data = report,
  type = c('l','l'),
  col = c("green", "blue"),
  auto.key = T
)

# suggested figure 2) w_se (blue) & w_sr (red)
xyplot(
  w_se.bsp + w_sr.bsp ~  day,
  data = report,
  type = c('l','l'),
  col = c("blue", "red"),
  auto.key = T
  )


