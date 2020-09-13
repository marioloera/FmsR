# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# packages needed
library(plyr) # install.packages('plyr')
library(zoo) # install.packages('zoo')

# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# loading data
load("SKFb_Mar2020_1week")
# tq: 194141 obs. of 9 variables

" filter by time exlude obeservation outside 
  market oppening hours 9:05 - 17:25 GTE
  our data is in CTE time
  "

# create a variable for time
time = strptime(tq$Time, format="%H:%M:%OS"); 

# add a column in tq with seconds
tq$Seconds = time$hour*3600 + time$min*60 + time$sec;
tq = tq[tq$Seconds > (8*3600 + 5*60) & tq$Seconds < (16*3600 + 25*60),]
# tq: 168391 obs. of 10 variables


# Preview data
head(tq)
tail(tq) 
hist(tq$Price)
hist(tq$Volume, 100)

plot(tq$Seconds, type = 'l')
plot(tq$Price, type = 'l')
plot(tq$Volume, type = 'l')

# add Absolute Spread
tq$Abs.Spread = tq$Ask.Price - tq$Bid.Price

# add Mid Price and Locf
tq$Mid.Price = (tq$Ask.Price + tq$Bid.Price) / 2
tq$Mid.Price = na.locf(tq$Mid.Price);
# tq: 168391 obs. of 12 variables

# add relative spread s
tq$s = tq$Abs.Spread / tq$Mid.Price

# add effective spread se
tq$se = abs(tq$Price - tq$Mid.Price) / tq$Mid.Price

# get traiding days in a vector
trading.days = levels(factor(tq$Date))

# define a data frame report
report <- data.frame(
  day = trading.days,
  obs = 0,
  vol.sek = NA, # trade volueme sek
  s.bsp = NA, # quoted spread bsp 
  w_s.bsp = NA, #  weighted quoted spread bsp
  se.bsp = NA, #  effective spread bsp
  w_se.bsp = NA, # weighted effective spread bsp
  ilr.bsp = NA # illiquidity ratio bsp
)

# calculate meas for each traiding day
for(day in trading.days){
  
  # save data for one day in td
  td = tq[tq$Date == day, ]
  
  # transform day to index
  i = which(trading.days %in% day)
  
  # observation per day
  report$obs[i] = count(td$Date)[[2]]
  
  # a. The quoted spread, reported in basis points
  report$s.bsp[i] = 10000 * mean(td$s, na.rm = 'TRUE') 
  
  # a.* weighted quoted spread
  report$w_s.bsp[i] =
    10000 * weighted.mean(td$s[1:NROW(td)-1], diff(td$Seconds), na.rm = 'TRUE')
  
  # b. The total trading volume, reported in million SEK
  report$vol.sek[i] = sum((td$Volume * td$Price), na.rm = 'TRUE')
  
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
# report 5 obs. of 8 variables

"
  avgweek.report average weekly report
  using weighted mean on the obs per day
  similar structure as report 
  exlude columns first two : day, obs
  start in column 3, using n 
" 
n = 3 
avgweek.report = report[1, n:(ncol(report))]

for(i in 1:dim(avgweek.report)[2]) {
  avgweek.report[i] = weighted.mean(report[,n-1+i], report$obs, na.rm = 'TRUE')
}
avgweek.report
# avgweek.report 1 obs. of 6 variables
