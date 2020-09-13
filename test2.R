rm(list = setdiff(ls(), lsf.str()))

library(plyr) # install.packages('plyr')
library(zoo) # install.packages('zoo')

setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

load("SKFb_Mar2020_1week")

# Preview data
head(tq)
tail(tq) 

head(tq[,1:3],10)

head(tq[tq$Type=="Trade",1:3],10)
tail(tq[tq$Type=="Trade",1:3],10)


time = strptime(tq$Time, format="%H:%M:%OS"); 

tq$Seconds = time$hour*3600 + time$min*60 + time$sec;



tq = tq[tq$Seconds > (9*3600 + 5*60) & tq$Seconds < (17*3600 + 25*60),]


tq$Abs.Spread = tq$Ask.Price-tq$Bid.Price

tq$Mid.Price = (tq$Ask.Price+tq$Bid.Price)/2
tq$Mid.Price = na.locf(tq$Mid.Price);
trading.days = levels(factor(tq$Date))

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

"
  avgweek.report average weekly report
  using weighted mean on the obs per day
  similar structure as report 
  exlude columns first two : day, obs
  start in column 3, using n 
" 
n = 3 
avgweek.report = report[1, n:(ncol(report))]

for(day in trading.days){
  td = tq[tq$Date == day, ]

  i = which(trading.days %in% day)
  
  report$obs[i] = count(td$Date)[[2]]
  
  report$s.bsp[i] = 10000 * mean((td$Abs.Spread / td$Mid.Price), na.rm = 'TRUE') 
  
  report$w_s.bsp[i] = 10000 * weighted.mean((td$Abs.Spread/td$Mid.Price)[1:NROW(td)-1], diff(td$Seconds), na.rm = 'TRUE')

  report$vol.sek[i] = sum((td$Volume * td$Price), na.rm = 'TRUE')
  
  report$se.bsp[i] = 10000 * mean(abs(td$Price - td$Mid.Price) / td$Mid.Price, na.rm = 'TRUE')
  
  report$w_se.bsp[i] = 10000 * weighted.mean((abs(tq$Price - tq$Mid.Price) / tq$Mid.Price), tq$Volume, na.rm = 'TRUE')
}

# compute the weighted average
avgweek.report$s.bsp = weighted.mean(report$s.bsp, report$obs, na.rm = 'TRUE')
avgweek.report$w_s.bsp = weighted.mean(report$w_s.bsp, report$obs, na.rm = 'TRUE')
avgweek.report$vol.sek = weighted.mean(report$vol.sek, report$obs, na.rm = 'TRUE')
avgweek.report$se.bsp = weighted.mean(report$se.bsp, report$obs, na.rm = 'TRUE')
avgweek.report$w_se.bsp = weighted.mean(report$w_se.bsp, report$obs, na.rm = 'TRUE')

w2 = report[1, 3:(ncol(report))]
for(i in 1:dim(avgweek.report)[2]) {
  w2[i] = 0
}
avgweek.report
w2
for(i in 1:dim(avgweek.report)[2]) {
  w2[i] = weighted.mean(report[,n-1+i], report$obs, na.rm = 'TRUE')
}
avgweek.report
w2
