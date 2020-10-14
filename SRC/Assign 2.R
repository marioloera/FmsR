# set working directory
#setwd("C:/Users/Alexander/Downloads")
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/Data")

# packages needed
library(plyr) # install.packages('plyr')
library(zoo) # install.packages('zoo')
library(lattice);

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# loading data
load("SKFb_Mar2020")
# tq: 810224 obs. of 9 variables

# create a variable for time
time = strptime(tq$Time, format="%H:%M:%OS"); 


hist(tq$Price)


plot(tq$Volume, type = 'l')
plot(tq$Date, type = 'l')


plot(tq$Date,tq$Price, type = 'l', xlab= "Date", ylab="Price" ) # Just an sketch





# add a column in tq with seconds
tq$Seconds = time$hour*3600 + time$min*60 + time$sec;
tq = tq[tq$Seconds > (8*3600 + 5*60) & tq$Seconds < (16*3600 + 25*60),]
# tq: 780106 obs. of 10 variables



outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
# tq: 780106 obs. of 10 variables
outlierKD(tq,Price)
y
outlierKD(tq,Volume)
y







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

# tq: 780106 obs. of 16 variables
# calculate meas for each traiding day



# add Absolute Spread
tq$Abs.Spread = tq$Ask.Price-tq$Bid.Price

# add Mid Price and Locf
tq$Mid.Price = (tq$Ask.Price+tq$Bid.Price)/2
tq$Mid.Price = na.locf(tq$Mid.Price);
# tq: 168391 obs. of 12 variables

# get traiding days in a vector
trading.days = levels(factor(tq$Date))

rm(report)
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
report
# report 5 obs. of 8 variables

# calculate meas for each traiding day
for(day in trading.days){
  
  # save data for one day in td
  td = tq[tq$Date == day, ]
  
  # transform day to index
  i = which(trading.days %in% day)
  
  # observation per day
  report$obs[i] = count(td$Date)[2][1]
  
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
  # TODO
}
report

plot(report$s.bsp, type="l", xlab="day", ylab="quoted spread bsp" )

