# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/Data")

# packages needed
library(plyr) # install.packages('plyr')
library(zoo) # install.packages('zoo')
library(lattice); # install.packages('lattice')

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# loading data
load("SKFb_Mar2020")
# tq: 810224 obs. of 9 variables

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
  cat("\nOutliers identified:", na2 - na1, "n")
  cat("\nPropotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("\nMean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("\nMean without removing outliers:", round(m1, 2), "n")
  cat("\nMean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("\nOutliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("\nNothing changed", "n")
    return(invisible(var_name))
  }
}

par(mar=c(1,1,1,1)) 
outlierKD(tq,Price)
y
outlierKD(tq,Volume)
y

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

save(tq, file ='tqAssigment2')
# tq: 780106 obs. of 16 variables
