# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/Data")

library(plyr) # install.packages('plyr')


# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

load("ttAssigment2_zadka")
# tt: 198893 obs. of 9 variables

"
  get the week, 
    conver tq$Date string in '%Y%m%d' format ex: 20200313
    to Date class, format '%Y-%m-%d' format ex: 2020-03-13
    get the week, fromat '%V'
" 
tt$week = strftime(as.Date(as.character(tt$Date), format = "%Y%m%d"), format = "%V")
# tt: 198893 obs. of 10 variables
# table(tt$week, tt$Date)

# get weeks in a vector
weeks = levels(factor(tt$week))


"
save original data frame in from tt in tt_all_data
so we can reuse tt for each week with the same formulas 
tt_all_data: 198893 obs. of 10 variables
"
tt_all_data = tt

# data frame for weeks data
report_per_week <- data.frame(
  order = NaN,
  week = weeks,
  obs = NaN,
  zadka_A = NaN,
  zadka_B = NaN
)



# calculate meas for each traiding day
for(week in weeks){

  # transform week to index
  i = which(weeks %in% week)
  report_per_week$order[i] = i
  
  # week rows
  tt = tt_all_data[tt_all_data$week == week, ]
  
  # observation per week, count give a distinct per value (x, freq)
  report_per_week$obs[i] = count(tt$week)[[2]]
  
  # zadkaA
  
  # eta represents unexpected find order flow
  tt$eta = ar(tt$Q)$resid;
  
  #psi represents unexpected direction of trade
  tt$psi = tt$D - (1 - 2 * pnorm(- (tt$Q - tt$eta) / sd(tt$eta, na.rm=T)));
  
  sadkaA = lm(dP ~ psi + eta + dD + dQ, data = tt)

  # zadkaB do the same model using returns instead of price change
  tt$R = c(NA, diff(log(tt$Price)))
  
  sadkaB = lm(R ~ psi + eta + dD + dQ, data = tt)
  
  report_per_week$zadka_A[i] = sadkaA
  report_per_week$zadka_B[i] = sadkaB
  


}
report_per_week

summary(report_per_week$zadka_A[[1]])


# zadka_A[1] first week
report_per_week$zadka_A[[1]]

# zadka_A[1] first week, columns 2:4 (psi, eta, dD)
report_per_week$zadka_A[[1]][2:4]

report_per_week$zadka_A[[1]][c('psi')]

