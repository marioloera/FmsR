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

"
 Use your coefficient estimates to obtain 
  the estimated effective spread, 
  the estimated adverse selection costs, 
  and the estimated order processing and inventory costs.
  
  Report your estimates in basis points relative to the midpoint
"
# data frame for weeks data
report_per_week <- data.frame(
  order = NaN,
  week = weeks,
  obs = NaN,
  asc_ = NaN, # " the estimated adverse selection costs, asc_ =  Lamda0*psi + Lamda1*eta "
  opc_ic_ = NaN, # " and the estimated order processing and inventory costs. opc_ic_ = gamma0*D + gama1*Q "
  se_ = NaN #  " the estimated effective spread   se_ = asc_ + opc_ic_ "
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
  
  # use SadkaA (nominal price differences)
  sadkaA = lm(dP ~ psi + eta + dD + dQ, data = tt)
  
  #get the coefficients
  lamda0 = sadkaA[["coefficients"]][c('psi')]
  lamda1 = sadkaA[["coefficients"]][c('eta')]
  gamma0 = sadkaA[["coefficients"]][c('dD')]
  gamma1 = sadkaA[["coefficients"]][c('dQ')]
  
  # asc_ =  Lamda0*psi + Lamda1*eta
  tt$asc_ = lamda0 * tt$psi + lamda1 * tt$eta
  
  # opc_ic_ = gamma0*D + gama1*Q
  tt$opc_ic_ = gamma0 * tt$D + gamma1 * tt$Q
  
  # se_ = d*(asc_ + opc_ic_) *** relative to the midpoint
  tt$se_ = tt$D * (tt$asc_ + tt$opc_ic_)
  
  # get the average per week
  report_per_week$lamda0[i] =  lamda0
  report_per_week$lamda1[i] =  lamda1
  report_per_week$gamma0[i] =  gamma0
  report_per_week$gamma1[i] =  gamma1
  report_per_week$asc_[i] =  mean(tt$asc_, na.rm = T)
  report_per_week$opc_ic_[i] =  mean(tt$opc_ic_, na.rm = T)
  report_per_week$se_[i] =  mean(tt$se_, na.rm = T)

  #report_per_week$zadka_A[i] = sadkaA
  #report_per_week$zadka_B[i] = sadkaB
}

report_per_week
