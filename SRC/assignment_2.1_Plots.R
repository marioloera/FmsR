# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/Data")

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

load("tqAssigment2")
# tq: 780106 obs. of 10 variables


hist(tq$Price)
plot(tq$Price, type = 'l')
plot(tq$Volume, type = 'l')
plot(tq$s, type = 'l')
