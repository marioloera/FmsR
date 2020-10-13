# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/Data")

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

load("tqAssigment2")
# tq: 780106 obs. of 10 variables


hist(tq$Price)
plot(tq$Price , type = 'l')
plot(tq$se, type = 'l')

plot(tq$Volume, type = 'l')
plot(tq$s, type = 'l')

x = tq[1:10000, ];

plot(x$Price, type = 'l')

# packages needed
library(lattice) # install.packages('lattice')

require(lattice)
xyplot(x ~ y + z, data=df, type = c('l','l'), col = c("blue", "red"), auto.key=T)

xyplot(Seconds ~ Ask.Price + Bid.Price, data=x, type = c('l','l'), col = c("blue", "red"), auto.key=T)

xyplot( Ask.Price ~  Seconds, data=x, type = c('l','l'), col = c("blue", "red"), auto.key=T)

xyplot( Ask.Price + Bid.Price ~  Seconds, data=x, type = c('l','l'), col = c("blue", "red"), auto.key=T)

xyplot( Ask.Price + Bid.Price + Mid.Price ~  Seconds, data=x, type = c('l','l'), col = c("blue", "red"), auto.key=T)

xyplot( Ask.Price + Abs.Spread ~  Seconds, data=x, type = c('l','l'), col = c("blue", "red"), auto.key=T)
xyplot(  Abs.Spread ~  Seconds, data=x, type = c('l','l'), col = c("blue", "red"), auto.key=T)

xyplot( Ask.Price ~  Seconds, data=tq, type = c('l','l'), col = c("blue", "red"), auto.key=T)
xyplot( Abs.Spread ~  Seconds, data=tq, type = c('l','l'), col = c("blue", "red"), auto.key=T)

outlierKD(tq, Price)
 