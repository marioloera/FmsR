# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))


# 1 ---------------------------------
# https://youtu.be/KI0KBZcty18
# load data saved in tutorial 1
load("tqData")
# tq: 200728 obs. of 14 variables



tt = tq[tq$Type == "Trade" & tq$D != 0, c("Date", "Seconds", "Price", "Volume", "D")];
tt$Q = tt$Volume*tt$D
# tt: 19545 obs. of 6 variables


head(tq)
