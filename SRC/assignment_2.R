# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# packages needed
library(plyr) # install.packages('plyr')
library(zoo) # install.packages('zoo')

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# loading data
load("Data/SKFb_Mar2020")
# tq: 810224 obs. of 9 variables

table(tq$Date, tq$Type)
