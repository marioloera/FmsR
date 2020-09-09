
# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))


# 1 ---------------------------------
# https://youtu.be/uVgwDXAdTg0
# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# loading data
load("2011-08-ABB.ST")
# tq: 201285 obs. of 10 variables


head(tq)

tail(tq) 


head(tq[tq$Type == 'Trade' & tq$Date == '20110822' , c("Type", "Time", "mechanism")], 20)

head(tq[tq$mechanism == 'cont' & tq$Date == '20110822' , c("Type", "Time", "mechanism")], 20)

tq[tq$mechanism == 'cont' & tq$Date == '20110822', c("Type", "Time", "mechanism")]


table(tq$Date)

table(tq$Type, useNA="always")

table(tq$mechanism, useNA="always")

table(tq$Type, tq$mechanism, useNA="always")


table(tq$mechanism, useNA="always")
# tq = tq[tq$mechanism == 'cont' | tq$Type == 'Quote',]
trates = tq[tq$Type == 'Trade',]
table(trates$Type, trates$mechanism, useNA="always")

auct = trates[trates$mechanism == 'auct',]

cont = trates[trates$mechanism == 'cont',]


#
head(tq, 100)



# seems the auct happens first 
head(trates, 100)
head(auct, 15)
head(cont, 15)




# seems the auct happens last 

tail(trates, 100)

tail(auct, 453)
tail(cont, 15)

auct[auct$Date == '20110822',2]

# solution, get the time of the firta quote of the day after 9 am
