# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))


# 1 ---------------------------------
# https://youtu.be/uVgwDXAdTg0
# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# loading data
load("2011-08-ABB.ST")


# 2 ---------------------------------
# https://youtu.be/4FWbUh0T2U4
# 2.a
head(tq)

# 2.b
tail(tq) 

# 2.c
table(tq$Date)
table(tq$Type)

# 2.d
hist(tq$Price)
hist(tq$Volume)
hist(tq$Volume, 50)

# 3 ---------------------------------
# https://youtu.be/sH0fvz8p618

" filter by time exlude obeservation outside 
  market oppening hours 9:00 - 17:25 "

# create a variable for time
time = strptime(tq$Time, format="%H:%M:%OS"); 

# add a column in tq with secconds
tq$Seconds = time$hour*3600 + time$min*60 + time$sec;

# filter by time
tq = tq[tq$Seconds > (9*3600) & tq$Seconds < (17*3600 + 25*60),]
head(tq[,2])
tail(tq[,2])

# filter by mechanism and Type
tq = tq[tq$mechanism == 'cont' | tq$Type == 'Quote',]
# tq: 200728 obs. of 11 variables

# 4 ---------------------------------

# https://youtu.be/Y7_SjC1rnOg

" Calculate relative quoted spread "

# add column Abs.spread, mid price
# a = ask price
# b = bid price
# S = Absolute Spread  = a - b
# m = mid.price = (a+b)/2


mean(tq$Ask.Price) # NA
mean(tq$Ask.Price , na.rm = 'TRUE') # 127.3869


tq$Abs.Spread = tq$Ask.Price-tq$Bid.Price
tq$Mid.Price = (tq$Ask.Price+tq$Bid.Price)/2
# tq: 200728 obs. of 13 variables


# s = relative spread = S/m 
# average relative spread in basis points
# filter na.rm = 'TRUE'
10000 * mean(tq$Abs.Spread / tq$Mid.Price, na.rm = 'TRUE') 
# 11.47669



# **** weighted ave relative spread
"
  weighted.mean(x, w, .)
  
  diff {base}	Lagged Differences
  
  function diff(tq$seconds) gives the diff between row_n and row_n+1
"
# method 1:  adding 0 at the of diff 
10000 * weighted.mean(tq$Abs.Spread/tq$Mid.Price, append(diff(tq$Seconds),0,NROW(tq)-1), na.rm = 'TRUE')

# method 2: take observation 0 to N-1
10000 * weighted.mean((tq$Abs.Spread/tq$Mid.Price)[1:NROW(tq)-1], diff(tq$Seconds), na.rm = 'TRUE')
# 9.924343

# 5 ----------------------------------
# https://youtu.be/Cro4WTrmVNI


"
 na.locf Last Observation Carried Forward
 Generic function for replacing each NA
 with the most recent non-NA prior to it.
 from package zoo
 
 ******* 
  need to be connected to internet
  in R studio right bottom area, commonly know as  4 - Plots and files
  click on Tools -> Install Packages 
  Install from: Repository (CRAN)
  Packages (separet multiple with space or comma): zoo

"
library(zoo)

"
 Se = efective spread
 p = transaction price 
 m = mid price
 d = direction of the trade: +1 for buy, -1 for sell
Lee-Ready algorithm:  
 if P > m  then d= 1  a buyer initiated the trade by crossing the spread
 if P < m  then d=-1  a seller init the trade by crossing the spread
 Se = d(p-m)/m  = abs(p-m)/m
 abs(transaction Price - Mid.Price)
"

# fill the gaps in the Mid.Price
tq$Mid.Price = na.locf(tq$Mid.Price);

# get the direction of the trade
tq$D = sign(tq$Price - tq$Mid.Price)
#this can be useful to illustrate if sellers/ buyers 
#had initiated the trades 
table(tq$D)
"-1     0     1 
10827   832  8718"

# effective spread
10000 * mean(tq$D * (tq$Price - tq$Mid.Price) / tq$Mid.Price, na.rm = 'TRUE')
10000 * mean(abs(tq$Price - tq$Mid.Price) / tq$Mid.Price, na.rm = 'TRUE')
# in order to compare quoted spread vs effective spread need to multiple effective spread * 2

# weighted effective spread with volume
10000 * weighted.mean((abs(tq$Price - tq$Mid.Price) / tq$Mid.Price), tq$Volume, na.rm = 'TRUE')
# 4.567386


# 6 ----------------------------------
# https://youtu.be/5XGY9Ree7Jk