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

#filter by mechanism and Type
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
avg.s = 10000 * mean(tq$Abs.Spread / tq$Mid.Price, na.rm = 'TRUE') 
avg.s # 11.47669



# **** weighted ave relative spread
"
  weighted.mean(x, w, .)
  
  diff {base}	Lagged Differences
  
  function diff(tq$seconds) gives the diff between row_n and row_n+1
"
# method 1:  adding 0 at the of diff 
10000 * weighted.mean(tq$Abs.Spread/tq$Mid.Price,append(diff(tq$Seconds),0,NROW(tq)-1), na.rm = 'TRUE')

# method 2: take observation 0 to N-1
10000 * weighted.mean((tq$Abs.Spread/tq$Mid.Price)[1:NROW(tq)-1],diff(tq$Seconds), na.rm = 'TRUE')
#9.924343
