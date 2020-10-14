# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# 1 ---------------------------------
# https://youtu.be/KI0KBZcty18
# load data saved in tutorial 1
load("tqAssigment2")
# tq: 200728 obs. of 15 variables

"
  make a supset using only Trades obsrvations
  and where the dirrecion of the trade different than 0
"

tt = tq[tq$Type == "Trade" & tq$D != 0, 
        c("Date", "Seconds", "Price", "Volume", "D", "Q")];


# calculate the difference, include NA for the first rows

tt$dP = c(NA, diff(tt$Price));
tt$dD = c(NA, diff(tt$D));
tt$dQ = c(NA, diff(tt$Q));
# tt: 19545 obs. of 9 variables

# 3 ---------------------------------
# https://youtu.be/GoNHYH7fot0
" Sadka 2006
  adverse selection cost should be derived from: 
    psi ( unexpecte elements of the direction of trade)
      psi_t = D_t - E(D_t)
      
    eta ( unexpecte elemenst of the sign order flow)
      n_t = Q_t - E(Q_t)
      is the residual of an autoregresive model of the Qt
      (regres a varaible in its past values, zadka used 5 times lags)
"

# auto regresive model
# need to find the lag order
ar(tt$Q) #all information
ar(tt$Q)$order

# eta represents unexpected find order flow
tt$eta = ar(tt$Q)$resid;

#psi represents unexpected direction of trade
tt$psi = tt$D - (1 - 2 * pnorm(- (tt$Q - tt$eta) / sd(tt$eta, na.rm=T)));


sadkaA = lm(dP ~ psi + eta + dD + dQ, data = tt)
summary(sadkaA)


# do the same model using returns instead of price change
tt$R = c(NA, diff(log(tt$Price)))

sadkaB = lm(R ~ psi + eta + dD + dQ, data = tt)
summary(sadkaB)

