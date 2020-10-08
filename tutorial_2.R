# set working directory
setwd("C:/Users/MarioLoeraLozano/Dropbox/R/")

# clean workspace and variables
rm(list = setdiff(ls(), lsf.str()))

# 1 ---------------------------------
# https://youtu.be/KI0KBZcty18
# load data saved in tutorial 1
load("Data/tqData")
# tq: 200728 obs. of 15 variables

"
  make a supset using only Trades obsrvations
  and where the dirrecion of the trade different than 0
"

tt = tq[tq$Type == "Trade" & tq$D != 0, 
        c("Date", "Seconds", "Price", "Volume", "D", "Q")];

# tt: 19545 obs. of 6 variables
head(tt)
# 2 ---------------------------------
# Glosten and Harris
# https://youtu.be/NuQvzmzSwuI
# calculate the difference, include NA for the first rows

tt$dP = c(NA, diff(tt$Price));
tt$dD = c(NA, diff(tt$D));
tt$dQ = c(NA, diff(tt$Q));
head(tt)
# tt: 19545 obs. of 9 variables

# 2.b Glosten and Harris restricted model
# delta_price_t = lamda_1 * Q_t + gama_0 * delta_D_t + epsilon_t
# delta_pt = l1*Qt + g0*delta_Dt + et
# they exclude D and diff (q)

GH.restricted = lm(dP ~ Q + dD, data = tt)
summary(GH.restricted)
"
Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 2.173e-04  3.934e-04   0.552    0.581    
Q           4.768e-06  3.711e-07  12.847   <2e-16 ***
dD          3.073e-02  4.669e-04  65.809   <2e-16 ***

 *notes: 
 lamda_1 = 4.768e-06 ~ 4.8 points per million
 gama_0 = 3.073e-02
"

#2.c  Glosten and Harris  Unrestricted model
# delta_pt = l0*Dt + l1*Qt + g0*delta_Dt + g1*delta_Q  + et

GH.unrestricted = lm(dP ~ D + Q + dD + dQ, data = tt)
summary(GH.unrestricted)

"
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1.200e-03  3.935e-04   3.048   0.0023 ** 
D           1.001e-02  5.771e-04  17.347   <2e-16 ***
Q           1.054e-07  5.826e-07   0.181   0.8565    
dD          2.698e-02  5.621e-04  48.005   <2e-16 ***
dQ          1.096e-07  4.428e-07   0.248   0.8045    

p_value for Q coefficient  0.8565
p_value for dQ coefficient 0.8045

most impact
D  1.001e-02
"

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
head(tt, 20)

tt[8:12,  c("eta", "psi")]
"
             eta       psi
742962        NA        NA
742998        NA        NA
743002  582.9481 1.0776045
743012  105.8620 0.9011682
743014 -134.0974 0.8871849
"

sadkaA = lm(dP ~ psi + eta + dD + dQ, data = tt)
summary(sadkaA)

"
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.087e-03  3.922e-04   2.772  0.00558 ** 
psi          1.101e-02  6.066e-04  18.145  < 2e-16 ***
eta          1.800e-06  7.546e-07   2.385  0.01708 *  
dD           2.590e-02  5.781e-04  44.797  < 2e-16 ***
dQ          -1.725e-06  5.748e-07  -3.001  0.00269 ** 

*notes

all four variables are highly statisticall significant
psi:  1.1 % vs 2% in zadka's table
eta: 1.8 point per million  vs 7.9 in zadka's table
dD: 2.6 % vs 5%
dQ: -1.7 point per million vs -3.1 point per million

this sample is more liquid than zadka
"

# do the same model using returns instead of price change
tt$R = c(NA, diff(log(tt$Price)))

sadkaB = lm(R ~ psi + eta + dD + dQ, data = tt)
summary(sadkaB)

"
all four variables are highly statisticall significant

              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.551e-06  3.084e-06   2.773  0.00556 ** 
psi          8.645e-05  4.770e-06  18.124  < 2e-16 ***
eta          1.424e-08  5.934e-09   2.400  0.01641 *  
dD           2.030e-04  4.545e-06  44.658  < 2e-16 ***
dQ          -1.363e-08  4.520e-09  -3.016  0.00256 **

this sample is more liquid than zadka on average
"
