"
 na.locf Last Observation Carried Forward
 Generic function for replacing each NA
 with the most recent non-NA prior to it.
 from package zoo
 
 ******* 
  need to be connected to internet
  in R studio right bottom area, commonly know as  4 - Plots and files
  click on Packages, Install
  Install from: Repository (CRAN)
  Packages (separet multiple with space or comma): zoo
  and make sure you select the package
"

tq$Ask.Price = na.locf(tq$Ask.Price)
tq$Bid.Price = na.locf(tq$Bid.Price)

mean(tq$Ask.Price) # 127.4185
mean(tq$Ask.Price , na.rm = 'TRUE') # 127.4185
