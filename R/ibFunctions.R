
#############################
########  Functions  ########
#############################


############################
#### Download Functions ####
############################

# download reports Function from IB
source("R/Source/getIbReport.R")

# download YtdDatas
source("R/Source/getYtdData.R")

# download client's IbNav datas
source("R/Source/getClientNav.R")

# download inflow/outflows datas
source("R/Source/getInOut.R")

# download accounted paidFees datas
source("R/Source/getIbFees.R")


##############################################
### functions to extract and format datas ####
##############################################

# extract Equity Trades
source("R/Source/extractEqTrades.R")

# extract historical Fx rates
source("R/Source/extractFxHisto.R")

# extract Swiss Stamp 
source("R/Source/extractSwissStamp.R")

# find end and start dates of current Quarter
source("R/Source/quarterDates.R")


#############################
### Calculation Functions ###
#############################

# calc adjusted Nav (book fees on proper dates)
source("R/Source/calcAdjNav.R")

# calc High Water Mark
source("R/Source/calcHwm.R")

# calc Swiss Stamp
source("R/Source/calcStamp.R")

