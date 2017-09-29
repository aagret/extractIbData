
########################
################################################################################
############ USER INPUT AREA 
####

########################
### Define Variables ###
########################


# IB_Artha communication chanel token# (to updated periodically ie yearly)

#####       ##########################
token   <-  "409423572751697446678243"
#####       ##########################   
    
# fees metrics (in %)

#####       ####
advFee  <-  0.50
#####       ####

#####       ####
perfFee <-  20
#####       ####

####
############ END OF USER INPUT AREA
################################################################################
########################


########################
################################################################################
############ SCRIPT
####

######################
### Load Libraries ###
######################

library(XML)
library(methods)
library(httr)
library(data.table)
library(TTR)
library(ggplot2)


#######################
### Load Functions ###
#######################

source("R/ibFunctions.R")


######################
### Download Datas ###
######################

# download YTD IB datas
ibData <- getYtdData(token)

# download YTD Artha fees from  IB
fee <- getAdvFee(token)

# download withdrawal/depositis from Ib account
inOut <- getInOut(token)

# download clients NAV history
clientNav <- getClientNav(token)


################################
### Extract and format Datas ###
################################

# extract Fx history from ibData
fxHisto <- extractFxHisto(ibData)

# extract Equity&Future trades
ytdTrades <- ibData[ibData$AssetClass == "STK", ]


###################
### Merge Datas ###
###################

# merge Trades & Fx datas
setkey(fxHisto,   TradeDate, Currency)
setkey(ytdTrades, TradeDate, Currency)

ytdTrades <- fxHisto[ytdTrades]                     # merge
ytdTrades[Currency == "CHF", Fx:= 1L]               # set CHF Fx to 1

# merge NAV, Fees and inOut datas
setkey(inOut,     ClientId, TradeDate)
setkey(fee,       ClientId, TradeDate)
setkey(clientNav, ClientId, TradeDate)

clientNav <-  inOut[fee[clientNav[NAV != 0,]]]       # merge


###################################
### Format and Calculate Values ###
###################################

# format resulting data table
clientNav[is.na(InOut), InOut:= 0]
clientNav[is.na(Fee),   Fee:=   0]

# adjust Nav by accounting fees on correct dates
clientNav[, aNav:= adjNav(NAV), 
          by= ClientId]

# calc HighWaterMark
clientNav[, Hwm:=  calcHwm(aNav), 
          by= ClientId]

# calc Performance fee
clientNav[aNav >  Hwm, pFee:= (aNav - Hwm) * perfFee / 100, 
          by= ClientId]

clientNav[aNav <= Hwm, pFee:=0, 
          by= ClientId]                         # 0 if lower than Hwm

clientNav[, pFee:= cumsum(pFee),
          by= ClientId]

# calc advisory Fee
clientNav[, aFee:= cumsum(c(0, diff(TradeDate)) * aNav * advFee / 36500),
          by= ClientId]

# calc Vat on Fees (for CH clients only)
clientNav[, Vat:= (pFee + aFee) * 8 / 100]


#######################
### Swiss Stamp Tax ###
#######################

# extract Swiss Stamp from trade list
timbre <- extractSwissStamp(ytdTrades)

# calc total stamp per client and merge with clientNav
clientNav <- timbre[, calcStamp(timbre)][clientNav]

# format Stamp Datas
clientNav[is.na(TF), TF:=0]
clientNav[, TF:= cumsum(TF), by= ClientId]

setcolorder(clientNav, c(colnames(clientNav)[-3], colnames(clientNav)[3]))

# calc Gross NAv ex Fees, VAt and Stramp Tax
clientNav[, Gross:= aNav + pFee + aFee + Vat + TF]

####
############ END OF USER INPUT AREA
################################################################################
########################





########################
################################################################################
############ TEST AND DEBUG ZONE
####


cli <- "U1427234"
plot(clientNav[ClientId== cli, 
               .(TradeDate, exp(cumsum(c(0, ROC(Gross)[-1]))))], type="l", col="red")

lines(clientNav[ClientId== cli, 
                .(TradeDate, exp(cumsum(c(0, ROC(aNav)[-1]))))], col="blue")
      

clientNav[, 
          .(TradeDate, aNav, Gross, 
            c(0, exp(cumsum(ROC(aNav)[-1]))),
            c(0, exp(cumsum(ROC(Gross)[-1])))),
          by= ClientId][ , .SD[.N,], by= ClientId]


####
############ END OF TEST AND DEBUG ZONE
################################################################################
########################


