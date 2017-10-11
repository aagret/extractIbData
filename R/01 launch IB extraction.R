
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
#clientNav[, aNav:= calcAdjNav(NAV)]

db <- calcAdjNav(NAV)
clientNav <- db[clientNav]
clientNav[, aNav:=aNav + cFee]

# group by quarter
clientNav[, Q:= quarter(TradeDate)]

# calc High Water Mark
clientNav[, Hwm:=  shift(aNav- cFee), by= c("ClientId")]
clientNav[is.na(Hwm), Hwm:= aNav]
clientNav[, Hwm:= cummax(Hwm), by= c("Q", "ClientId")]

# calc Performance fee (estimated and Real end Quarter)
clientNav[aNav >  Hwm, pFee:= (aNav - Hwm) * perfFee / 100, 
          by= c("ClientId", "Q")]

clientNav[aNav <= Hwm, pFee:=0, ]                         # 0 if lower than Hwm

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
clientNav[, Gross:= aNav + cumsum(Fee), by= ClientId]

# calc Net of fee Perormance (artha fee, all cost)
clientNav[, fNet:= Gross - pFee - aFee - Vat - TF, by= ClientId]
clientNav[, cNet:= Gross - pFee - aFee - Vat - TF, by= ClientId]


####
############ END OF SCRIPT
################################################################################
########################





########################
################################################################################
############ TEST AND DEBUG ZONE
####


# group by quarter
clientNav[, Q:= quarter(TradeDate)]

clientNav[TradeDate > as.Date("2016-12-31") & 
              TradeDate <= as.Date("2017-09-29"), 
          .((aNav[.N] - Hwm[1]) * perfFee / 100,
            (aNav[1] + aNav[.N]) /2 * advFee * 3 / 1200), 
          by= c("ClientId", "Q")]



cli <- "U2202020" # U1427234"
plot(clientNav[ClientId== cli, 
               .(TradeDate, exp(cumsum(c(0, ROC(Gross)[-1]))))], type="l", col="red")

lines(clientNav[ClientId== cli, 
                .(TradeDate, exp(cumsum(c(0, ROC(cNet)[-1]))))], col="blue")




clientNav[, .(TradeDate, NAV, aNav, Gross, cNet,
              c(0, exp(cumsum(ROC(aNav)[-1]))),
              c(0, exp(cumsum(ROC(Gross)[-1]))),
              c(0, exp(cumsum(ROC(cNet)[-1])))),
          by= ClientId][ , .SD[.N,], by= ClientId]


clientNav[, .SD[.N], by= ClientId,
          .SDcols= c("pFee", "aFee", "Vat", "TF")]

clientNav[TradeDate %in% as.Date(c("2016-12-30", "2017-03-31", "2017-06-30", "2017-09-29")), .SD, by= ClientId]


####
############ END OF TEST AND DEBUG ZONE
################################################################################
########################


