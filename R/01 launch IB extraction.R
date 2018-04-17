
########################
################################################################################
############ USER INPUT AREA 
####


########################
### Define Variables ###
########################

# IB_Artha communication chanel ibToken# (to updated periodically ie yearly)

#####       ##########################
ibToken   <-  "280265450571377242472700"
#####       ##########################   

# ibpaidFeess metrics (in %)

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
ibData <- getYtdData(ibToken)

# download YTD Artha ibpaidFeess from  IB
ibpaidFees <- getIbFees(ibToken)

# download withdrawal/depositis from Ib account
ibInOut <- getInOut(ibToken)

# download clients ibNav history
ibClientNav <- getClientNav(ibToken)


################################
### Extract and format Datas ###
################################

# extract Fx history from ibData
ibFxHisto <- extractFxHisto(ibData)

# extract Equity&Future trades
ibTrades <- ibData[AssetClass == "STK", ]


###################
### Merge Datas ###
###################

# merge Trades & Fx datas
setkey(ibFxHisto, TradeDate, Currency)
setkey(ibTrades,  TradeDate, Currency)

ibTrades <- ibFxHisto[ibTrades]                     # merge
ibTrades[Currency == "CHF", Fx:= 1L]                # set CHF Fx to 1

# merge IbNav, paidFees and ibInOut datas
setkey(ibInOut,     ClientId, TradeDate)
setkey(ibpaidFees,  ClientId, TradeDate)
setkey(ibClientNav, ClientId, TradeDate)

ibClientNav <-  ibInOut[ibpaidFees[ibClientNav[ibNav != 0, ]]]       # merge


###################################
### Format and Calculate Values ###
###################################

# format resulting data table
ibClientNav[is.na(InOut), InOut:= 0]
ibClientNav[is.na(paidFee),   paidFee:=   0]

# adjust Nav by accounting ibpaidFeess on correct dates
ibClientNav <- calcAdjNav(ibClientNav)[ibClientNav]
ibClientNav[, adjNav:= adjNav + dueFee]

# group by quarter
ibClientNav[, Q:= quarter(TradeDate)]

# calc High Water Mark
ibClientNav[, Hwm:=  shift(adjNav- dueFee), by= c("ClientId")]
ibClientNav[is.na(Hwm), Hwm:= adjNav]
ibClientNav[, Hwm:= cummax(Hwm), by= c("Q", "ClientId")]

# calc Performance ibpaidFees (estimated and Real end Quarter)
ibClientNav[adjNav >  Hwm, perfFee:= (adjNav - Hwm) * perfFee / 100, 
            by= c("ClientId", "Q")]

ibClientNav[adjNav <= Hwm, perfFee:=0, ]                        # 0 if lower than Hwm

ibClientNav[, perfFee:= cumsum(perfFee),
            by= ClientId]

# calc advisory paidFee
ibClientNav[, advFee:= cumsum(c(0, diff(TradeDate)) * adjNav * advFee / 36500),
            by= ClientId]

# calc Vat on paidFees (for CH clients only)
ibClientNav[, Vat:= (perfFee + advFee) * 8 / 100]




#######################
### Swiss Stamp Tax ###
#######################

# extract Swiss Stamp from trade list
ibTimbre <- extractSwissStamp(ibTrades)
fwrite(ibTimbre, "ibTimbre.csv")

# calc total stamp per client and merge with ibClientNav
ibClientNav <- ibTimbre[, calcStamp(ibTimbre)][ibClientNav]

# format Stamp Datas
ibClientNav[is.na(timbre), timbre:= 0]
ibClientNav[, timbre:= cumsum(timbre), by= ClientId]

setcolorder(ibClientNav, c(colnames(ibClientNav)[-3], colnames(ibClientNav)[3]))

# calc Gross NAv ex paidFees, VAt and Stramp Tax
ibClientNav[, Gross:= adjNav + cumsum(paidFee), by= ClientId]

# calc Net of ibpaidFees Perormance (artha ibpaidFees, all cost)
ibClientNav[, fNet:= Gross - perfFee - advFee - Vat - timbre, by= ClientId]
ibClientNav[, cNet:= Gross - perfFee - advFee - Vat - timbre, by= ClientId]


####
############ END OF SCRIPT
################################################################################
########################



