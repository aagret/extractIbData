
## query IB flex statement

library(XML)
library(methods)
library(httr)
library(data.table)
library(TTR)
library(ggplot2)

#source functions
source("R/ibFunctions")


# initiate (IB token to be updated manually periodically)
token <- "409423572751697446678243"

# fees metrics (in%)
advFee  <- 0.5
perfFee <- 20


# download YTD IB datas
ibData <- getYtdIBData(token)

# extract Fx history from ibData
fxHisto <- extractFxHisto(ibData)

# extract Equity&Future trades
ytdTrades <- extractEqTrades(ibData)

# attribute correct Fx to each trade
ytdTrades <- fxHisto[ytdTrades]

# set CHF trade to 1
ytdTrades[CurrencyPrimary == "CHF", Fx:= 1L]

# extract Swiss Stamp from trade list
timbre <- extractSwissStamp(ytdTrades)

# download YTD Artha fees from  IB
advFee <- getAdvFee(token)

# download withdrawal/depositis from Ib account
inOut <- getInOut(token)

# download clients NAV history
clientNav <- getClientNav(token)



# merge NAV, Fees and inOut datas
setkey(inOut,     ClientId, Date)
setkey(advFee,    ClientId, Date)
setkey(clientNav, ClientId, Date)

clientNav <-  inOut[advFee[clientNav[NAV !=0,]]]

# format datas
clientNav[is.na(InOut),   InOut:=0]
clientNav[is.na(advFee), advFee:=0]





# calc returns


# GROSS perf ???
clientNav[, Gross:= NAV + advFee - InOut]

clientNav[, ibPerf:= exp(cumsum(c(0, ROC(NAV, type= "continuous", na.pad=FALSE)))), 
          by= ClientId]

clientNav[, grossPerf:= log(Gross / shift(NAV)),
          by= ClientId]

clientNav[is.na(grossPerf), grossPerf:=0]

clientNav[, grossPerf:= exp(cumsum(grossPerf)), by= ClientId]




### HWM and attribute date of fee

dt <- clientNav[advFee != 0, .(ClientId, Date, advFee)]
dt[, Qq:=as.Date(cut(as.Date(cut(Date, "quarter")), "quarter")) - 1]
dt[, PayDate:= Date]
colnames(dt)[3] <- "QFee"
dt[strftime(Qq, "%u") == 6, Qq:= Qq - 1]
dt[strftime(Qq, "%u") == 7, Qq:= Qq - 2]

clientNav <- dt[clientNav, roll=-Inf]#[, .(ClientId, Date, advFee, Q)]

clientNav[, Net:= NAV ]
clientNav[Date < PayDate & Date >= Qq, Net:= Net - QFee, by=ClientId]





clientNav[, netPerf:= exp(cumsum(c(0, ROC(Net, type= "continuous", na.pad=FALSE)))),
by= ClientId]
clientNav[is.na(netPerf), netPerf:= 0]


# graph result
g <- ggplot(clientNav) + 
    geom_line(aes(x= Date, y= ibPerf, colour="ibPerf")) +
    geom_line(aes(x= Date, y= grossPerf, colour="grossPerf")) +
    geom_line(aes(x= Date, y= netPerf, colour="netPerf")) +
    facet_grid(ClientId ~.)












#PRINT
#############



header <- cat("ARTHA FINANCE SA", "\t", "\t", "REGISTRE DU TIMBRE FEDERAL DE NEGOCIATION,", "\n", 
    "Interactive Broker (U.K.) Ltd.", "\t", "BOURSE, EMISSIONS ET FONDS DE PLACEMENT DU", Sys.Date(), "\t", Sys.time())

timbre[, .I, by=c("OrderTime", "Description") ]
           
           
 one <- timbre[OrderTime=="20170911;112915",]








