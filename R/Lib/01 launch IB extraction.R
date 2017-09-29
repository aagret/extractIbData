
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
ibData <- getYtdData(token)

# extract Fx history from ibData
fxHisto <- extractFxHisto(ibData)

# extract Equity&Future trades
ytdTrades <- ibData[ibData$AssetClass == "STK", ]

# merge datas ###
# attribute correct Fx to each trade
setkey(fxHisto,   TradeDate, Currency)
setkey(ytdTrades, TradeDate, Currency)

ytdTrades <- fxHisto[ytdTrades]

# set CHF trade to 1
ytdTrades[Currency == "CHF", Fx:= 1L]

# download YTD Artha fees from  IB
fee <- getAdvFee(token)

# download withdrawal/depositis from Ib account
inOut <- getInOut(token)

# download clients NAV history
clientNav <- getClientNav(token)

### ###
# merge NAV, Fees and inOut datas
setkey(inOut,     ClientId, TradeDate)
setkey(fee,       ClientId, TradeDate)
setkey(clientNav, ClientId, TradeDate)

# merge it
clientNav <-  inOut[fee[clientNav[NAV !=0,]]]

# format datas
clientNav[is.na(InOut),InOut:= 0]
clientNav[is.na(Fee),  Fee:=   0]

clientNav[, aNav:= adjNav(NAV)]
clientNav[, Hwm:=  calcHwm(aNav), by= ClientId]


# calc perf Fee
clientNav[aNav >  Hwm, pFee:= (aNav - Hwm) * perfFee / 100, 
          by= ClientId]
clientNav[aNav <= Hwm, pFee:=0, 
          by= ClientId]
clientNav[, pFee:= cumsum(pFee), 
          by= ClientId]

# calc advisory Fee
clientNav[, aFee:= cumsum(c(0, diff(TradeDate)) * aNav * advFee / 36500),
          by= ClientId]

# calc Vat on Fees (for CH clients only)
clientNav[, Vat:= (pFee + aFee) * 8 / 100]

# extract Swiss Stamp from trade list
timbre <- extractSwissStamp(ytdTrades)

# calc total stamp per client and merge with clientNav
clientNav <- timbre[, calcStamp(timbre)][clientNav]
clientNav[is.na(TF), TF:=0]
clientNav[, TF:= cumsum(TF), by= ClientId]
setcolorder(clientNav, c(colnames(clientNav)[-3], colnames(clientNav)[3]))

# calc Gross ex all Fees NAV (should also get VAT back later...) TO DO!
clientNav[, Gross:= aNav + pFee + aFee + Vat + TF]


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

