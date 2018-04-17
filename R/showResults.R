


########################
################################################################################
############ TEST AND DEBUG ZONE
####


# group by quarter
ibClientNav[, Q:= quarter(TradeDate)]

ibClientNav[TradeDate > as.Date("2017-12-31") &
              TradeDate <= as.Date("2018-03-31"),
          .((adjNav[.N] - Hwm[1]) * perfFee / 100,
            (adjNav[1] + adjNav[.N]) /2 * advFee * 3 / 1200),
          by= c("ClientId", "Q")]


library(quantmod)

getSymbols("^GSPC", src="yahoo")
SPX <- as.data.table(GSPC["2017-12-31/2018-03-31", 4])

cli <- "U1427234" #"U2202020" #

lines(ibClientNav[ClientId== cli,
                 .(TradeDate, exp(cumsum(c(0, ROC(Gross)[-1]))))], type="l", col="red")

lines(ibClientNav[ClientId== cli,
                  .(TradeDate, exp(cumsum(c(0, ROC(fNet)[-1]))))], col="blue")


lines(SPX[, .(index, exp(cumsum(c(0, ROC(GSPC.Close)[-1]))))])


library(PerformanceAnalytics)
table.Stats(ibClientNav$Gross)
    
xt <- xts(x = ibClientNav[ClientId == cli, Gross], 
          order.by = ibClientNav[ClientId == cli, TradeDate])
  
SharpeRatio(ROC(xt)[-1], Rf= 0.00966, FUN= "StdDev")
table.AnnualizedReturns(ROC(xt)[-1], Rf= 0.000035)    
table.Distributions(ROC(xt)[-1])



ibClientNav[, .(TradeDate, ibNav, adjNav, Gross, cNet,
              c(0, exp(cumsum(ROC(adjNav)[-1]))),
              c(0, exp(cumsum(ROC(Gross)[-1]))),
              c(0, exp(cumsum(ROC(cNet)[-1])))),
          by= ClientId][ , .SD[.N,], by= ClientId]


ibClientNav[, .SD[.N], by= ClientId,
          .SDcols= c("perfFee", "advFee", "Vat", "timbre")]

ibClientNav[TradeDate %in% as.Date(c("2016-12-30", "2017-03-31", "2017-06-30", "2017-09-29")), .SD, by= ClientId]

ibClientNav[,last(.SD,1), 
            by=c("Q", "ClientId")]

ibClientNav[,last(.SD,1), 
            by=c("Q", "ClientId")][, sum(perfFee+advFee+Vat+timbre), by=c("Q", "ClientId")]


####
############ END OF TEST AND DEBUG ZONE
################################################################################
########################
#
