## calc perf.fee

cli <- "U1427234"
cliData <- clientNav[ClientId == cli, .(ClientId, Date,
                                        NAV, Gross, Net)] 


fee <- 0.5
perfFee <- 20

# get today's and current quarter Dates
today   <- Sys.Date()
endDt   <- as.Date(cut(as.Date(cut(today, "quarter")), "quarter")) -1
startDt <- as.Date(cut(as.Date(cut(today, "quarter")) - 90, "quarter")) - 1



#cliData[, Net]

cliData[, Hwm:= shift(cummax(Net), fill = 0)]
cliData$Hwm[1] <- cliData$Net[1]

cliData[, PnL:= cumsum(c(0, diff(Net)))]

cliData[Net >  Hwm, pFee:= (Net - Hwm) * 0.20]
cliData[Net <= Hwm, pFee:=0]
cliData[, pFee:= cumsum(pFee)]

cliData[, aFee:= cumsum(c(0, diff(Date)) * Net * 0.5 / 36500)]

cliData[, Vat:= (pFee + aFee) * 8 / 100]

cliData[, newG:= Net + pFee + aFee + Vat]

plot (exp(cumsum(ROC(cliData[, newG])[-1])), type="l")
lines(exp(cumsum(ROC(cliData[, Net ])[-1])), col="red")

last(exp(cumsum(ROC(cliData[Date >= "2016-12-30", .(Date, NAV, Net, newG)])[-1])), 1)
last(exp(cumsum(ROC(cliData[Date >= "2017-06-30", .(Date, NAV, Net, newG)])[-1])), 1)
last(exp(cumsum(ROC(cliData[Date >= "2017-08-31", .(Date, NAV, Net, newG)])[-1])), 1)

last(exp(cumsum(ROC(cliData[Date >= "2016-12-30" & Date <= "2017-06-30",
                                    .(Date, NAV, Net, newG)])[-1])), 1)

plot(cliData$Gross, type="l")
lines(cliData$newG, col="red")



