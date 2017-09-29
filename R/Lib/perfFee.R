
#
# calc net p&L
cliData[, PnL:= cumsum(c(0, diff(Net)))]

# plot and display results



plot (exp(cumsum(ROC(clientNav[ClientId == "U1427234", Gross])[-1])), type="l")
lines(exp(cumsum(ROC(clientNav[ClientId == "U1427234", aNav ])[-1])), col="red")

last(exp(cumsum(ROC(cliData[Date >= "2016-12-30", .(Date, NAV, Net, newG)])[-1])), 1)


# graph result
g <- ggplot(clientNav) + 
    geom_line(aes(x=TradeDate, y= c(1, ret(aNav)), colour="aNav")) +
    geom_line(aes(x=TradeDate, y= c(1, ret(Gross)), colour="Gross")) +
    facet_grid(ClientId ~.)

ret <- function(nav = aNav) {
    ret <- exp(cumsum(ROC(nav)[-1]))
    return(ret)
}


plot(clientNav[ClientId=="U2202020", .(TradeDate, Gross)], type="l")

clientNav[, exp(cumsum(c(0, ROC(aNav)))), by=ClientId]



ClientNav[, ibPerf:= exp(cumsum(c(0, ROC(NAV, type= "continuous", na.pad=FALSE)))), 
          by= ClientId]

clientNav[, grossPerf:= log(Gross / shift(NAV)),
          by= ClientId]

clientNav[is.na(grossPerf), grossPerf:=0]

clientNav[, grossPerf:= exp(cumsum(grossPerf)), by= ClientId]




#PRINT
#############



header <- cat("ARTHA FINANCE SA", "\t", "\t", "REGISTRE DU TIMBRE FEDERAL DE NEGOCIATION,", "\n", 
              "Interactive Broker (U.K.) Ltd.", "\t", "BOURSE, EMISSIONS ET FONDS DE PLACEMENT DU", Sys.Date(), "\t", Sys.time())

timbre[, .I, by=c("OrderTime", "Description") ]


one <- timbre[OrderTime=="20170911;112915",]










