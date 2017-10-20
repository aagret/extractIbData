
## impression ibTimbre

setorder(ibTimbre, OrderTime, Description, TradePrice, -ClientId) # utile?


timbre <- ibTimbre


# add opération Number
trades <-  unique(ibTimbre[, .(OrderTime, Description)])
trades[, opNbr:= rownames(trades)]

setkey(timbre, OrderTime, Description)
setkey(trades, OrderTime, Description)

timbre <- timbre[trades]
timbre <- timbre[, .(opNbr, TradeDate, Buy.Sell, Currency, abs(Quantity),
                     Description, TradePrice, ClientId,
                     SoumisSuisse, SoumisEtr, NonSoumis)]


save(timbre, file="timbre.RData")




