
## impression timbre

setorder(timbre, OrderTime, Description, TradePrice, -ClientId)


TF <- timbre


# add opération Number
trades <-  unique(timbre[, .(OrderTime, Description)])
trades[, opNbr:= rownames(trades)]

setkey(TF, OrderTime, Description)
setkey(trades, OrderTime, Description)

TF <- TF[trades]
TF <- TF[, .(opNbr, TradeDate, Buy.Sell, Currency, abs(Quantity),
             Description, TradePrice, ClientId,
             SoumisSuisse, SoumisEtr, NonSoumis)]


save(TF, file="TF.RData")


