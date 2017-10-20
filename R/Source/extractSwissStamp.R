
# Function to extract Swiss Stamp Data from IB trade list
extractSwissStamp <- function(trade= ibTrades) {
    
    # get today's and current quarter Dates
    #quarterDates <- quarterDates(Sys.Date())
    quarterDates <- as.Date("2016-12-31")
    
    # extract Timbre datas
    db <- trade[TradeDate >= quarterDates[1] &  AssetClass =="STK",
                .(OrderTime, AssetClass,
                  Description, Symbol, Isin,
                  TradeDate, Currency,  
                  Buy.Sell, Quantity, TradePrice, 
                  Proceeds, ClientId, Fx)]
    
    #tickers <- fread ("/home/artha/Alexandre/Tfc/ticker4.csv")
    tickers <- fread ("U:/Tfc/ticker4.csv")[, c(3, 2)]
    
    colnames(tickers) <- c("Symbol", "Isin")
    
    db2 <- db[Isin == "",][, Symbol:= paste0(Symbol, " US Equity")]
    
    db1 <- db[Isin != "",]
    
    setkey(tickers, Symbol)
    setkey(db2, Symbol)
    
    db2  <- tickers[db2][,-6, with=FALSE]
    
    db <- rbind(db1, db2)[, -4]
    
    setkey(db, OrderTime, Description, ClientId)
    
    # calc stamp amount
    db[Currency ==  "CHF", ":=" (SoumisSuisse= Proceeds * Fx,
                                 SoumisEtr= 0,
                                 NonSoumis= 0)]
    
    db[!Currency == "CHF", ":=" (SoumisSuisse= 0,
                                 SoumisEtr= Proceeds * Fx,
                                 NonSoumis= 0)]
    
    
    db[, Quantity:= abs(Quantity)]
#    db[, TradePrice:= round(TradePrice, 3)]
    db[, Buy.Sell:= ifelse(Buy.Sell=="BUY", "ACH", "VTE")]
    
    # generate IB contrepartie datas
    ibTimbre <- db[, lapply(.SD, sum, na.rm=TRUE), 
                   by= c("OrderTime", "Description"),
                   .SDcols= c("Quantity", "Proceeds",
                              "SoumisSuisse", "SoumisEtr",
                              "NonSoumis")]
    
    ibTrades <- db[, head(.SD,1), 
                   by= c("OrderTime", "Description"), 
                   .SDcols= -c("Quantity", "Proceeds",
                               "SoumisSuisse", "SoumisEtr",
                               "NonSoumis")]
    
    # setkey(ibTrades, OrderTime, Description) useless ?
    
    ibTimbre <- ibTimbre[ibTrades]
    
    ibTimbre[, ":=" (ClientId="Interactive Brokers",
                     NonSoumis= SoumisSuisse + SoumisEtr,
                     SoumisSuisse= 0,
                     SoumisEtr= 0)]
    
    ibTimbre[, Buy.Sell:= ifelse(Buy.Sell=="ACH", "VTE", "ACH")]
    
    db <- rbind(db, ibTimbre)
    
    # sort ibTimbre
    setorder(db, OrderTime, Description, -ClientId)
    
    db[, cliNbr:= .N, by= c("OrderTime","Description")]
    
    setkey(db, ClientId, TradeDate)
    
}
