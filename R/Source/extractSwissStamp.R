
# Function to extract Swiss Stamp Data from IB trade list
extractSwissStamp <- function(trade= ytdTrades) {

    # get today's and current quarter Dates
    #qDts <- quarterDates(Sys.Date())
    qDts <- as.Date("2016-12-31")
    
    # extract Timbre datas
    db <- trade[TradeDate >= qDts[1] &  AssetClass =="STK",
                .(OrderTime,
                  Description, Symbol, ISIN,
                  TradeDate, Currency,  
                  Buy.Sell, Quantity, TradePrice, 
                  Proceeds, ClientId, Fx)]
    
    #tickers <- fread ("/home/artha/Alexandre/Tfc/ticker4.csv")
    tickers <- fread ("U:/Tfc/ticker4.csv")
    tickers <- tickers[,c(3,2)]
    colnames(tickers) <- c("Symbol", "ISIN")
   
    db2 <- db[ISIN == "",][, Symbol:= paste0(Symbol, " US Equity")]
    
    db1 <- db[ISIN != "",]
    
    setkey(tickers, Symbol)
    setkey(db2, Symbol)
    
    db2  <- tickers[db2][,-5, with=FALSE]
    
    db <- rbind(db1, db2)
    
    setkey(db, OrderTime, Description, ClientId)
    
    # calc stamp amount
    db[Currency ==  "CHF", ":=" (SoumisSuisse= round(Proceeds * Fx, 0),
                                 SoumisEtr= 0,
                                 NonSoumis= 0)]
    
    db[!Currency == "CHF", ":=" (SoumisSuisse= 0,
                                 SoumisEtr= round(Proceeds * Fx, 0),
                                 NonSoumis= 0)]
    
    
    db[, Quantity:= abs(Quantity)]
    db[, TradePrice:= round(TradePrice, 3)]
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
    
    setkey(ibTrades, OrderTime, Description)
    
    ibTimbre <- ibTimbre[ibTrades]
    
    ibTimbre[, ":=" (ClientId="Interactive Brokers",
                     NonSoumis= SoumisSuisse + SoumisEtr,
                     SoumisSuisse= 0,
                     SoumisEtr= 0)]
    
    ibTimbre[, Buy.Sell:= ifelse(Buy.Sell=="ACH", "VTE", "ACH")]
    
    db <- rbind(db, ibTimbre)
    
    # sort timbre
    setorder(db, OrderTime, Description, -ClientId)
    
    
    db[, cliNbr:= .N, by= c("OrderTime","Description")]

    
    
    # order columns
    setcolorder(db, c(1, 16, 3, 4, 5, 7, 
                      8, 2, 9, 6, 12,
                      11, 10, 13, 14, 15))
    
    setkey(db, ClientId, TradeDate)
    

    return(db)
    
}
