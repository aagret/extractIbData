
# Function to extract Swiss Stamp Data from IB trade list
extractSwissStamp <- function(trade= ytdTrades) {

    # get today's and current quarter Dates
    #qDts <- quarterDates(Sys.Date())
    qDts <- as.Date("2016-12-31")
    
    # extract Timbre datas
    db <- trade[TradeDate >= qDts[1] &  AssetClass =="STK",
                .(OrderTime,
                  Description, Conid,
                  TradeDate, Currency,  
                  Buy.Sell, Quantity, TradePrice, 
                  Proceeds, ClientId, Fx)]
    
    db[Currency ==  "CHF", ":=" (SoumisSuisse= round(Proceeds * Fx, 0),
                                 SoumisEtr= 0,
                                 NonSoumis= 0)]
    
    db[!Currency == "CHF", ":=" (SoumisSuisse= 0,
                                 SoumisEtr= round(Proceeds * Fx, 0),
                                 NonSoumis= 0)]
    
    db[, TradePrice:= round(TradePrice, 3)]
    
    db[, Buy.Sell:= ifelse(Buy.Sell=="BUY", "ACH", "VTE")]
    
    # generate IB datas
    ibTimbre <- db[, lapply(.SD, sum, na.rm=TRUE), 
                   by= c("OrderTime", "Description"),
                   .SDcols= c(7, 9, 12, 13, 14)]
    
    ibTrades <- db[, head(.SD,1), 
                   by= c("OrderTime", "Description"), 
                   .SDcols= -c(7, 9, 12, 13, 14)]
    
    setkey(ibTrades, OrderTime, Description)
    setkey(ibTimbre, OrderTime, Description)
    
    ibTimbre <- ibTimbre[ibTrades]
    
    ibTimbre[, ":=" (ClientId="Interactive Brokers",
                     NonSoumis= SoumisSuisse + SoumisEtr,
                     SoumisSuisse= 0,
                     SoumisEtr= 0)]
    
    ibTimbre[, Buy.Sell:= ifelse(Buy.Sell=="ACH", "VTE", "ACH")]
    
    db <- rbind(db, ibTimbre)
    
    setkey(db, ClientId, TradeDate)
    
    db[, cliNbr:= length(ClientId), by= c("OrderTime","Description")]
    
    # sort timbre
    setorder(db,OrderTime, Description, -ClientId)
    setkey(db, ClientId, TradeDate)
    
    return(db)
    
}
