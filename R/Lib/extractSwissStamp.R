
# Function to extract Swiss Stamp Data from IB trade list
extractTF <- function(trade= yearlyTrades) {
    
    # get today's and current quarter Dates
    today   <- Sys.Date()
    startDt <- as.Date(cut(as.Date(cut(today, "quarter")), "quarter"))
    endDt   <- as.Date(cut(as.Date(cut(today, "quarter")) + 92, "quarter")) - 1
    
    # extract Timbre datas
    timbre <- yearlyTrades[TradeDate >= startDt &  AssetClass =="STK",
                           .(OrderTime,
                             Description, Conid,
                             TradeDate, CurrencyPrimary,  
                             Buy.Sell, Quantity, TradePrice, 
                             Proceeds, ClientAccountID, Fx)]
    
    timbre[CurrencyPrimary ==  "CHF", ":=" (SoumisSuisse= round(Proceeds * Fx, 0),
                                            SoumisEtr= 0,
                                            NonSoumis= 0)]
    
    timbre[!CurrencyPrimary == "CHF", ":=" (SoumisSuisse= 0,
                                            SoumisEtr= round(Proceeds * Fx, 0),
                                            NonSoumis= 0)]
    
    timbre[, TradePrice:= round(TradePrice, 3)]
    
    timbre[, Buy.Sell:= ifelse(Buy.Sell=="BUY", "ACH", "VTE")]
    
    # generate IB datas
    ibTimbre <- timbre[, lapply(.SD, sum, na.rm=TRUE), 
                       by= c("OrderTime", "Description"),
                       .SDcols= c(7, 9, 12, 13, 14)]
    
    ibTrades <- timbre[, head(.SD,1), 
                       by= c("OrderTime", "Description"), 
                       .SDcols= -c(7, 9, 12, 13, 14)]
    
    setkey(ibTrades, OrderTime, Description)
    setkey(ibTimbre, OrderTime, Description)
    
    ibTimbre <- ibTimbre[ibTrades]
    
    ibTimbre[, ":=" (ClientAccountID="Interactive Brokers",
                     NonSoumis= SoumisSuisse + SoumisEtr,
                     SoumisSuisse= 0,
                     SoumisEtr= 0)]
    
    ibTimbre[, Buy.Sell:= ifelse(Buy.Sell=="ACH", "VTE", "ACH")]
    
    timbre <- rbind(timbre, ibTimbre)
    
    setkey(timbre, TradeDate, CurrencyPrimary)
    
    # sort timbre
    setorder(timbre,OrderTime, Description, -ClientAccountID)
    
    return(timbre)
    
}