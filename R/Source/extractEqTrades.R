
# function to extract Equity and Futures trades from IB trades ! usefull ?
extractEqTrades <- function(db= ibData) {
    
    # extract equities datas ! if DT DF n ot needed !
    db <- ibData[ibData$AssetClass == "STK", ]
    
    #format datas
    db[, ":=" (TradeDate=  as.Date(TradeDate, format= "%Y%m%d"),
               Quantity=   as.numeric(Quantity),
               TradePrice= as.numeric(TradePrice),
               Proceeds=   abs(as.numeric(Proceeds)))] 
    
    setkey(db, TradeDate, CurrencyPrimary)
    
    return(db)
    
}
