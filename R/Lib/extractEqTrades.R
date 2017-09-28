
# function to extract Equjity and Futures trades from IB trades

extractEqTrades <- function(db= ibData) {
    
    # extract equities datas
    ytdTrades <- ibData[ibData$AssetClass == "STK", ]
    
    #format datas
    ytdTrades[, ":=" (TradeDate=  as.Date(TradeDate, format= "%Y%m%d"),
                      Quantity=   as.numeric(Quantity),
                      TradePrice= as.numeric(TradePrice),
                      Proceeds=   abs(as.numeric(Proceeds)))] 
    
    return(ytdTrades)
    
}
    