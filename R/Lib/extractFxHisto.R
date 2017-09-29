

# Function to retrieve Fx history from IB Trades
extractFxHisto <- function(db = ibData) {
    
    # extract Fx vs CHF history
    db <- db[AssetClass =="CHF", .(ClientId, Currency, Symbol)]

    # rename and format data 
    colnames(db)[c(1, 3)] <- c("TradeDate", "Fx")
    
    db[, ':=' (Fx= as.numeric(Fx),
               TradeDate= as.Date(TradeDate, format= "%Y%m%d"))]
    
    # cast in worksheet form
    #db <- dcast(db, TradeDate + Fx ~ Currency, 
    #                 value.var="Symbol")

    return(db)
    
}
