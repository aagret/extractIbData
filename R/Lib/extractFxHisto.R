

# Function to retrieve Fx history from IB Trades
extractFxHisto <- function(db = ibdata) {
    
    # extract Fx vs CHF history
    fxHisto <- db[AssetClass =="CHF",]
    
    # format data and remove useless columns
    fxHisto[, Symbol:= as.numeric(Symbol)][ ,c(1,2,4)]
    
    # rename columns
    colnames(fxHisto)[c(1,3)] <- c("TradeDate", "Fx")
    
    # format datas
    fxHisto[, TradeDate:= as.Date(TradeDate, format= "%Y%m%d")]
    
    setDT(fxHisto, key= c("TradeDate", "CurrencyPrimary"))
    
    return(fxHisto)
    
}
