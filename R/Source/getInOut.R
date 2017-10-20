
# function to extract deposit/withdrawals from UB accounts
getInOut <- function(tok= ibToken) {
    
    # exctract in/out
    reportId <- "265392"
    
    db    <- getIbReport(reportId, tok)
    
    #colnames
    colnames(db)[c(1, 3, 8)] <- c("ClientId", "TradeDate", "InOut")
    
    # format data and remove useless columns
    db <- db[ActivityCode == "DEP", .(ClientId, TradeDate, InOut)]
    
    
    db[, ':=' (TradeDate= as.Date(TradeDate, format= "%Y%m%d"), 
               InOut = as.numeric(InOut))]
    
    
    
}
