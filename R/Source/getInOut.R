
# function to extract deposit/withdrawals from UB accounts
getInOut <- function(tok= token) {
    
    # exctract in/out
    reportId <- "265392"
    db    <- getIbReport(reportId, tok)
    
    # format data and remove useless columns
    db <- db[Activity.Code == "DEP", .(ClientAccountID, Report.Date, Amount)]
    
    db[, ':=' (Report.Date= as.Date(Report.Date, format= "%Y%m%d"), 
               Amount= as.numeric(Amount))]
    
    
    colnames(db) <- c("ClientId", "TradeDate", "InOut")
    
    return(db)
    
}
