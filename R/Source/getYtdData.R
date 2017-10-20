
getYtdData <- function(tok= ibToken) {
    
    # get yearly trades datas form IB web
    reportId    <- "139144" 
    
    db <- getIbReport(reportId, tok)
    
    # remove multi header error
    db <- db[!ClientAccountID == "ClientAccountID", ]
    
    colnames(db)[c(1,2,7)] <- c("ClientId", "Currency", "Isin")
    
    # format datas
    db[, ":=" (TradeDate=    as.Date(TradeDate, format= "%Y%m%d"),
               Quantity=     as.numeric(Quantity),
               TradePrice=   as.numeric(TradePrice),
               Proceeds=     abs(as.numeric(Proceeds)))]
    
    
}

