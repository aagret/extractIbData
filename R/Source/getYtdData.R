
getYtdData <- function(tok= token) {
    
    # get yearly trades datas form IB web
    reportId    <- "139144" 
    db <- getIbReport(reportId, tok)
    
    # remove multi header error
    db <- db[!db$ClientAccountID == "ClientAccountID",]
    colnames(db)[1:2] <- c("ClientId", "Currency")
    
    # format datas
    db[, ":=" (TradeDate=    as.Date(TradeDate, format= "%Y%m%d"),
               Quantity=     as.numeric(Quantity),
               TradePrice=   as.numeric(TradePrice),
               Proceeds=     abs(as.numeric(Proceeds)))]
    

    
    return(db)
    
}

