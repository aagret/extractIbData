
getYtdTrades <- function(tok= token) {
    
    # get yearly trades datas form IB web
    reportId    <- "139144" 
    ibData <- getIbReport(reportId, token)
    
    # remove multi header error
    ibData <- ibData[!ibData$ClientAccountID == 
                         "ClientAccountID",] 
    
    # format datas
    ibData[, ":=" (TradeDate=    as.Date(TradeDate, format= "%Y%m%d"),
                   Quantity=     as.numeric(Quantity),
                   TradePrice=   as.numeric(TradePrice),
                   Proceeds=     abs(as.numeric(Proceeds)))]
    
    # remove multi header error
    ibData <- ibData[!ibData$ClientAccountID == "ClientAccountID",] 
    
    # format datas
    ibData[, ":=" (TradeDate=    as.Date(TradeDate, format= "%Y%m%d"),
                   Quantity=     as.numeric(Quantity),
                   TradePrice=   as.numeric(TradePrice),
                   Proceeds=     abs(as.numeric(Proceeds)))] 
    
    return(IbData)

}

