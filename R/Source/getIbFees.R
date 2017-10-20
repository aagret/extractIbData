
# function to extract Artha advisory fees from IB
getIbFees <- function(tok= ibToken) {
    
    # extract advisory paidFees
    reportId <- "265101"
    
    db <- getIbReport(reportId, tok)
    
    # rename Columns
    colnames(db)[c(1, 4, 6)] <- c("ClientId", "TradeDate", "paidFee")    
    
    # remove useless columns
    db <- db[Fee.Type == "Fees", .(ClientId, TradeDate, paidFee)]
    
    # format datas
    db[, TradeDate:= as.Date(TradeDate, format= "%Y%m%d")]
    
}
