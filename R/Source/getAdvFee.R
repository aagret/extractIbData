
# function to extract Artha advisory fees from IB
getIbFees <- function(tok= ibToken) {
    
    # extract advisory paidFees
    reportId <- "265101"
    
    db <- getIbReport(reportId, ibToken)
    
    setDT(db)
    
    # remove useless columns
    db <- db[Fee.Type == "Fees", .(ClientAccountID, Date, Net)]
    
    # format datas adn rename columns
    db[, Date:= as.Date(Date, format= "%Y%m%d")]
    
    colnames(db) <- c("ClientId", "TradeDate", "paidFee")
    
    return(db)
    
}
