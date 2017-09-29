
# function to extract Artha advisory fees from IB
getAdvFee <- function(tok= token) {
    
    # extract advisory Fees
    reportId <- "265101"
    db <- getIbReport(reportId, token)
    
    setDT(db)
    
    # remove useless columns
    db <- db[Fee.Type == "Fees", .(ClientAccountID, Date, Net)]
    
    # format datas adn rename columns
    db[, Date:= as.Date(Date, format= "%Y%m%d")]
    colnames(db) <- c("ClientId", "TradeDate", "Fee")
    
    return(db)
    
}