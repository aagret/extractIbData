
# function to extract Artha advisory fees from IB
getAdvFee <- function(tok= token) {
    
    # extract advisory Fees
    reportId <- "265101"
    advFee <- getIbReport(reportId, token)
    
    # remove useless columns
    advFee[Fee.Type == "Fees", .(ClientAccountID, Date, Net)]
    
    # format datas adn rename columns
    advFee[, Date:= as.Date(Date, format= "%Y%m%d")]
    colnames(advFee) <- c("ClientId", "Date", "advFee")
    
    return(advFee)
    
}