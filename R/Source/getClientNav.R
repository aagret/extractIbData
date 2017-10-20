
# function to extract IbNav history from IB clients

getClientNav <- function(tok = ibToken) {
    
    # extract client IbNav
    reportId  <- "138916"
    
    db <- getIbReport(reportId, tok)
    
    # rename columns
    colnames(db) <- c("ClientId", "TradeDate", "ibNav")
    
    # remove TimbreDual header
    db <- db[ClientId != "ClientAccountID", ]
    
    # reformat data
    db[,':=' (TradeDate= as.Date(TradeDate, format= "%Y%m%d"),
              ibNav= as.numeric(ibNav))]
    
}
