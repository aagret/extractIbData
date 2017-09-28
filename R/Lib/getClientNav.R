
# function to extract NAV history from IB clients

getClientNav <- function(token) {
    
    # extract client NAV
    reportId  <- "138916"
    clientNav <- getIbReport(reportId, token)
    
    # remove dual header
    clientNav <- clientNav[ClientAccountID != "ClientAccountID",]
    
    #rerformat data
    clientNav[,':=' (ReportDate= as.Date(ReportDate, format= "%Y%m%d"),
                     Total= as.numeric(Total))]
    colnames(clientNav) <- c("ClientId", "Date", "NAV")
    
    return(clientNav)
    
}