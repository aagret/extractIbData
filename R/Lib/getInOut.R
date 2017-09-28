
# function to extract deposit/withdrawals from UB accounts
getInOut <- function(token) {
    
    # exctract in/out
    reportId <- "265392"
    inOut    <- getIbReport(reportId, token)
    
    # format data and remove useless columns
    inOut[Activity.Code == "DEP", .(ClientAccountID, Report.Date, Amount)]
    inOut[, ':=' (Report.Date= as.Date(Report.Date, format= "%Y%m%d"), 
                  Amount= as.numeric(Amount))]
    colnames(inOut) <- c("ClientId", "Date", "InOut")
    
    return(inOut)
    
}