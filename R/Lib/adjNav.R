
# function to adjust Nav to account fees under proper date
adjNav <- function(db= clientNav) {
    
    db <- clientNav[Fee != 0, .(ClientId, TradeDate, Fee)]
    
    db[, Qq:=as.Date(cut(as.Date(cut(TradeDate, "quarter")), "quarter")) - 1]
    db[, PayDate:= TradeDate]
    
    colnames(db)[3] <- "QFee"
    
    # change date for last workday
    db[strftime(Qq, "%u") == 6, Qq:= Qq - 1]
    db[strftime(Qq, "%u") == 7, Qq:= Qq - 2]
    
    setkey(db, ClientId, TradeDate)
    
    db <- db[clientNav, roll=-Inf]
    
    db[, Net:= NAV ]
    db[TradeDate < PayDate & TradeDate >= Qq, Net:= Net - QFee, by= ClientId]
    
    return(db$Net)
    
}
    