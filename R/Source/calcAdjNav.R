
# function to adjust Nav to account fees under proper date
calcAdjNav <- function(db= ibClientNav) {
    
    db <- ibClientNav[paidFee != 0, .(ClientId, TradeDate, paidFee)]
    
    db[, Qq:= as.Date(cut(as.Date(cut(TradeDate, "quarter")), "quarter")) - 1]
    
    db[, PayDate:= TradeDate]
    
    colnames(db)[3] <- "QFee"
    
    # change date for last workday
    db[strftime(Qq, "%u") == 6, Qq:= Qq - 1]
    db[strftime(Qq, "%u") == 7, Qq:= Qq - 2]
    
    setkey(db, ClientId, TradeDate) #utile?
    
    db <- db[ibClientNav, roll= -Inf]
    
    db[, adjNav:= ibNav ]
    db[TradeDate < PayDate & TradeDate >= Qq, adjNav:= adjNav - QFee, by= ClientId]
    
    db[TradeDate == Qq, dueFee:= QFee, by= ClientId]
    db[TradeDate != Qq, dueFee:= 0, by= ClientId]
    
    db[is.na(dueFee), dueFee:=0]
    
    db <- db[, .(ClientId, TradeDate, adjNav, dueFee)]
    
}
