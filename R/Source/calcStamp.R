
# function to calc total stamp by client  ! usefull ?
calcStamp <- function(db = ibTimbre) {
    
    db <- db[!grepl("Interactive", ClientId), 
             lapply(.SD, sum), .SDcols= c("SoumisSuisse", "SoumisEtr"),
             by= c( "ClientId", "TradeDate")]
    
    db <- db[, timbre:= (SoumisSuisse * 0.075 / 100) + 
                 (SoumisEtr    * 0.15 / 100),][, .(ClientId, TradeDate, timbre)]
    
    return(db)
    
}
