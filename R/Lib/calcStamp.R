
# function to calc total stamp by client
calcStamp <- function(db = timbre) {
    
    db <- db[!grepl("Interactive", ClientId), 
             lapply(.SD, sum), .SDcols= c("SoumisSuisse", "SoumisEtr"),
             by= c( "ClientId", "TradeDate")]
    
    db <- db[, TF:= (SoumisSuisse * 0.075 / 100) + 
                 (SoumisEtr    * 0.15 / 100),][, .(ClientId, TradeDate, TF)]
    
    return(db)
    
}