
# get ibTimbre
extractIbStamp <- function(db= ibTrades) {

    # extract Timbre datas
    db <- db[AssetClass =="STK",
             .(OrderTime,  AssetClass, Description, 
               Symbol,     Isin,       TradeDate, 
               Currency,   Buy.Sell,   Quantity, 
               TradePrice, Proceeds,   ClientId)]
    
    # add client Domicile
    db[ , Dom:= switch(ClientId,
                       U2202020 = "CH",
                       U1427234 = "CH",
                       NA), 
        by= ClientId]
    
}


# get pictetTimbre
extractPictetStamp <- function() {
    
    db <- fread("pictetTimbre.csv", sep=";")
    
    # add OrderTime
    db$OrderTime <- db$`Performance date`
    db$Symbol    <- ""
    
    # keep only usefull columns and rename
    db <- db[, .(`OrderTime`, `Financial instrument type`,
                 `Instrument`, `Symbol`, `ISIN`,
                 `Performance date`, `Trade currency`,
                 `Transaction type`, `Quantity`,
                 `Market price in trade currency`,
                 `Gross amount in trade currency`,
                 `Account nr.`)]
    
    colnames(db) <- c("OrderTime",  "AssetClass", "Description", 
                      "Symbol",     "Isin",       "TradeDate", 
                      "Currency",   "Buy.Sell",   "Quantity", 
                      "TradePrice", "Proceeds",   "ClientId")
    
    db[, ClientId:= as.character(ClientId)]
    db[, TradeDate:= as.Date(TradeDate, "%Y/%m/%d")]
    db[, OrderTime:= as.Date(OrderTime, "%Y/%m/%d")]
    
    # add client Domicile
    db[ , Dom:= switch(ClientId,
                       `162306.002` = "CH",
                       `162675.001` = "ETR",
                       `164368.002` = "ETR",
                       `167966.001` = "ETR",
                       `169966.002` = "ETR",
                       `170312.001` = "CH",
                       `301335.001` = "CH",
                       NA), 
        by= ClientId]
    
}

#######################

#add Fx histo rates
addFx <- function(db= ibTimbre, fx= ibFxHisto) {
    
    # merge Trades & Fx datas
    setkey(fx, TradeDate, Currency)
    setkey(db, TradeDate, Currency)
    
    db  <- fx[db]                     # merge
    db[Currency == "CHF", Fx:= 1L]                # set CHF Fx to 1    
}

# add missing Isin
addIsin <- function(db= ibTimbre) {
    
    #tickers <- fread ("/home/artha/Alexandre/Tfc/ticker4.csv")
    tickers <- fread ("U:/Tfc/ticker4.csv") # ! check one line only
    tickers <- tickers[, c(3, 2)]
    colnames(tickers) <- c("Symbol", "Isin")
    
    setkey(db, Symbol)
    setkey(tickers, Symbol)
    
    db[Isin == "", Symbol:= paste0(Symbol, " US Equity")]
    db[Isin == "", Isin:= tickers[Symbol, Isin]]
    
    db <- db[, -7]
    
}

# format Stamp datas
formatStamp <- function(db= ibTimbre) {
    
    # change op type
    # db[substr(Buy.Sell, 1,3) == "Red", Buy.Sell:="REMB"]
    # db[substr(Buy.Sell, 1,3) == "Sub", Buy.Sell:="SOUS"]
    # db[substr(Buy.Sell, 1,3) == "Buy", Buy.Sell:="ACH"]
    # db[substr(Buy.Sell, 1,3) == "Sel", Buy.Sell:="VTE"]

    
    db[, Buy.Sell:= switch(tolower(substr(Buy.Sell, 1,3)),
                           red = "REMB",
                           sub = "SOUS",
                           buy = "ACH",
                           sel = "VTE",
                           NA),
       by= Buy.Sell]
    
    # # change asset class type
    # db[, AssetClass:= "STK"]
    
    db[grepl("Fixed", AssetClass), AssetClass:= "FXD"]
    db[grepl("fund",  AssetClass, 
             ignore.case = TRUE),  AssetClass:= "FND"]
    
    # change attributes
    db[, ':=' (Quantity= abs(Quantity),
               ClientId= as.character(ClientId),
               Proceeds= abs(Proceeds))]
    

    
}


# generate IB/Bk counterpart datas
addBkData <- function(db= ibTimbre, bk= "Interactive Brokers, Ldn") {
    
    # sum clients stamp sumbers
    bkTimbre <- db[, lapply(.SD, sum, na.rm=TRUE), 
                   by= c("OrderTime", "Description"),
                   .SDcols= c("Quantity", "Proceeds")]
    
    # copy 1st client datas
    bkTrades <- db[, head(.SD, 1), 
                 by= c("OrderTime", "Description"), 
                 .SDcols= -c("Quantity", "Proceeds")]
    
    setkey(bkTimbre, OrderTime, Description)
    setkey(bkTrades, OrderTime, Description)
    
    # merge datas
    bkTimbre <- bkTimbre[bkTrades]
    
    #add bank's name'
    bkTimbre[, ClientId:= bk]
    
    bkTimbre[, Buy.Sell:= switch(tolower(Buy.Sell),
                                 ach = "VTE",
                                 vte = "ACH",
                                 remb= "ACH",
                                 sous= "VTE",
                                 NA),
             by= Buy.Sell]
                  
    
    db <- rbind(db, bkTimbre)
    
}


MERGE


##### TODO



# calculate stamp
calcStamp <- function(db= ibTimbre) {
    

    db[Currency == "CHF", ':=' (SoumisCH= Proceeds * Fx,
                                SoumisEtr= 0,
                                NonSoumis= 0)]
    
    db[Currency != "CHF", ':=' (SoumisCH= 0,
                                SoumisEtr= Proceeds * Fx,
                                NonSoumis= 0)]
    
    db[Dom == "ETR" &
           AssetClass == "FXD", ':=' (SoumisCH = 0,
                                      SoumisEtr= 0,
                                      NonSoumis= Proceeds * Fx)]
    db[Buy.Sell == "REMB", ':=' (SoumisCH = 0,
                                 SoumisEtr= 0,
                                 NonSoumis= Proceeds * Fx)]
    
    db[db[, .I[.N], 
          by= c("OrderTime", "Description")][["V1"]], 
       ':=' (SoumisCH=  0,
             SoumisEtr= 0,
             NonSoumis= Proceeds * Fx)]

}



# get today's and current quarter Dates
#quarterDates <- quarterDates(Sys.Date())
quarterDates <- as.Date("2016-12-31")

# sort ibTimbre
setorder(db, OrderTime, Description, -ClientId)

db[, cliNbr:= .N, by= c("OrderTime","Description")]


pict <- extractPictetStamp()
pict <- addFx(pict)
pict <- addIsin(pict)
pict <- formatStamp(pict)
pict <- addBkData(pict, "Pictet Lux")
pict <- calcStamp(pict)


ib <- extractIbStamp(ibTrades)
ib <- addFx(ib)
ib <-addIsin(ib)
ib <- formatStamp(ib)
ib <- addBkData(ib)
ib <- calcStamp(ib)

ib[TradeDate >= as.Date("2017-06-30") & 
       TradeDate <= as.Date("2017-09-30"), 
   lapply(.SD, sum, na.rm=TRUE), 
   .SDcols=c("SoumisCH", "SoumisEtr", "NonSoumis")]



