

library(data.table)

# load datas#
pictet <- fread("pictetTimbre.csv", sep=";")

# remove empty columns
# pictet <- pictet[, which(unlist(lapply(pictet, 
#                                        function(x) !all(is.na(x))))), 
#                  with= FALSE]

# keep only usefull columns
pictet <- pictet[, .(`Trade date`, 
                     `Transaction type`,
                     `Quantity`,
                     `Instrument`,
                     `ISIN`,
                     `Market price in trade currency`,
                     `Trade currency`,
                     `Account nr.`,
                     `Gross amount in trade currency`,
                     `Financial instrument type`)]

colnames(pictet) <- c("Date.Op", "Type", "Nominal", "Description", 
                      "Isin", "TradePrice", "Currency", "Client", 
                      "Proceeds", "Instument")


# format data
pictet[substr(Type, 1,3) == "Red", Type:="REMB"]
pictet[substr(Type, 1,3) == "Sub", Type:="SOUS"]
pictet[substr(Type, 1,3) == "Buy", Type:="ACH"]
pictet[substr(Type, 1,3) == "Sel", Type:="VTE"]

pictet[, Instrument:= "STK"]
pictet[grepl("Fixed", Instrument), Instrument:= "FXD"]
pictet[grepl("fund", Instrument, ignore.case = TRUE), Instrument:= "FND"]


pictet[, ':=' (Date.Op= as.character(strftime(as.Date(Date.Op, "%Y/%m/%d"), 
                                              "%d-%m%-%Y")),
               Nominal= abs(Nominal),
               client= as.character(Client),
               Proceeds= abs(Proceeds))]

# calc stamp amount
pictet[Type == "REMB", ':=' (SoumisSuisse= 0,
                             SoumisEtr= 0,
                             NonSoumis= Proceeds * Fx)]

pictet[Type != "REMB" & Dev == "CHF", ':=' (SoumisSuisse= Proceeds * Fx,
                                            SoumisEtr= 0,
                                            NonSoumis= 0)]


pictet[Type != "REMB" & Dev != "CHF", ':=' (SoumisSuisse= 0,
                                            SoumisEtr= Proceeds * Fx,
                                            NonSoumis= 0)]

# generate contrepartie datas
bkTimbre <- pictet[, lapply(.SD, sum, na.rm=TRUE), 
                   by= c("Date.Op", "Nom"),
                   .SDcols= c("Nominal", "Proceeds",
                              "SoumisSuisse", "SoumisEtr",
                              "NonSoumis")]

bkTrades <- pictet[, head(.SD,1), 
                   by= c("Date.Op", "Nom"), 
                   .SDcols= -c("Nominal", "Proceeds",
                               "SoumisSuisse", "SoumisEtr",
                               "NonSoumis")]

setkey(bkTrades, Date.Op, Nom)
setkey(bkTimbre, Date.Op, Nom)
bkTimbre <- bkTimbre[bkTrades]

bkTimbre[, ":=" (ClientId="Pictet, Lux",
                 NonSoumis= SoumisSuisse + SoumisEtr,
                 SoumisSuisse= 0,
                 SoumisEtr= 0)]

db <- rbind(db, bkTimbre)

# sort ibTimbre
setorder(db, OrderTime, Description, -ClientId)


db[, cliNbr:= .N, by= c("OrderTime","Description")]



# order columns
setcolorder(db, c(1, 16, 3, 4, 5, 7, 
                  8, 2, 9, 6, 12,
                  11, 10, 13, 14, 15))

setkey(db, ClientId, TradeDate)



