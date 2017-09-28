
#function to get specific Ib report
getIbReport <- function(id= reportId, tok= token){
    
    # current IB default
    baseUrl     <- "https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest?t="
    version     <- "v=2"
    
    # url to query file
    queryUrl     <- paste0(baseUrl, tok, "&q=", id, "&", version)
    
    # get data and parse xml 
    xmlFile <- xmlTreeParse(GET(queryUrl))
    xmlTop  <- xmlRoot(xmlFile) 
    xmlData <- xmlSApply(xmlTop, function(x) xmlSApply(x, xmlValue)) 
    
    # url of requested report
    reportUrl <- paste0(xmlData[2], "?q=", xmlData[1], "&t=", token, "&", version)
    
    
    for (try in 1:20) {
        
        options(stringsAsFactors = FALSE)
        reportDoc <- read.csv(reportUrl, sep=",", header= TRUE)
        
        if (!grepl("FlexStatementResponse", colnames(reportDoc)[1])) break
        Sys.sleep(5)
    }
    
    
    reportDoc <- setDT(reportDoc)
    
    return(reportDoc)
}