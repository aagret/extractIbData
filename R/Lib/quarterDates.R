
# function to return first and last day of the Quarter
quarterDates <- function(dt= Date) {
    
    endDt   <- as.Date(cut(as.Date(cut(dt, "quarter")), "quarter")) -1
    startDt <- as.Date(cut(as.Date(cut(dt, "quarter")) - 90, "quarter")) - 1
    
    return(c(endDt, startDt))
    
}

