
# function to compute high water mark
calcHwm <- function(nav = aNav) {
    
    Hwm <- shift(cummax(nav), fill = 0)
    Hwm[1] <- nav[1]
    
    return(Hwm)
    
}
