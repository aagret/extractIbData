
# function to compute High Water Mark # Usefull ?
calcHwm <- function(nav = adjNav) {
    
    Hwm <- shift(cummax(nav), fill = 0)
    Hwm[1] <- nav[1]
    
    return(Hwm)
    
}
