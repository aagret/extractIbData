


########################
################################################################################
############ TEST AND DEBUG ZONE
####

# 
# # group by quarter
# ibClientNav[, Q:= quarter(TradeDate)]
# 
# ibClientNav[TradeDate > as.Date("2016-12-31") & 
#               TradeDate <= as.Date("2017-09-29"), 
#           .((adjNav[.N] - Hwm[1]) * perfFee / 100,
#             (adjNav[1] + adjNav[.N]) /2 * advFee * 3 / 1200), 
#           by= c("ClientId", "Q")]
# 
# 
# 
cli <- "U1427234" #"U2202020"  
plot(ibClientNav[ClientId== cli,
                 .(TradeDate, exp(cumsum(c(0, ROC(Gross)[-1]))))], type="l", col="red")

lines(ibClientNav[ClientId== cli,
                  .(TradeDate, exp(cumsum(c(0, ROC(fNet)[-1]))))], col="blue")

# 
# 
# 
# ibClientNav[, .(TradeDate, ibNav, adjNav, Gross, cNet,
#               c(0, exp(cumsum(ROC(adjNav)[-1]))),
#               c(0, exp(cumsum(ROC(Gross)[-1]))),
#               c(0, exp(cumsum(ROC(cNet)[-1])))),
#           by= ClientId][ , .SD[.N,], by= ClientId]
# 
# 
# ibClientNav[, .SD[.N], by= ClientId,
#           .SDcols= c("perfFee", "advFee", "Vat", "timbre")]
# 
# ibClientNav[TradeDate %in% as.Date(c("2016-12-30", "2017-03-31", "2017-06-30", "2017-09-29")), .SD, by= ClientId]
# 

####
############ END OF TEST AND DEBUG ZONE
################################################################################
########################
