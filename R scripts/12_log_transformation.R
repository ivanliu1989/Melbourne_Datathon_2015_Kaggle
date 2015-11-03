# P <- c(-1.1,.0002,-.000003,.74,5.23,-1.214,-.1235,-12,-5,12,-2)
# log_P <- sign(P)*log10(abs(P) + 1 )
# P2 <- sign(log_P)*(10^(abs(log_P))-1)
# P;log_P;P2
# identical(P,P2)
# sum(P-P2)
# plot(log_P)

#################
### Functions ###
#################
log_trans <- function(P) {
    sign(P)*log10(abs(P) + 1 )    
}
raw_trans <- function(log_P){
    sign(log_P)*(10^(abs(log_P))-1)    
}