dt[, FEAT:= cumsum(c), by = A]

dt[, LAGFEAT := shift(FEAT, 0, -1), by = A]



shift<-function(x,fill,shift_by){
    stopifnot(is.numeric(shift_by))
    stopifnot(is.numeric(x))
    
    if (length(shift_by)>1)
        return(sapply(shift_by,shift, x=x))
    
    out<-NULL
    abs_shift_by=abs(shift_by)
    if (shift_by > 0 )
        out<-c(tail(x,-abs_shift_by),rep(fill,abs_shift_by))
    else if (shift_by < 0 )
        out<-c(rep(fill,abs_shift_by), head(x,-abs_shift_by))
    else
        out<-x
    out
}

v<-runif(10)

shift(v,0,-1)
