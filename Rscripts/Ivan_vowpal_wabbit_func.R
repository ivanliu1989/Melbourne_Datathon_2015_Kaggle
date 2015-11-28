# Function used to select variables for each namespace
get_feature_type <- function(X, threshold = 50, verbose = FALSE) {
    q_levels <- function (x)
    {
        if (data.table::is.data.table(x)) {
            unlist(x[, lapply(.SD, function(x) length(unique(x)))])
        }
        else {
            apply(x, 2, function(x) length(unique(x)))
        }
    }
    
    lvs = q_levels(X)
    fact_vars = names(lvs[lvs < threshold])
    num_vars = names(lvs[lvs >= threshold])
    if (verbose) {
        print(data.frame(lvs))
    }
    list(fact_vars = fact_vars, num_vars = num_vars)
}

to_vw <- function(dt, feat, target, path){
    dt[,target] = ifelse(dt[,target] == 'Y', 1, -1)
    dt = data.table::setDT(dt[,feat])
    data_types = get_feature_type(dt[, setdiff(names(dt), target), with=F], threshold = 30)
    print(data_types)
    namespaces = list(n = list(varName = data_types$num_vars, keepSpace=F),
                      c = list(varName = data_types$fact_vars, keepSpace=F))
    dt2vw(dt, path, namespaces, target=target, weight=NULL); system(paste0('head -3 ', path))
    return(dt)
}