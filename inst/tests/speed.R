.tt <- function(name){
    meth <- get(name, envir = .GlobalEnv)
    environment(meth) <- environment(meth)
    ## environment(meth) <- .GlobalEnv
    meth
}

system.time(for(i in 1:10000) .tt("par"))
system.time(for(i in 1:10000) .tt("par")("xlog"))
system.time(for(i in 1:10000) par("xlog"))

system.time(for(i in 1:10000) .tt("is.matrix"))
system.time(for(i in 1:10000) .tt("is.matrix")("xlog"))
system.time(for(i in 1:10000) is.matrix("xlog"))

## almost same overhead independently of the size of code?
system.time(for(i in 1:10000) .tt("plot"))
system.time(for(i in 1:10000) .tt("lm"))
system.time(for(i in 1:10000) .tt("as"))

