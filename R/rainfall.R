rainfall <-
function(ndays, thiswth, depth, lambda) {
    # note: matlab exprnd specifies mean
    # R rexp specifies rate
    # mean = 1/rate

    if(!missing(thiswth)) {
        depth <- thiswth$depth
        lambda <- thiswth$lambda
    }
    
    rain <- rep(0, ndays)

    # the first day it will rain
    thisrain <- ceiling(rexp(n=1, rate=lambda))

    while(thisrain < ndays) {
        rain[thisrain] <- rexp(n=1, rate=1/depth)
        thisrain <- thisrain + ceiling(rexp(n=1, rate=lambda))
    }

    rain
}


