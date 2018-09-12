rainfall <- function(ndays, thiswth, months) {

    if(missing(ndays)) ndays <- length(months)
    llim <- thiswth$llim
    thiswth <- thiswth$params

    rain <- rep(NA, ndays)

    if (any(names(thiswth) == "lambda")) {
        # use original VFS poisson simulation
        # note: matlab exprnd specifies mean
        # R rexp specifies rate
        # mean = 1/rate

        depth <- thiswth$depth
        lambda <- thiswth$lambda
        rain <- rep(0, ndays)

        # the first day it will rain
        thisrain <- ceiling(rexp(n = 1, rate = lambda))
        while (thisrain < ndays) {
            rain[thisrain] <- rexp(n = 1, rate = 1/depth)
            thisrain <- thisrain + ceiling(rexp(n = 1, rate = lambda))
        }
    } 
    if(any(names(thiswth) == "prcpdw")) {
        # Markov chain model
        if(missing(months)) {
            # assume starts on January 1, 365-day year
            months <- rep(seq_len(12), times = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
            months <- rep(months, length.out = ndays)
        }

        prevday <- 0

        # using a loop while I work out the details
        for(i in seq_along(rain)) {
            thismonth <- months[i]
            if(prevday == 0) {
                thisprob <- thiswth$prcpdw[thismonth]
            } else {
                thisprob <- thiswth$prcpww[thismonth]
            }
            if(runif(1) <= thisprob) {
                # it rains! 
                prevday <- 1

                # skewed normal distribution
                R6 <- thiswth$prcpskew[thismonth] / 6
                X1 <- (rnorm(1) - R6) * R6 + 1
                XLV <- (X1 ^ 3 - 1) * 2 / thiswth$prcpskew[thismonth]
                rain[i] <- max(XLV * thiswth$prcpsd[thismonth] + thiswth$prcpmean[thismonth], llim)

            } else {
                # no rain
                prevday <- 0
                rain[i] <- 0

            }
        }
    }
    rain
}


