wth.param <-
function(dly) {
    # calculate weather parameters for climate simulation

    # drop leap days to simplify DOY calculations
    dly <- dly[!(dly$MONTH == 2 & dly$DAY == 29), ]

    # remove partial years at beginning and end
    dly <- dly[seq(min(which(dly$MONTH == 1 & dly$DAY == 1)), max(which(dly$MONTH == 12 & dly$DAY == 31))), ]

    prcp <- dly$PRCP.VALUE
    tmin <- dly$TMIN.VALUE
    tmax <- dly$TMAX.VALUE
    tave <- (tmin + tmax) / 2

    # lambda: Mean rainfall inter-arrival frequency (d-1)
    prun <- rle(ifelse(prcp > 0, 1, 0))
    lambda <- 1/mean(prun$lengths[prun$values == 0], na.rm=TRUE)

    # d: Mean rainfall depth (mm)
    d <- mean(prcp[prcp > 0], na.rm=TRUE)

    # A: Mean annual temperature (C)
    A <- mean(tave, na.rm=TRUE)

    # B: Temperature half-amplitude (C)
    # B <- (max(tmax, na.rm=TRUE) - min(tmin, na.rm=TRUE)) / 4
    B <- diff(range(tave, na.rm=TRUE)) / 4

    # C: Day of the year with minimum temperature (DOY)
    C <- which.min(apply(matrix(tave, nrow=365, byrow=FALSE), 1, mean, na.rm=TRUE))

    list(lambda = lambda, depth = d, A = A, B = B, C = C, start=min(dly$YEAR), end=max(dly$YEAR))

}
