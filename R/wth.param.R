
wth.param <- function(dly, llim = 0, method = "poisson", year.col = "YEAR", month.col = "MONTH", day.col = "DAY", prcp.col = "PRCP.VALUE", tmin.col = "TMIN.VALUE", tmax.col = "TMAX.VALUE") {

   # calculate weather parameters for climate simulation

    results <- NA

    # discard partial years
    yearlist <- table(dly[[year.col]])
    yearlist <- as.numeric(names(yearlist)[yearlist >= 365])
    nyears <- length(yearlist)

    dly <- dly[ dly[[year.col]] %in% yearlist, ]


    # drop leap days to simplify DOY calculations
    dly <- dly[!(dly[[month.col]] == 2 & dly[[day.col]] == 29), ]

    # remove partial years at beginning and end
    dly <- dly[seq(min(which(dly$MONTH == 1 & dly$DAY == 1)), max(which(dly$MONTH == 12 & dly$DAY == 31))), ]


    # temperature parameters

    tmin <- dly[[tmin.col]]
    tmax <- dly[[tmax.col]]
    tave <- (tmin + tmax)/2

    # A: Mean annual temperature
    A <- mean(tave, na.rm = TRUE)

    # B: Temperature half-amplitude (C)
    B <- diff(range(tave, na.rm = TRUE))/4

    # C: Day of the year with minimum temperature (DOY)
    C <- which.min(apply(matrix(tave, nrow = 365, byrow = FALSE), 1, mean, na.rm = TRUE))


    if(method == "poisson") {
        dly <- dly[!(dly[[month.col]] == 2 & dly[[day.col]] == 29), ]
        dly <- dly[seq(min(which(dly[[month.col]] == 1 & dly[[day.col]] == 1)), max(which(dly[[month.col]] == 12 & dly[[day.col]] == 31))), ]
        prcp <- dly[[prcp.col]]

        # lambda: Mean rainfall inter-arrival frequency (d-1)
        prun <- rle(ifelse(prcp > llim, 1, 0))
        lambda <- 1/mean(prun$lengths[prun$values == 0], na.rm = TRUE)

        # d: Mean rainfall depth
        d <- mean(prcp[prcp > 0], na.rm = TRUE)
        results <- list(lambda = lambda, depth = d)
    }

    if(method == "markov") {

        # calculate APEX weather parameters from a GHCN file 
        # imported with read.dly()
        # APEX theoretical manual, citing Nicks 1974

        # month summary
        # summarize each month across all years

        # treat each month as separate list element, because months have different numbers of days
        mindex <- dly[[month.col]]
        mindex <- factor(mindex, levels=1:12)
        mnames <- paste0("m", sprintf("%02d", 1:12))

        dayT.max <- split(dly[[tmax.col]], mindex)
        dayT.min <- split(dly[[tmin.col]], mindex)
        dayT.mean <- split((dly[[tmax.col]] + dly[[tmin.col]]) / 2, mindex)
        dayP.sum <- split(dly[[prcp.col]], mindex)

        monthP.sum <- sapply(dayP.sum, sum, na.rm=TRUE) / nyears
        prcpmean <- sapply(dayP.sum, function(x)mean(x[x > 0], na.rm=TRUE))


        monthP.max <- sapply(dayP.sum, max, na.rm=TRUE)
        monthT.mmin <- sapply(dayT.min, mean, na.rm=TRUE)
        monthT.mmax <- sapply(dayT.max, mean, na.rm=TRUE)


        # APEX statistics
        # calculate standard deviations of monthly values
        tsdmin <- sapply(dayT.min, sd, na.rm=TRUE)
        tsdmax <- sapply(dayT.max, sd, na.rm=TRUE)


        prsd <- sapply(dayP.sum, function(x)sd(x[x > 0], na.rm=TRUE))

        # skewness of precipitation
        prskew <- sapply(dayP.sum, function(x)e1071::skewness(x[x > 0], na.rm=TRUE))
        prskew[is.nan(prskew)] <- 0

        # number of rain days (days with precip > llim)
        dayP.wetv <- ifelse(dly[[prcp.col]] > llim, 1, 0)
        # assume NA days have no precipitation
        dayP.wetv[is.na(dayP.wetv)] <- 0
        dayP.wet <- split(dayP.wetv, mindex)

        prdays <- sapply(dayP.wet, sum) / sapply(dayP.wet, length)

        # probability of wet after wet, wet after dry

        Ptoday <- c(dayP.wetv, 0)
        Pyest <- c(0, dayP.wetv)

        Pyt <- paste0(Pyest, Ptoday)
        Pyt <- Pyt[-length(Pyt)]
        Pyt <- split(Pyt, mindex)

        prww <- sapply(Pyt, function(x)sum(x == "11")) / sapply(Pyt, function(x)sum(x == "11" | x == "10"))
        prdw <- sapply(Pyt, function(x)sum(x == "01")) / sapply(Pyt, function(x)sum(x == "01" | x == "00"))

    #	# runs of wet, dry days
    #	wet.rle <- lapply(dayP.wet, rle)
    #	runs.wet <- sapply(wet.rle, function(x){mean(x$lengths[x$values == 1])})
    #	runs.dry <- sapply(wet.rle, function(x){mean(x$lengths[x$values == 0])})

        results <- data.frame(
            tmin = monthT.mmin,
            tminsd = tsdmin,
            tmax = monthT.mmax,
            tmaxsd = tsdmax,
            prcp = monthP.sum,
            prcpmean = prcpmean,
            prcpmax = monthP.max,
            prcpsd = prsd,
            prcpskew = prskew,
            prcpwet = prdays,
            prcpww = prww,
            prcpdw = prdw)

    }

    list(params = results, temperature = list(A = A, B = B, C = C), llim = llim, start = min(yearlist), end = max(yearlist))

}


