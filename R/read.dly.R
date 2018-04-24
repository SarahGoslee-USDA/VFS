read.dly <-
function(filename) {


    is.leapyear <- function(year) {
        # https://www.r-bloggers.com/leap-years/
        # http://en.wikipedia.org/wiki/Leap_year
        ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)
    }

    # import GHCN daily weather data
    # https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt

    x <- utils::read.fwf(filename, widths=c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), header=FALSE, stringsAsFactors=FALSE)
    colnames(x) <- c("ID", "YEAR", "MONTH", "ELEMENT", paste0(c("VALUE", "MFLAG", "QFLAG", "SFLAG"), rep(1:31, each=4)))
    x[x == -9999] <- NA

    # fill in missing months
    yrmo <- expand.grid(MONTH = seq(1, 12), YEAR = seq(min(x$YEAR), max(x$YEAR)))[, c(2, 1)]
    yrmody <- yrmo[rep(1:nrow(yrmo), each=31),]
    yrmody$DAY <- rep(1:31, times=nrow(yrmo))

    # all should have these five elements, and maybe more
    # PRCP = Precipitation (tenths of mm)
    # SNOW = Snowfall (mm)
    # SNWD = Snow depth (mm)
    # TMAX = Maximum temperature (tenths of degrees C)
    # TMIN = Minimum temperature (tenths of degrees C)

    el.core <- c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN")
    el.other <- sort(unique(x$ELEMENT))
    el.other <- el.other[!(el.other %in% el.core)]
    el.all <- c(el.core, el.other)

    results <- vector(length(el.all), mode="list")
    names(results) <- el.all

    for(i in seq_along(el.all)) {
        thisset <- subset(x, x$ELEMENT == el.all[i])
        thisset <- merge(yrmo, thisset, all.x=TRUE)
        thisset <- thisset[order(thisset$YEAR, thisset$MONTH), ]

        tsvalue <- as.vector(t(as.matrix(thisset[, grepl("VALUE", colnames(thisset))])))
        tsmflag <- as.vector(t(as.matrix(thisset[, grepl("MFLAG", colnames(thisset))])))
        tsqflag <- as.vector(t(as.matrix(thisset[, grepl("QFLAG", colnames(thisset))])))
        tssflag <- as.vector(t(as.matrix(thisset[, grepl("SFLAG", colnames(thisset))])))

        tsvalue[grepl(" ", tsvalue)] <- NA
        tsmflag[grepl(" ", tsmflag)] <- NA
        tsqflag[grepl(" ", tsqflag)] <- NA
        tssflag[grepl(" ", tssflag)] <- NA

        thisset <- data.frame(VALUE=tsvalue, MFLAG=tsmflag, QFLAG=tsqflag, SFLAG=tssflag, stringsAsFactors=FALSE)

        results[[i]] <- thisset
    }

    results <- do.call("data.frame", list(results, stringsAsFactors=FALSE))
    results <- data.frame(yrmody, results, stringsAsFactors=FALSE)

    # have 12 31-day months. This is not good.
    # can get rid of some of the days automatically
    results <- subset(results, !(results$MONTH == 2 & results$DAY > 29))
    results <- subset(results, !(results$MONTH ==  4 & results$DAY > 30))
    results <- subset(results, !(results$MONTH ==  6 & results$DAY > 30))
    results <- subset(results, !(results$MONTH ==  9 & results$DAY > 30))
    results <- subset(results, !(results$MONTH == 11 & results$DAY > 30))
    results <- subset(results, !(!is.leapyear(results$YEAR) & results$MONTH == 2 & results$DAY > 28))

    # correct units: mm for PRCP, C for TMAX, TMIN
    results$PRCP.VALUE <- results$PRCP.VALUE/10
    results$TMAX.VALUE <- results$TMAX.VALUE/10
    results$TMIN.VALUE <- results$TMIN.VALUE/10

    results
}
