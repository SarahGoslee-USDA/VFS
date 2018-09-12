VFSAPLE <- function(x, soilP, OM, manureP = 25, manureSolids = 25, manureWEP = 50, manureIn = 40, fertP = 10, fertIn = 40) {
    if(!inherits(x, "VFS")) {
	stop("VFSAPLE requires the output of VFS\n")
    }

    # mean annual rainfall in mm
    precip <- x$AnnualRainfall[,1]
    precip <- precip / 25.4 # inches

    # mean annual runoff in mm
    runoff <- x$AnnualRunoff[,1]
    runoff <- runoff / 25.4 # inches

    # field characteristics
    clay <- 100 * x$field[["clay"]] # percent

    # mean annual erosion BEFORE vegetated filter strip t/ha
    erosionPre <- x$AnnualMassInMUSLE[,1]
    # years with no runoff are flagged NA; change them to 0
    erosionPre[is.na(erosionPre)] <- 0
    erosionPre <- erosionPre * 0.44609

    # mean annual erosion AFTER vegetated filter strip t/ha
    erosionPost <- x$AnnualMassOutMUSLE[,1]
    # years with no runoff are flagged NA; change them to 0
    erosionPost[is.na(erosionPost)] <- 0
    erosionPost <- erosionPost * 0.44609



    preVFS <- APLE(soilP, clay, OM, precip, runoff, erosionPre, manureP = 25, manureSolids = 25, manureWEP = 50, manureIn = 40, fertP = 10, fertIn = 40)

    postVFS <- APLE(soilP, clay, OM, precip, runoff, erosionPost, manureP = 25, manureSolids = 25, manureWEP = 50, manureIn = 40, fertP = 10, fertIn = 40)


    # P removal efficiency
    pErosion <- 100 * (1 - postVFS$lossErosion/preVFS$lossErosion)
    pErosion[is.na(pErosion)] <- 0

    pTotal <- 100 * (1 - postVFS$lossTotal/preVFS$lossTotal)
    pTotal[is.na(pTotal)] <- 0

    results <- list(preVFS=preVFS, postVFS=postVFS, pErosion=pErosion, pTotal=pTotal)


    class(results) <- "APLE"
    results


}


