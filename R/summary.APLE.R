summary.APLE <-
function(object, ...) {
# summarizes an APLE object
# from either APLE or VFSAPLE

    if(names(object)[1] == "preVFS") {
	# VFSAPLE object
	results <- c(
	    AnnualErosionPRemoval = mean(object$pErosion),
	    AnnualErosionPRemovalsd = sd(object$pErosion),
	    AnnualTotalPRemoval = mean(object$pTotal),
	    AnnualTotalPRemovalsd = sd(object$pTotal))
    } else {
	results <- c(
	    AnnualLossErosion = mean(object$lossErosion),
	    AnnualLossDissolvedSoil = mean(object$lossDissolvedSoil),
	    AnnualLossDissolvedManure = mean(object$lossDissolvedManure),
	    AnnualLossDissolvedFert = mean(object$lossDissolvedFert),
	    AnnualLossTotal = mean(object$lossTotal))
    }

    results

}


