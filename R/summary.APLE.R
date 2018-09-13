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
	    AnnualTotalPRemovalsd = sd(object$pTotal),

        AnnualLossErosionPre = mean(object$preVFS$lossErosion),
	    AnnualLossDissolvedSoilPre = mean(object$preVFS$lossDissolvedSoil),
	    AnnualLossDissolvedManurePre = mean(object$preVFS$lossDissolvedManure),
	    AnnualLossDissolvedFertPre = mean(object$preVFS$lossDissolvedFert),
	    AnnualLossTotalPre = mean(object$preVFS$lossTotal),

	    AnnualLossErosionPost = mean(object$postVFS$lossErosion),
	    AnnualLossDissolvedSoilPost = mean(object$postVFS$lossDissolvedSoil),
	    AnnualLossDissolvedManurePost = mean(object$postVFS$lossDissolvedManure),
	    AnnualLossDissolvedFertPost = mean(object$postVFS$lossDissolvedFert),
	    AnnualLossTotalPost = mean(object$postVFS$lossTotal))

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


