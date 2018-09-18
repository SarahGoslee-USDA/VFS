print.APLE <-
function(x, ...) {
# prints a short summary of APLE object
    nyears <- nrow(x$daily)/365

    if(names(x)[1] == "preVFS") {
	# VFSAPLE object
	cat("Mean annual erosion P reduction by VFS:", round(mean(x$pErosion), 3), "\n")
	cat("Mean annual total P reduction by VFS:", round(mean(x$pTotal), 3), "\n")
    } else {
	cat("Mean annual P loss:", round(mean(x$lossErosion), 3), "\n")
	cat("Mean annual P loss:", round(mean(x$lossDissolvedSoil), 3), "\n")
	cat("Mean annual P loss:", round(mean(x$lossDissolvedManure), 3), "\n")
	cat("Mean annual P loss:", round(mean(x$lossDissolvedFert), 3), "\n")
	cat("Mean annual P loss:", round(mean(x$lossTotal), 3), "\n")
    }

    invisible(x)

}


