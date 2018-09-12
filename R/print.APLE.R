print.APLE <-
function(x, ...) {
# prints a short summary of APLE object
    nyears <- nrow(x$daily)/365

    if(names(x)[1] == "preVFS") {
	# VFSAPLE object
	cat("Mean annual erosion P reduction by VFS:", mean(x$pErosion), "\n")
	cat("Mean annual total P reduction by VFS:", mean(x$pTotal), "\n")
    } else {
	cat("Mean annual P loss:", mean(x$lossTotal), "\n")
    }

    invisible(x)

}


