print.VFS <-
function(x, ...) {
# prints a short summary of VFS x
    nyears <- nrow(x$daily)/365

    if(all(is.na(x$MassOut))) 
        modeltype <- "Erosion"
    else
        modeltype <- "VFS"

    cat("\n\n", modeltype, "model: ", nyears, "years\n")

    if(modeltype == "VFS") {
        cat("Mean annual load into vegetated filter strip:\n")
        print(colMeans(x$AnnualLoadIn, na.rm=TRUE))
        cat("\nMean annual load out of vegetated filter strip:\n")
        print(colMeans(x$AnnualLoadOut, na.rm=TRUE))
        cat("\nMean annual removal efficiency:\n")
        print(colMeans(x$AnnualRemovalEfficiency, na.rm=TRUE))
    } else {
        cat("Mean annual sediment load:\n")
        print(colMeans(x$AnnualLoadIn, na.rm=TRUE))
    }
    invisible(x)

}


