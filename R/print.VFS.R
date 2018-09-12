print.VFS <-
function(x, ...) {
# prints a short summary of VFS object
    nyears <- nrow(x$daily)/365

    if(all(is.na(x$MassOut))) 
        modeltype <- "Erosion"
    else
        modeltype <- "VFS"

    cat("\n\n", modeltype, "model: ", nyears, "years\n")

    if(modeltype == "VFS") {
        cat("Mean annual load into vegetated filter strip:\n")
        print(colMeans(x$AnnualMassIn, na.rm=TRUE))
        cat("Mean annual load out of vegetated filter strip:\n")
        print(colMeans(x$AnnualMassOut, na.rm=TRUE))
        cat("Mean annual removal efficiency:\n")
        print(colMeans(x$AnnualRemovalEfficiency, na.rm=TRUE))
        cat("\nMean annual load into vegetated filter strip (MUSLE):\n")
        print(colMeans(x$AnnualMassInMUSLE, na.rm=TRUE))
        cat("Mean annual load out of vegetated filter strip (MUSLE):\n")
        print(colMeans(x$AnnualMassOutMUSLE, na.rm=TRUE))
        cat("Mean annual removal efficiency (MUSLE):\n")
        print(colMeans(x$AnnualRemovalEfficiencyMUSLE, na.rm=TRUE))
    } else {
        cat("Mean annual sediment load:\n")
        print(colMeans(x$AnnualMassIn, na.rm=TRUE))
        cat("Mean annual sediment load (MUSLE):\n")
        print(colMeans(x$AnnualMassInMUSLE, na.rm=TRUE))
    }
    invisible(x)

}


