summary.VFS <-
function(object, ...) {
# summarizes a VFS object
# averages across all b values

    runoff <- matrix(object$daily$runoff, nrow=365, byrow=FALSE)
    runoffd <- runoff
    runoffd[runoffd > 0] <- 1


    if(all(is.na(object$MassOut))) 
        modeltype <- "Erosion"
    else
        modeltype <- "VFS"


    nyears <- nrow(object$AnnualRainfall)


    if(modeltype == "VFS") {
    results <- c(
        # load reduction across years with runoff 
        ALR = mean(colMeans(object$AnnualRemovalEfficiency, na.rm=TRUE)), 

        # error propagation: correct SD is sqrt(mean(variance of each trt))
        ALRsd = sqrt(mean(apply(object$AnnualRemovalEfficiency, 2, var, na.rm=TRUE))), 

        # load reduction across all years; no runoff = 100% reduction
        ALRall = mean(apply(object$AnnualRemovalEfficiency, 2, function(object){object[is.na(object)] <- 100; mean(object)})),

        # error propagation: correct SD is sqrt(mean(variance of each trt))
        ALRallsd = sqrt(mean(apply(object$AnnualRemovalEfficiency, 2, function(object){object[is.na(object)] <- 100; var(object)}))), 

        APEA = mean(object$Ftannualavg, na.rm=TRUE), 

        APEAsd = sqrt(mean(apply(object$Ftannual, 1, function(object)var(object[object > 0])) * 100, na.rm=TRUE)),

        SedIn = mean(colSums(object$MassIn, na.rm=TRUE)/nyears), 

        SedInsd = sqrt(mean(apply(object$MassIn, 2, var, na.rm=TRUE))), 

        SedOut = mean(colSums(object$MassOut, na.rm=TRUE)/nyears), 

        SedOutsd = sqrt(mean(apply(object$MassOut, 2, var, na.rm=TRUE))), 

        # SedLoss/SedIn
        TLR = 100 - 100 * mean(colSums(object$MassOut, na.rm=TRUE)/nyears) / mean(colSums(object$MassIn, na.rm=TRUE)/nyears),


        MUSLEIn = mean(colSums(object$MassInMUSLE, na.rm=TRUE)/nyears), 

        MUSLEInsd = sqrt(mean(apply(object$MassInMUSLE, 2, var, na.rm=TRUE))), 

        MUSLEOut = mean(colSums(object$MassOutMUSLE, na.rm=TRUE)/nyears), 

        MUSLEOutsd = sqrt(mean(apply(object$MassOutMUSLE, 2, var, na.rm=TRUE))), 
 
        # SedLoss/SedIn
        MUSLETLR = 100 - 100 * mean(colSums(object$MassOutMUSLE, na.rm=TRUE)/nyears) / mean(colSums(object$MassInMUSLE, na.rm=TRUE)/nyears),

        Runoff = mean(colSums(runoff)),

        Runoffsd = sd(colSums(runoff)),

        RunoffDays = mean(colSums(runoffd)),

        RunoffDayssd = sd(colSums(runoffd)))
        
    } else {
    results <- c(
        SedIn = mean(colSums(object$MassIn, na.rm=TRUE)/nyears), 

        SedInsd = sqrt(mean(apply(object$MassIn, 2, var, na.rm=TRUE))), 

        MUSLEIn = mean(colSums(object$MassInMUSLE, na.rm=TRUE)/nyears), 

        MUSLEInsd = sqrt(mean(apply(object$MassInMUSLE, 2, var, na.rm=TRUE))), 
        
        Runoff = mean(colSums(runoff)),

        Runoffsd = sd(colSums(runoff)),

        RunoffDays = mean(colSums(runoffd)),

        RunoffDayssd = sd(colSums(runoffd)))

    }

    
    results

}


