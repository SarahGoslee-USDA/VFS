summary.VFS <-
function(object, ...) {
# summarizes a VFS object
# averages across all b values

    runoff <- matrix(object$dat$runoff, nrow=365, byrow=FALSE)
    runoff[runoff > 0] <- 1


    if(all(is.na(object$MassOut))) 
        modeltype <- "Erosion"
    else
        modeltype <- "VFS"


    if(modeltype == "VFS") {
    list(
        ALR = mean(colMeans(object$AnnualRemovalEfficiency, na.rm=TRUE)), 

        # error propagation: correct SD is sqrt(mean(variance of each trt))
        ALRsd = sqrt(mean(apply(object$AnnualRemovalEfficiency, 2, var, na.rm=TRUE))), 

        # load reduction across 1000 years
        ALR1000 = mean(apply(object$AnnualRemovalEfficiency, 2, function(object){object[is.na(object)] <- 100; mean(object)})),

        # error propagation: correct SD is sqrt(mean(variance of each trt))
        ALR1000sd = sqrt(mean(apply(object$AnnualRemovalEfficiency, 2, function(object){object[is.na(object)] <- 100; var(object)}))), 

        APEA = mean(object$Ftannualavg, na.rm=TRUE), 

        APEAsd = sqrt(mean(apply(object$Ftannual, 1, function(object)var(object[object > 0])) * 100, na.rm=TRUE)),

        SedIn = mean(colSums(object$Load, na.rm=TRUE)/1000), 

        SedInsd = sqrt(mean(apply(object$Load, 2, var, na.rm=TRUE))), 

        SedLoss = mean(colSums(object$MassOut, na.rm=TRUE)/1000), 

        SedLosssd = sqrt(mean(apply(object$MassOut, 2, var, na.rm=TRUE))), 

        # SedLoss/SedIn
        TLR = 100 - 100 * mean(colSums(object$MassOut, na.rm=TRUE)/1000) / mean(colSums(object$Load, na.rm=TRUE)/1000),
        
        RunoffDays = mean(colSums(runoff)),

        RunoffDayssd = sd(colSums(runoff)),

        Days = nrow(object$dat))
        
    } else {
    list(
        SedIn = mean(colSums(object$Load, na.rm=TRUE)/1000), 

        SedInsd = sqrt(mean(apply(object$Load, 2, var, na.rm=TRUE))), 

        RunoffDays = mean(colSums(runoff)),

        RunoffDayssd = sd(colSums(runoff)),

        Days = nrow(object$dat))
        
    }

}


