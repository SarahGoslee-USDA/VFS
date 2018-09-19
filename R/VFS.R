VFS <-
function(nyears = 1000, thissoil, thisbuffer, rain, temperature, Duration = 2, FieldArea = 4000, VFSwidth = 10.7, VFSslope = 0.02, FieldSlope, z = 1000, a = 1, b = 1.5, carrysoilwater = TRUE, runoffcalc = TRUE) {

    # nyears: number of years to simulate
    # thissoil: soil properties, for instance a row of soildat
    # rain: daily rainfall in mm
    # temperature: daily mean temperature in C
    # thisbuffer: vegetation properties, for instance a row of vegdat
    #    if thisbuffer = NA, simulates erosion only
    # Duration: rainfall length; default is 2 hours
    # FieldArea: area of the field in m^2 (Note that 1 ac = 4047 sq m)
    # VFSwidth: Width, m
    # VFSslope: ft/ft
    # z: rooting zone depth in mm
    # b: flow parameter
    #
    # These options allow skipping certain subcomponents of the model for research purposes.
    # carrysoilwater: if TRUE, saturation is a function of rain, ET, runoff; otherwise soil is kept at field capacity
    # runoffcalc: if TRUE, use intensity and saturation exceedances; if FALSE, runoff == rainfall 

    # Simulates 365-day years
    date.Day <- rep(seq_len(365), times=nyears)
    date.Year <- as.factor(rep(seq_len(nyears), each=365))

    ## Basic Setup
    Tmax <- length(date.Day)
    i <- 1                    #Daily Counter
    Bval <- 1
    Duration <- Duration / 24

    ## convert buffer width to ft
    VFSwidth <- 3.28084 * VFSwidth

    ## Assume a square field
    FieldLength <- sqrt(FieldArea) #m2
    FieldLength <- 3.28084 * FieldLength # ft

    ## If field slope isn't specified, use VFS slope
    if(missing(FieldSlope)) FieldSlope <- VFSslope

    ## Estimate MUSLE LS factor
    LS <- MUSLE.LS(FieldLength, FieldSlope)

    ###################################################################


    # Soil Parameters

    ## Settling velocities [ft/s]
    Vsc <- 0.22
    Vsm <- 0.00047
    Vsf <- 0.0000047

    ## Kinematic viscosity based on temperature
    v <- 1.407 * 10 ^ -5        # At 50 Deg F [ft^2/s]

    Ksat <- thissoil$Ksat       # Saturated hydraulic conductivity, mm/d  
    ThetaSAT <- thissoil$ThetaSAT   # Saturated water content
    ThetaFC <- thissoil$ThetaFC    # Field capacity
    ThetaWP <- thissoil$ThetaWP    # Permanent wilting point

    ## Sediment Particle Size Distribution Entering the VFS
    #PSD for Clay
    fc <- thissoil$fc  # Fraction of particles >0.037mm
    fm <- thissoil$fm  # Fraction of particles 0.004 to 0.037mm
    ff <- thissoil$ff  # Fraction of particles<0.004mm

    # Estimate MUSLE K factor
    Kf <- MUSLE.K(fc, fm, ff)

    ## Also set up Blaney-Criddle parameters p and kc (crop coefficient)
    ## vector of monthly parameters by day
    ## kc in this table is for corn

    BC.p = c(
        rep(6.75, 31), 
        rep(6.72, 28), 
        rep(8.32, 31), 
        rep(8.93, 30), 
        rep(10.01, 31),
        rep(10.09, 30),
        rep(10.22, 31),
        rep(9.55, 31),
        rep(8.39, 30),
        rep(7.75, 31),
        rep(6.73, 30),
        rep(6.54, 31))
    BC.kc = c(
        rep(0.49, 31),
        rep(0.57, 28),
        rep(0.73, 31),
        rep(0.85, 30),
        rep(0.9, 31),
        rep(0.92, 30),
        rep(.92, 31),
        rep(0.91, 31),
        rep(0.87, 30),
        rep(.79, 31),
        rep(0.67, 30),
        rep(0.55, 31))


    ###################################################################


    # Climate Parameters

    temperatureF <- temperature * 1.8 + 32


    ###################################################################


    # Vegetated Buffer Parameters

    hasbuffer <- !(is.na(thisbuffer[1]))

    if(hasbuffer) {
        bg <- thisbuffer$bg / 12
        n <- thisbuffer$n
    } else {
        bg <- n <- NA
    }

    ###################################################################


    # Initialize results vectors
    # set them all to NA rather than 0 as a check on the results
    # all values should be numeric at the end

    S <- rep(NA, Tmax)  # soil water content
    S[1] <- z*ThetaFC   # Initial moisture content is field capacity

    kt <- rep(NA, Tmax)
    ET <- rep(NA, Tmax)
    runoff <- rep(0, Tmax)
    Q <- rep(NA, Tmax)
    fd <- rep(NA, Tmax)  #Flow depth through VFS (fd), ft
 
    R <- rep(NA, Tmax)
    Vm <- rep(NA, Tmax)
    Re <- rep(NA, Tmax)
    Va <- rep(NA, Tmax)
    Nfc <- rep(NA, Tmax)
    Nfm <- rep(NA, Tmax)
    Nff <- rep(NA, Tmax)
    fdc <- rep(NA, Tmax)
    fdm <- rep(NA, Tmax)
    fdf <- rep(NA, Tmax)
    Ft <- rep(NA, Tmax)

    # convert intensity to mm/d
    intensity <- rain/Duration

    # add MUSLE calculation
    peakflow <- rep(NA, Tmax)
    musle <- rep(NA, Tmax)

    ###################################################################
    

    # Begin daily loop

    for(i in seq_len(Tmax)) {

        dayofyr <- date.Day[i]

        #Define Blaney-Criddle parameters p and kc (crop coefficient)
        p <- BC.p[dayofyr]
        kc <- BC.kc[dayofyr]    

        #Define Blaney Criddle Parameter kt (temperature coefficient)
        if(temperatureF[i] < 36) {
            kt[i] <- 0.3
        } else {
            kt[i] <- 0.0173 *temperatureF[i] - 0.314
        }


        #Calculate ET only for days when rainfall doesn't occur
        #Modify the PET for soil moisture stress conditions
        if(rain[i] == 0) {
            ET[i] <- kt[i] * kc * temperatureF[i] * p/100/30*25.4 # in mm
            if(ET[i] > (S[i]-ThetaWP*z)) {
               ET[i] <- S[i]-ThetaWP*z
            }
        } else {
            ET[i] <- 0
        }


        ## Trigger Surface Runoff

        # start with no runoff

        if(runoffcalc) {
            # if intensity is greater than saturated hydraulic conductivity:
            ## Horton Runoff (Intensity exceedance) 
            if(intensity[i] > Ksat) {

	        # check soil capacity, route exceedance to runoff
		runoff[i] <- rain[i] - min(Ksat * Duration, ((ThetaSAT*z - S[i])))

            } else {

                # if intensity is less, but the soil is very saturated # changed here: scg
                #Dunne Runoff (Saturation exceedance)
                # rainfall gt difference btw what soil can hold and what it is holding
                if(rain[i] > (ThetaSAT*z - S[i])) {
                    # fills the soil capacity, and the rest runs off
                    runoff[i] <- rain[i] - (ThetaSAT*z - S[i])
                }
            }
        } else {
            runoff[i] <- rain[i]
        }


        ## Storage Calculations (Water Balance)
        # added check for saturation, but also fixed Ksat calculations so this should never happen
        if(i < Tmax) {
            if(carrysoilwater) {
                if(dayofyr == 365) {
                    S[i+1] <- ThetaFC*z #Reset to field capacity at the beginning of each of year
                } else {
                    S[i+1] <- min(ThetaSAT*z, S[i] + rain[i] - ET[i] - runoff[i])    #Soil moisture, mm
                }
            } else {
                # always kept at field capacity
                S[i+1] <- ThetaFC*z
            }
        }


	# calculate discharge
	if(runoff[i] > 0) {
	    Q[i] <- runoff[i]/1000*FieldArea	#m^3/day
	    Q[i] <- Q[i] * 35.3147              #Convert to ft^3/day
	    Q[i] <- Q[i] / (Duration * 24) / 3600            #Convert to ft^3/sec

        thisrunoff <- runoff[i]/1000*FieldArea # m3
        thisarea <- FieldArea/10000 # ha
        peakflow[i] <- peak(thisrunoff / Duration, thisarea)

        musle[i] <- MUSLE(thisrunoff, peakflow[i], thisarea, K = Kf, LS = LS)

	} else {
	    Q[i] <- 0
        musle[i] <- 0
	}


        if(hasbuffer) {
            ## Flow through VFS
	    if(runoff[i] > 0) {
		q <- Q[i]

		fun <- function(x, n, bg, VFSslope, q) {
		    y <- ((1.5/n*(x)^(5/3)*(bg/(2*x+bg))^(2/3)*VFSslope^(1/2))-q) #Solve for flow depth, x
		y
		}
		fd[i] <- nleqslv::nleqslv(0.0001, fun, n=n, bg=bg, VFSslope=VFSslope, q=q)$x #Flow depth through VFS (fd), ft

		#Flow Calculations
		# Calculate the hydraulic radius, R
		R[i] <- bg*fd[i]/(2*fd[i]+bg)#[ft]
		# Calculate Manning's velocity,Vm
		Vm[i] <- (1.5*R[i]^(2/3)*VFSslope^(1/2))/n #[ft/s]
		# Calculate Reynold's number Re
		Re[i] <- Vm[i]*R[i]/v
		# Calculate the actual shear stress
		Va[i] <- (32.2*fd[i]*VFSslope)^(1/2)#[ft/s]

	        #Sediment Calculations

		#Trapping Efficiencies
		#Individual particle sizes (fine, medium, and coarse

                #Fall Numbers
                Nfc[i] <- Vsc*VFSwidth /(Vm[i]*fd[i])# Fall number for coarse
                Nfm[i] <- Vsm*VFSwidth/(Vm[i]*fd[i])# Fall number for medium
                Nff[i] <- Vsf*VFSwidth/(Vm[i]*fd[i]) # Fall number for fine

                #Trapping Efficiencies
                fdf[i] <- exp(-1.05*10^-3*Re[i]^(0.82)*Nff[i]^(-0.91))
                fdm[i] <- exp(-1.05*10^-3*Re[i]^(0.82)*Nfm[i]^(-0.91))
                fdc[i] <- exp(-1.05*10^-3*Re[i]^(0.82)*Nfc[i]^(-0.91))
            
                #Total trapping efficiency of VFS
                Ft[i] <- fdc[i]*fc+fdm[i]*fm+fdf[i]*ff

	    } else {
		fd[i] <- 0
		R[i] <- 0
		Vm[i] <- 0
		Re[i] <- 0
		Va[i] <- 0

                Nfc[i] <- 0
                Nfm[i] <- 0
                Nff[i] <- 0

                fdf[i] <- 0
                fdm[i] <- 0
                fdc[i] <- 0

                Ft[i] <- 0
            }

        }   # End vegetated filter strip calculations
    }       # End daily loop

    ###################################################################


    ## Two methods for determining sediment: C-Q and MUSLE
    ## C-Q was used in Gall et al. 2018


    ## Determine Sediment Load
    #C <- aQ^b

    bnames <- paste0("b", sprintf("%02d", 10*b))

    # explicitly mark years with no runoff
    runoffannual <- matrix(runoff, ncol=365, byrow=TRUE)
    runoffannual <- rowSums(runoffannual)

    Volume <- 1000 * runoff/1000*FieldArea	# l/day

    if(length(b) > 1) {

        # multiple b values

        Conc <- data.frame(sapply(b, function(x){a * Q ^ x}))
        colnames(Conc) <- bnames

        Load <- data.frame(sweep(Conc, 1, Volume, "*")) 
        colnames(Load) <- bnames
        Load <- Load / (1000*1000)

        AnnualLoadIn <- aggregate(Load, by=list(date.Year), sum)[, -1, drop = FALSE]
        colnames(AnnualLoadIn) <- bnames


        if(hasbuffer) {

            # multiple b values
            # VFS model

            MassRemoved <- data.frame(sweep(Load, 1, Ft, "*"))
            colnames(MassRemoved) <- bnames

            MassOut <- Load - MassRemoved
            colnames(MassOut) <- bnames

            AnnualLoadOut <- aggregate(MassOut, by=list(date.Year), sum)[, -1, drop = FALSE]
            colnames(AnnualLoadOut) <- bnames

            AnnualRemovalEfficiency <- (1 - AnnualLoadOut/AnnualLoadIn) * 100
            AnnualRemovalEfficiency[is.na(AnnualRemovalEfficiency)] <- 0

            Ftannual <- matrix(Ft, ncol = 365, byrow = TRUE) * 100

            Ftannualavg <- apply(Ftannual, 1, function(x)mean(x[x > 0]))

            AnnualLoadIn[runoffannual == 0, ] <- NA
            AnnualLoadOut[runoffannual == 0, ] <- NA
            AnnualRemovalEfficiency[runoffannual == 0, ] <- NA

        } else {

            # multiple b values
            # erosion only model

            MassRemoved <- data.frame(matrix(NA, nrow=nrow(Conc), ncol=length(b)))
            colnames(MassRemoved) <- bnames

            MassOut <- MassRemoved

            AnnualLoadOut <- MassRemoved

            AnnualRemovalEfficiency <- MassRemoved

            Ftannual <- matrix(NA, ncol=365, nrow = nyears)

            Ftannualavg <- rep(NA, length = nyears)
        }

    } else {
       
        # one b value

        Conc <- data.frame(dat = a * Q ^ b)
        colnames(Conc) <- bnames
 
        Load <- data.frame(dat = Conc * Volume) # mg?
        colnames(Load) <- bnames
        Load <- Load / (1000*1000)

        AnnualLoadIn <- aggregate(Load, by=list(date.Year), sum)[, -1, drop = FALSE]
        colnames(AnnualLoadIn) <- bnames


        if(hasbuffer) {

            # one b value
            # VFS model

            MassRemoved <-  data.frame(dat = Load * Ft)
            colnames(MassRemoved) <- bnames

            MassOut <- Load - MassRemoved

            AnnualLoadOut <- aggregate(MassOut, by=list(date.Year), sum)[, -1, drop = FALSE]

            AnnualRemovalEfficiency <- (1 - AnnualLoadOut/AnnualLoadIn) * 100
            AnnualRemovalEfficiency[is.na(AnnualRemovalEfficiency)] <- 0

            Ftannual <- matrix(Ft, ncol=365, byrow=TRUE) * 100

            Ftannualavg <- apply(Ftannual, 1, function(x)mean(x[x > 0]))

            AnnualLoadIn[runoffannual == 0, ] <- NA
            AnnualLoadOut[runoffannual == 0, ] <- NA
            AnnualRemovalEfficiency[runoffannual == 0, ] <- NA


        } else {

            # one b value
            # erosion only model
    
            MassRemoved <- data.frame(var = rep(NA, length(Conc)))
            colnames(MassRemoved) <- bnames

            MassOut <- MassRemoved

            AnnualLoadOut <- MassRemoved

            AnnualRemovalEfficiency <- MassRemoved

            Ftannual <- matrix(NA, ncol=365, nrow = nyears)

            Ftannualavg <- rep(NA, length = nyears)
 
        }
    }


    # Repeat VFS calculations with MUSLE erosion values



    AnnualLoadInMUSLE <- aggregate(musle, by=list(date.Year), sum)[, -1, drop = FALSE]

    if(hasbuffer) {

        # VFS model

        MassRemovedMUSLE <-  data.frame(dat = musle * Ft)

        MassOutMUSLE <- musle - MassRemovedMUSLE

        AnnualLoadOutMUSLE <- aggregate(MassOutMUSLE, by=list(date.Year), sum)[, -1, drop = FALSE]

        AnnualRemovalEfficiencyMUSLE <- (1 - AnnualLoadOutMUSLE/AnnualLoadInMUSLE) * 100
        AnnualRemovalEfficiencyMUSLE[is.na(AnnualRemovalEfficiencyMUSLE)] <- 0

        AnnualLoadInMUSLE[runoffannual == 0, ] <- NA
        AnnualLoadOutMUSLE[runoffannual == 0, ] <- NA
        AnnualRemovalEfficiencyMUSLE[runoffannual == 0, ] <- NA


    } else {

        # erosion only model
    
        MassRemovedMUSLE <- data.frame(var = rep(NA, length(musle)))

        MassOutMUSLE <- MassRemovedMUSLE

        AnnualLoadOutMUSLE <- MassRemovedMUSLE

        AnnualRemovalEfficiencyMUSLE <- MassRemovedMUSLE
 
    }

    musle <- data.frame(MUSLE = musle)

    ## Annual Calculations
    AnnualRainfall <- aggregate(rain, by=list(date.Year), sum)[, -1, drop = FALSE]
    AnnualRunoff <- aggregate(runoff, by=list(date.Year), sum)[, -1, drop = FALSE]

    output <- list(daily = data.frame(rain=rain, temperature=temperature, S=S, kt=kt, ET=ET, intensity=intensity, runoff=runoff, Q=Q, fd=fd, R=R, Vm=Vm, Re=Re, Va=Va, Nfc=Nfc, Nfm=Nfm, Nff=Nff, fdc=fdc, fdm=fdm, fdf=fdf, Ft=Ft, peakflow=peakflow), field=c(clay=ff, area=FieldArea), AnnualRainfall=AnnualRainfall, AnnualRunoff=AnnualRunoff, Conc=Conc, MassIn=Load, MassOut=MassOut, MassRemoved=MassRemoved, AnnualMassIn=AnnualLoadIn, AnnualMassOut=AnnualLoadOut, AnnualRemovalEfficiency=AnnualRemovalEfficiency, MassInMUSLE=musle, MassOutMUSLE=MassOutMUSLE, MassRemovedMUSLE=MassRemovedMUSLE, AnnualMassInMUSLE=AnnualLoadInMUSLE, AnnualMassOutMUSLE=AnnualLoadOutMUSLE, AnnualRemovalEfficiencyMUSLE=AnnualRemovalEfficiencyMUSLE, Ftannual=Ftannual, Ftannualavg=Ftannualavg)

    class(output) <- "VFS"

    output

}

