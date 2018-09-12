MUSLE.K <- function(fc, fm, ff) {
    # PTF from Goslee, unpublished data (manuscript in review)
	Sa <- fc * 100
	Si <- fm * 100
	Cl <- ff * 100

    Sa2 <- Sa^2
    SaI <- 1/Sa
    SaL <- log(Sa)
    Si <- Si
    SiI <- 1/Si
    SiL <- log(Si)
    Cl <- Cl
    Cl2 <- Cl^2
    ClL <- log(Cl)

	Kf <- 0.94141 + -0.00007 * Sa2 + -0.06612 * SaI + -0.04916 * SaL + -0.00242 * Si + 0.01343 * SiI + 0.00512 * SiL + -0.01209 * Cl + 0.00004 * Cl2 + 0.00789 * ClL

	if(Kf < 0.03) Kf <- 0.03
	if(Kf > 0.69) Kf <- 0.69

    Kf
}

####


MUSLE.LS <- function(FieldLength, FieldSlope) {

	slopelength <- FieldLength          # ft
	slopeanglep <- 100 * FieldSlope	    # percent
	slopeangled  <- atan(FieldSlope) * (360 / (2 * pi))    # degrees


	m <- NA
	if(slopeanglep > 5) m <- 0.5
	if(slopeanglep > 3 & slopeanglep <= 5) m <- 0.4
	if(slopeanglep > 1 & slopeanglep <= 3) m <- 0.3
	if(slopeanglep <= 1) m <- 0.2


	LS <- (slopelength / 72.6) ^ m * (65.41 * sin(slopeangled * (2 * pi) / 360) ^ 2 + 4.56 * sin(slopeangled * (2 * pi) / 360) + 0.065)

    LS
}

###

peak <- function(intensity, area, c = 0.25) {
    # intensity: mm/hr
    # area: ha
    
    intensity <- intensity / 25.4   # convert intensity to inch/hour
    area <- area * 2.47105          # convert drainage area to acres

    peakd <- c * intensity * area   # cfs

    peakd * 0.0283168               # return m3/second
}


