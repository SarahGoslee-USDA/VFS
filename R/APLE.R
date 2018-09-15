# APLE short form

APLE <- function(soilP, clay, OM, precip, runoff, erosion, manureP = 25, manureSolids = 25, manureWEP = 50, manureIn = 40, fertP = 10, fertIn = 40) {

## Inputs

# soilP: soil test Mehlich 3 phosphorus (mg / kg)
# clay: soil clay (%)
# OM: soil organic matter (%)
# precip: annual precipitation (in)
# runoff: annual runoff (in)
# erosion: annual erosion (ton / ac)
# manureP: manure P applied (kg / ha): default 25
# manureSolids: manure solids (%): default 50
# manureWEP: manure WEP / TP (%): default 50
# manureIn: manure incorporated (%): default 40
# fertP: fertilizer P applied (kg / ha): default 10
# fertIn: fertilizer incorporated (%): default 40



## Conversions and coefficients

# runoff (L / ha)
runoffL <- (runoff * 25.4) / 10 * 100^4 / 1000

# erosion (kg / ha)
erosion <- erosion * 2000 / 2.205 * 2.471

# erosion enrichment ratio
eer <- ifelse(erosion == 0, 0, pmax(exp(2.2 - 0.25 * log(erosion)), 1))
eer[is.na(eer)] <- 0

# soil dissolved P extraction coefficient: default 0.005
Pcoef <- 0.005



# soil organic carbon (%)
SOC <- OM * 0.58

# soil PSP
soilPSP <- pmin(0.9, pmax(0.05, ( -0.053 * log(clay) + 0.001 * (soilP / 2) - 0.029 * SOC + 0.42)))

# soil labile P (mg / kg)
soilPlabile <- soilP / 2

# soil active P (mg / kg)
soilPactive <- soilPlabile * (1 - soilPSP) / soilPSP

# soil stable P (mg / kg)
soilPstable <- soilPactive * 4

# soil organic P (mg / kg)
soilPorganic <- SOC * 10000 / 8 / 14

# soil total P (mg / kg)
soilPtotal <- soilPlabile + soilPactive + soilPstable + soilPorganic


# AB manure WEP mineralized (kg / ha)
manureWEPmineral <- (ifelse(manureSolids < 15, (manureP * (1 - (manureWEP / 100)) * ((100 - manureIn) / 100) * 0.4), (manureP * (1 - (manureWEP / 100)) * ((100 - manureIn) / 100)))) * 0.15

# AC manure WEP available (kg / ha)
manureWEPavail <- ifelse(manureSolids < 15, (manureP * (manureWEP / 100) * ((100 - manureIn) / 100) * 0.4), (manureP * (manureWEP / 100) * ((100 - manureIn) / 100))) + manureWEPmineral

# AD manure PD factor
manurePD <- (runoff / precip)^0.225

# AE fertilizer P available (kg / ha)
fertPavail <- (fertP * ((100 - fertIn) / 100))

# AF fertilizer PD factor
fertPD <- 0.034 * exp((runoff / precip) * 3.4)


# AH soil erosion P loss (kg / ha)
lossErosion <- erosion * soilPtotal * eer / 1000000

# AI soil dissolved P loss (kg / ha)
lossDissolvedSoil <- soilP * 0.5 * Pcoef * runoffL / 1000000

# AJ manure dissolved P loss (kg / ha)
lossDissolvedManure <- manureWEPavail * runoff / precip * manurePD

# AK fertilizer dissolved P loss (kg / ha)
lossDissolvedFert <- fertPavail * runoff / precip * fertPD

# AL total P loss (kg / ha)
lossTotal <- lossErosion + lossDissolvedSoil + lossDissolvedManure + lossDissolvedFert


results <- list(lossErosion = lossErosion, lossDissolvedSoil = lossDissolvedSoil, lossDissolvedManure = lossDissolvedManure, lossDissolvedFert = lossDissolvedFert, lossTotal = lossTotal)

class(results) <- "APLE"
results

}


