

#BRIAN HINCKLEY
#Started 1/23/18
#This script uses linear models and ANOVA to analyze biogeochemical data
library("car")

setwd("../Biogeochemical Assays/")
master = read.csv("2017_Mattamuskeet_SoilBiogeo.csv")


#Mean center all continuous variables
master$Moisture = master$Moisture- mean(master$Moisture)
master$ugNH4.gDM = master$ugNH4.gDM- mean(master$ugNH4.gDM)
master$ugNO3.gDM = master$ugNO3.gDM- mean(master$ugNO3.gDM)



#NITRIFICATION
#assessing the effect of site and all variables on all N rates
results = lm(nitrification ~ Site + Moisture, data = master)
summary(results)

results = lm(nitrification ~ Site + ugNH4.gDM, data = master)
summary(results)

results = lm(nitrification ~ Site + Date, data = master)
summary(results)


results = lm(nitrification ~ Site + Hydrology, data = master)
summary(results)

#NITRIFICATION INTERACTIONS

results = lm(nitrification ~ Site * Moisture, data = master)
summary(results)


results = lm(nitrification ~ Site * ugNH4.gDM, data = master)
summary(results)

results = lm(nitrification ~ Site * Date, data = master)
summary(results)

results = lm(nitrification ~ Site * Hydrology, data = master)
summary(results)

#NITRIFICATION Multiple TERMS MODEL

results = lm(nitrification ~ Site + Date + Moisture + ugNH4.gDM , data = master)
summary(results)

#NITRIFICATION Multiple TERMS MODEL WITH INTERACTIONS


results = lm(nitrification ~ Site * Date  + ugNH4.gDM, data = master)
summary(results)

results = lm(nitrification ~ Site * Moisture + Date + ugNH4.gDM, data = master)
summary(results)

results = lm(nitrification ~ Site * ugNH4.gDM + Moisture + Date , data = master)
summary(results)




#DENITRIFICATION


results = lm(denitrification ~ Site + Moisture , data = master)
summary(results)

results = lm(denitrification ~ Site  + ugNO3.gDM, data = master)
summary(results)

results = lm(denitrification ~ Site + Date, data = master)
summary(results)

results = lm(denitrification ~ Site + Hydrology, data = master)
summary(results)


#DENITRIFICATION INTERACTIONS

results = lm(denitrification ~ Site * Moisture , data = master)
summary(results)

results = lm(denitrification ~ Site * ugNO3.gDM, data = master)
summary(results)

results = lm(denitrification ~ Site * Date, data = master)
summary(results)

results = lm(denitrification ~ Site * Hydrology, data = master)
summary(results)

#DENITRIFICATION Multiple TERMS MODEL

results = lm(denitrification ~ Site + Date  + Moisture , data = master)
summary(results)

#DENITRIFICATION Multiple TERMS MODEL WITH INTERACTIONS

results = lm(denitrification ~ Site * Date + Moisture + ugNO3.gDM, data = master)
summary(results)

results = lm(denitrification ~ Site * Moisture + Date + ugNO3.gDM, data = master)
summary(results)

results = lm(denitrification ~ Site * ugNO3.gDM + Moisture + Date , data = master)
summary(results)

results = lm(denitrification ~ Site * ugNO3.gDM + Moisture , data = master)
summary(results)



#NMineralization

results = lm(Nmineralization ~ Site + Moisture, data = master)
summary(results)

results = lm(Nmineralization ~ Site + ugNO3.gDM , data = master)
summary(results)

results = lm(Nmineralization ~ Site + Date, data = master)
summary(results)

results = lm(Nmineralization ~ Site + Hydrology, data = master)
summary(results)

#NMineralization INTERACTIONS

results = lm(Nmineralization ~ Site * Moisture, data = master)
summary(results)

results = lm(Nmineralization ~ Site * ugNH4.gDM, data = master)
summary(results)

results = lm(Nmineralization ~ Site * Date, data = master)
summary(results)

results = lm(Nmineralization ~ Site * Hydrology, data = master)
summary(results)

#NMINERALIZATION Multiple TERMS MODEL

results = lm(Nmineralization ~ Site + Date  + Moisture + ugNH4.gDM, data = master)
summary(results)

#NMINERALIZATION Multiple TERMS MODEL WITH INTERACTIONS


#N-min
results = lm(Nmineralization ~ Site * Date + ugNH4.gDM + Moisture, data = master)
summary(results)


#N-min
results = lm(Nmineralization ~ Site * Moisture + Date + ugNH4.gDM, data = master)
summary(results)

results = lm(Nmineralization ~ Site * ugNH4.gDM + Moisture + Date , data = master)
summary(results)

