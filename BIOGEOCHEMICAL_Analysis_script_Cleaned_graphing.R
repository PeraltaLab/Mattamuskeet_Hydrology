

#BRIAN HINCKLEY
#Started 1/23/18
#This script uses linear models and ANOVA to analyze biogeochemical data
library("car")

master = read.csv("2017_Mattamuskeet_SoilBiogeo.csv")
setwd("../../Biogeochemical Assays/")
#All Seasons Rates Boxplots




#Mean center all continuous variables
master$Moisture = master$Moisture- mean(master$Moisture)
master$ugNH4.gDM = master$ugNH4.gDM- mean(master$ugNH4.gDM)
master$ugNO3.gDM = master$ugNO3.gDM- mean(master$ugNO3.gDM)


ag_master = master[master$Site == 'Ag',]
MSM_master = master[master$Site == 'MSM',]

#THIS RUN WAS DONE ON NON-MEAN CENTERED DATA FOR THE FIRST RUN
#NITRIFICATION
#assessing the effect of site and all variables on all N rates

#NITRIFICATION Multiple TERMS MODEL
setwd("LM_PLOTS")
#AG
results = lm(nitrification ~ Moisture + Mean_AIR_TEMP..C.. + Soil_C_wt_. + Soil_N_wt_. + Soil_pH, data = ag_master)
summary(results)

jpeg("Ag_LM_Nitrification.jpg", width = 12, height = 10, units="in", res=350)
plot(results$model$nitrification, results$fitted.values, xlim=c(0,10), ylim=c(0,10), xlab=expression(Lab~Measured~Nitrification),
   ylab=expression(Predicted~Nitrification))
text(2,7,paste("R²=0.66, p=2.361e-07"),cex=1)
dev.off()

#MSM
results = lm(nitrification ~ Moisture + Mean_AIR_TEMP..C.. + C.N_Ratio, data = MSM_master)
summary(results)

jpeg("MSM_LM_Nitrification.jpg", width = 12, height = 10, units="in", res=350)
plot(results$model$nitrification, results$fitted.values, xlim=c(-0.3,1.5), ylim=c(-0.3,1.5), xlab=expression(Lab~Measured~Nitrification),
     ylab=expression(Predicted~Nitrification))
text(0,1,paste("R²=0.34, p=0.0009886"),cex=1)
dev.off()


#DENITRIFICATION
#AG
results = lm(denitrification ~ Moisture + Mean_AIR_TEMP..C.. + Soil_C_wt_. + Soil_N_wt_. + Soil_pH, data = ag_master)
summary(results)

jpeg("Ag_LM_Denitrification.jpg", width = 12, height = 10, units="in", res=350)
plot(results$model$denitrification, results$fitted.values, xlim=c(-1,1350), ylim=c(-1,1350), xlab=expression(Lab~Measured~Denitrification),
     ylab=expression(Predicted~Denitrification))
text(200,1000,paste("R²=0.73, p=1.282e-08"),cex=1)
dev.off()


#MSM
results = lm(denitrification ~ Moisture + Mean_AIR_TEMP..C.. + C.N_Ratio + Soil_pH + ugNH4.gDM, data = MSM_master)
summary(results)

jpeg("MSM_LM_Denitrification.jpg", width = 12, height = 10, units="in", res=350)
plot(results$model$denitrification, results$fitted.values, xlim=c(-30,1500), ylim=c(-30,1500), xlab=expression(Lab~Measured~Denitrification),
     ylab=expression(Predicted~Denitrification))
text(200,1000,paste("R²=0.19, p=0.04113"),cex=1)
dev.off()


#NMin
#AG
results = lm(Nmineralization ~ Moisture + Soil_C_wt_. + ugNH4.gDM + ugNO3.gDM , data = ag_master)
summary(results)

jpeg("Ag_LM_N-mineralization.jpg", width = 12, height = 10, units="in", res=350)
plot(results$model$Nmineralization, results$fitted.values, xlim=c(0,3), ylim=c(0,3), xlab=expression(Lab~Measured~Nmineralization),
     ylab=expression(Predicted~Nmineralization))
text(.8,2.5,paste("R²=0.70, p=1.858e-08"),cex=1)
dev.off()

#MSM
results = lm(Nmineralization ~ Mean_AIR_TEMP..C.. + Moisture + Soil_N_wt_. + Soil_C_wt_., data = MSM_master)
summary(results)

jpeg("MSM_LM_N-mineralization.jpg", width = 12, height = 10, units="in", res=350)
plot(results$model$Nmineralization, results$fitted.values, xlim=c(-.6,5), ylim=c(-.6,5), xlab=expression(Lab~Measured~Nmineralization),
     ylab=expression(Predicted~Nmineralization))
text(.4,3.3,paste("R²=0.47, p=7.677e-05"),cex=1)
dev.off()

