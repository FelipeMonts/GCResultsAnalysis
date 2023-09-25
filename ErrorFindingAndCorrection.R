##############################################################################################################
# 
# 
#          Program to ttrack and correct erros in the GC analysis results 
# 
#     
#          Felipe Montes 2023/09/22
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################

.libPaths("C:\\Users\\frm10\\AppData\\Local\\R\\win-library\\4.2")  ;


###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("openxlsx",  dependencies = T)

# install.packages("Rtools",  dependencies = T)

# install.packages("pdftools",  dependencies = T)

# install.packages("askpass",  dependencies = T)

# install.packages("cli",  dependencies = T)

# install.packages("utf8",  dependencies = T)

# install.packages("quantreg",  dependencies = T)

# install.packages("HMR",  dependencies = T)

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)

library(quantreg)

library(HMR)



###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd("D:\\Felipe\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis")



#### N2O Sampling year  #####

# Year = 2021

Year = 2022


PeakArea.results <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults" , Year ,".csv" ) , header = T)

str(PeakArea.results)

load(file = paste0("FluxDataAnalysisResults\\GCAnalysis" , Year , ".RData"))

ls()

str(GC.Data.NoSTD)

###### plotting the GC data ####

boxplot(N2O ~ Treatment.F * CoverCrop.F , data =  GC.Data.NoSTD, main = Year)

boxplot(CO2 ~ Treatment.F * CoverCrop.F , data =  GC.Data.NoSTD, main = Year)

###### plotting the GC calibrated data ####

boxplot(N2O.ppm ~ Treatment.F * CoverCrop.F   , data =  GC.Data.NoSTD, main = Year)

boxplot(CO2.ppm ~ Treatment.F * CoverCrop.F , data =  GC.Data.NoSTD, main = Year)

