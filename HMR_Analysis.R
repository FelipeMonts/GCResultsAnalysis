###############################################################################################################
#
#  Calculation of flux rates based on the paper:
# 
# Pedersen, A. R., S. O. Petersen, and K. Schelde. “A Comprehensive Approach to Soil-Atmosphere Trace-Gas Flux 
# 
# Estimation with Static Chambers.” European Journal of Soil Science 61, no. 6 (2010): 888–902. 
# 
# https://doi.org/10.1111/j.1365-2389.2010.01291.x.
# 
# 
# and the r package : 

# Pedersen, Asger R. “HMR: Flux Estimation with Static Chamber Data,” May 20, 2020. https://CRAN.R-project.org/package=HMR.
#
# Version 1.0.5  	2025-12-17
#
# Updated on 2023 by :
#
#Pullens, Johannes Wilhelmus Maria, Diego Abalos, Søren O. Petersen, and Asger R. Pedersen. 
#
# “Identifying Criteria for Greenhouse Gas Flux Estimation with Automatic and Manual Chambers: 

# A Case Study for N2O.” European Journal of Soil Science 74, no. 1 (2023): e13340. https://doi.org/10.1111/ejss.13340.

# 
# Felipe Montes 2023
###############################################################################################################

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("HMR",  dependencies = T)

# install.packages("nlstools",  dependencies = T)

# install.packages("nls2",  dependencies = T)

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################


library(HMR)

library(nlstools)

library(nls2)

library(lattice)

###############################################################################################################

# readClipboard() 


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults")

getwd()

###############################################################################################################
#                           Select the N2O Sampling year
###############################################################################################################

# Year = 2021

# Year = 2022

####### GAS  HMR analysis ############

# Gas = "CO2"
# 
# Gas = "N2O"
# 
# Gas = "CH4"


###############################################################################################################
#                           load the working data from GCANalysis 
###############################################################################################################


load(file = paste0("GCAnalysis" , Year , ".RData"))


###############################################################################################################
#                           Flux Chamber dimensions for the calculations
###############################################################################################################


###############################################################################################################
#                        HMR data and flux physical units
###############################################################################################################
#
# HMR data and flux physical units:
# For maximal flexibility, HMR has no requirements for the physical units of input data. 
# The chosen units do, however, determine the unit of the estimated flux, 
# which has the physical unit of (V C)/(At), where t and C denote, respectively, time and concentration. 
# Some examples:  
#   V [L], A [m2], t [h], C [μg/L] ⇒ f0 [μg/m2/h] 
#   V [L], A [m2], t [min], C [μL/L] ⇒ f0 [μL/m2/min] 
#   V [m2], A [km2], t [s], C [kg/m3] ⇒ f0 [kg/km2/s]
#
####  Using V [L], A [m2], t [min], C [μL/L] ⇒ f0 [μL/m2/min] #####
#


###############################################################################################################
#
#               Reference data taken from Allison Kohele's Calculations Excel Files
#
###############################################################################################################



Chamber.Dimensions<-data.frame(DIMENSION=c("Length", "Width" , "Height", "Volume" , "Surface.Area"),UNITS = c(rep("cm",3), "1000 x cm3 or L" , "m2"), VALUE=c(52.705, 32.385, 10.16, 9999, 9999));

Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Volume", c("VALUE")] <- Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3]*Chamber.Dimensions[3,3] / 1000 ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Surface.Area", c("VALUE")] <- Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3] / 10000;

Molar.Mass<-data.frame(GAS=c("CH4" , "CO2" , "N2O"), UNITS=c("g/mol"), VALUE=c(16.04, 44.01, 44.013));

Gas.Law<-data.frame(UNITS=c("L-atm/Mol-K", "J/K-Mol", "m3-Pa/K-Mol", "Kg-m2-s2/K-Mol", "m3-atm/K-Mol"), VALUE=c(0.08205736, 8.314462,8.314462, 8.314462, 8.205736e-5 ))  ;





###############################################################################################################
#           
#                             CONSTANTS FOR MASS CALCULATIONS
#    
#             Universal Gas Constant
#
#             https://en.wikipedia.org/wiki/Gas_constant
# 
#             R = 8.31446261815324   	m3⋅Pa⋅K−1⋅mol−1
#
#             R = 0.082057366080960   	L⋅atm⋅K−1⋅mol
#
#      
#    Atmospheric Pressure Patm = 101325 Pa  ,  1 atm
#    
#    Degrees Kelvin  K = 273.15 + T °C
#    
#    Day = 60 * 24 min = 1440 min
#    
#    Year = 1440 min * 365 days = 525600 min
#    
#    Hectare Ha = 100m * 100 m = 1000 m2 
#    
#    Fluxm kg X · ha-1 day-1 
# 
#    CO2 = 44.0095 g mol-1  https://webbook.nist.gov/cgi/cbook.cgi?ID=C124389&Units=SI
#    
#    N2O = 44.0128 g mol-1 https://webbook.nist.gov/cgi/cbook.cgi?ID=C10024972&Units=SI
#    
#    CH4 = 16.0425 g mol-1 https://webbook.nist.gov/cgi/cbook.cgi?ID=C74828&Units=SI
#    
#    C = 12.0107  g mol-1  https://webbook.nist.gov/cgi/cbook.cgi?ID=C7440440&Units=SI
#    
#    O = 15.9994 g mol-1   https://webbook.nist.gov/cgi/cbook.cgi?Formula=O&NoIon=on&Units=SI
#    
#    N = 14.0067 g mol-1 https://webbook.nist.gov/cgi/cbook.cgi?Formula=N&NoIon=on&Units=SI
#    
#    H = 1.00794 g mol-1 https://webbook.nist.gov/cgi/cbook.cgi?ID=C12385136&Units=SI
# 
# 
############################################################################################################### 

R = 0.082057366080960

Patm = 1  

Concentration.Flux.Data$Temp.K <- 293.15 ;




###############################################################################################################
#                           Format concentration data for HMR
###############################################################################################################



Gas.Series <- data.frame(GC.Data.NoSTD[, c( "Series") ], Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume" , c("VALUE")],
                         
                         Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area" , c("VALUE")], 
                         
                         GC.Data.NoSTD[, c( "Sampling.Time" , paste0(Gas,".ppm")) ]) ;

names(Gas.Series) <- c("Series" , "Volume.L" , "Surface.Area.m2" , "Sampling.Time" , paste0(Gas,".ppm") );

str(Gas.Series)

head(Gas.Series)


str(Gas.Series)

head(Gas.Series)   ### Need to order the series 

Gas.Series.HMR <- Gas.Series[order(Gas.Series$Series,Gas.Series$"Sampling.Time" ),]

head(Gas.Series.HMR)


write.table(x = Gas.Series.HMR , sep = ";", dec = "." ,file = paste0(Gas,"_" , Year , ".Series.csv"), row.names = F)  ;

######### Variance of the ambient concentration measurements #############

str(GC.Data.NoSTD)

GC.Data.NoSTD$Sampling.Day.F <- as.factor(GC.Data.NoSTD$Sampling.Day );

sigma02 <- var( GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, c(paste0(Gas,".ppm"))])

bwplot(as.formula(paste0(Gas,".ppm", " ~ ", "Sampling.Day.F" )) , 
       
       data = GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, ] )

# The variance in sampling date 20220623 is an outlier and should be not included #

bwplot(as.formula(paste0(" ~ ", Gas,".ppm")),  data = GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, ], horizontal = T )


Gas.HRM.Results <- HMR(filename = paste0(Gas,"_" , Year , ".Series.csv") , sep = ";" , dec = "." , SatPct = NA,
                       SatTimeMin = NA, pfvar = sigma02, pfalpha = 0.05,  LR.always = T , FollowHMR = T,
                       IfNoValidHMR = 'LR', IfNoFlux = 'No flux',  IfNoSignal = 'No flux') ;

str(Gas.HRM.Results)


###### Transform flux data (F0) from text to number

Gas.HRM.Results$Flux <- as.numeric(Gas.HRM.Results$f0)   ;

######## Results that were processed  #########

str(Gas.HRM.Results[!Gas.HRM.Results$Warning == "Data error" ,])



######## Results that have errors  #########

str(Gas.HRM.Results[Gas.HRM.Results$Warning == "Data error" ,])




###############################################################################################################
#
#  Add the fluxes to complete the database for analysis
#    
###############################################################################################################
str(GC.Data.NoSTD)

str(Gas.HRM.Results)

GC.Data.NoSTD$Series

Flux.Data <- merge(GC.Data.NoSTD, Gas.HRM.Results, by = "Series") ;

str(Flux.Data)

######## Test for the merge operation ######

str(Flux.Data[Flux.Data$Warning== "Data error" ,])

Flux.Data[Flux.Data$Warning == "Data error" , c("Series")]

str(Gas.HRM.Results)

unique(Flux.Data[Flux.Data$Warning == "Data error" , c("Series")])

unique(Flux.Data[!Flux.Data$Warning == "Data error" , c("Series")])

Gas.HRM.Results[Gas.HRM.Results$Warning == "Data error" , c("Series")]

unique(Gas.HRM.Results[Gas.HRM.Results$Warning == "Data error" , c("Series")])

unique(Gas.HRM.Results[!Gas.HRM.Results$Warning == "Data error" , c("Series")])

Flux.Data[Flux.Data$Series == "20220901_3_Trit_C" , c("f0")]

Gas.HRM.Results[Gas.HRM.Results$Series ==  "20220901_3_Trit_C" , c("f0")]


Gas.HRM.Results[is.na(Gas.HRM.Results$Series),]

Flux.Data[is.na(Flux.Data$Series),]


save.image(file = paste0(Gas,"_HMR_Analysis_" , Year, ".RData"))

