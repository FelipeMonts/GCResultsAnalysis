###############################################################################################################
#
#  Calculation of flux mass rates based on concetration(volume) flux rates
# 
#   Felipe Montes 2023/09/04
#
#############################################################################################################


###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("HMR",  dependencies = T)



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################


###############################################################################################################

# readClipboard() 


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd(paste0("D:\\Felipe\\Current_Projects\\CCC Based Experiments\\" ,
             
             "StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\"))

getwd()


###############################################################################################################
#                           load the working data from Concentration.Flux.Data analysis 
###############################################################################################################

Concentration.Flux.Data <- read.csv( file = "Concentration.Flux.Data.csv" ) ;

str(Concentration.Flux.Data)


###############################################################################################################
#           
#                             CONSTANTS FOR MASS CALCULATIONS
# 
# 
#    Universal Gas Constant R = 8.31446261815324   	m3⋅Pa⋅K−1⋅mol−1
#    
#    Atmospheric Pressure Patm = 101325 Pa
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
   

Concentration.Flux.Data$Temp.K <- 293.15 ;

Concentration.Flux.Data$F.mol.min <- Concentration.Flux.Data$f0 * Patm / (R * Concentration.Flux.Data$Temp.K *1e6) ;

Concentration.Flux.Data$F.KgElement.Ha.day <- Concentration.Flux.Data$F.mol.min * 44.0095 * 1440 * 10000 / 1000 ;

Concentration.Flux.Data$F.KgSubstance.Ha.day <- Concentration.Flux.Data$F.mol.min * 12.0107 * 1440 * 10000 / 1000 ;


