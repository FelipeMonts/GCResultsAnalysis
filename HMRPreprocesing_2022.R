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
# Felipe Montes 2026
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

setwd(paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\",
             
             "CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\",
             
             "RCode\\GCResultsAnalysis\\FluxDataAnalysisResults"))

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



###############################################################################################################
#                 Read data frame with the curated data GC_data_Curated_20222026_11_20_16_46_47.csv
###############################################################################################################

Data.2022.1 <- read.csv( file = "GC_data_Curated_20222026_11_25_19_07_54.csv" , header = T) ;

str(Data.2022.1)

Data.2022.1$ID <- seq(1,nrow(Data.2022.1)) ;


# ###### Select only necessary columns: Sampling.Date , Treatment , BLOCK , CoverCrop , Sampling.Time
# 
# Field.Notes, Note , CO2.mg.m3, N2O.mg.m3

Data.2022 <- Data.2022.1[ , c( "ID" , "Sampling.Date" , "BLOCK" , "CoverCrop" ,"Treatment" , "Sampling.Time" , 
                                 
                                "Field.Notes", "Note.Field" , "CO2.mg.m3" ,"N2O.mg.m3" )] ;


str(Data.2022)
###############################################################################################################
#                           Data with field notes 
###############################################################################################################

Notes.2022 <- Data.2022[Data.2022$Field.Notes == "YES" , ] ;

Notes.2022 <- Notes.2022[order(Notes.2022$Sampling.Date , Notes.2022$BLOCK , Notes.2022$CoverCrop , 
                 
                                   Notes.2022$Treatment , Notes.2022$Sampling.Time) ,]

unique(Notes.2022$Sampling.Date)

# ###############################################################################################################
# 
# [1] "2022-06-09" "2022-06-15" "2022-06-23" "2022-07-07" "2022-07-19" "2022-08-02" "2022-08-19"
# [8] "2022-09-01" "2022-09-10" "2022-09-27" "2022-10-05" 
# ###############################################################################################################

Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 3 & $CoverCrop == "Clover" & Treatment == "A"
###### Sampling.Time == 30
######  Leaking and not well evacuated T30

Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 3 &  
               
            Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "A" ,]




plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                        
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
                        
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A"  , "CO2.mg.m3"],
      
      main = "CO2" ,
  
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A"  , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


##### Remove T 30 #####

###### Initialize data frame with points to be removed #####

Data.Remove.2022 <- Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                                
                                Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A"   &
                                  
                                  Data.2022$Sampling.Time == 30,] ; 





Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 3 & $CoverCrop == "Clover" & Treatment == "B"
###### Sampling.Time == 30 and 45
###### Leaking and not well evacuated T30 , Leaking and not well evacuated T45


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 3 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "B" &
             
             Notes.2022$Sampling.Time == 30 ,]


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 3 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "B" &
             
             Notes.2022$Sampling.Time == 45 ,]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "B"  , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "B"  , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;



######## All data seems right . Leave and remove note #########


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
            
            Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "B" &
            
            Data.2022$Sampling.Time == 30, c( "Field.Notes" , "Note.Field")] <- c("NO" , NA) ;

Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
            
            Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "B" &
            
            Data.2022$Sampling.Time == 45, c( "Field.Notes" , "Note.Field")] <- c("NO" , NA) ;




Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 3 & $CoverCrop == "Clover" & Treatment == "C"
###### Sampling.Time == 45
###### Leaking and not well evacuated T45


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 3 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "C" &
             
             Notes.2022$Sampling.Time == 45 ,]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C"  , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;

plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C"  , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


###### Remove T45 ########


Data.Remove.2022.T <- rbind(Data.Remove.2022 ,
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                                        
                                        Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" &
                                        
                                        Data.2022$Sampling.Time == 45, ]) ;
                            
                            
  
  
Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022




Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 3 & $CoverCrop == "Trit" & Treatment == "A"
###### Sampling.Time ==  all T
###### Leakage in all Ts

Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 3 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "A" , ] 


             
plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A"   , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A"   , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;



######## All data seems right . Leave and remove note #########


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" ,
          
          c("Field.Notes" , "Note.Field") ] <- c( rep("NO" , 4) , rep( NA , 4)) ;


Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 3 & $CoverCrop == "Trit" & Treatment == "B"
###### Sampling.Time == All T
###### Leakage in all Ts 


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 3 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "B" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B"   , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B"   , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


######## All data seems OK, Not perfect, but it has clearly a trend . Leave and remove note #########


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 3 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , 
          
          c( "Field.Notes" , "Note.Field") ] <- c( rep("NO" , 4) , rep( NA , 4)) ; 




Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "A"
###### Sampling.Time == 0 and 15
###### In one of these T-15 is T0 and T0 is T15 ?

Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "A" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A"  , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A"  , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;




####### Data seems fine . Keep and remove note  ####


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" & 
            
            Data.2022$Sampling.Time == 0 , c( "Field.Notes" , "Note.Field") ] <- c( "NO" , NA ) ;


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" & 
            
            Data.2022$Sampling.Time == 15 , c( "Field.Notes" , "Note.Field") ] <- c( "NO" , NA ) ; 

          



Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "B"
###### Sampling.Time == 0 and 15
###### In one of these T-15 is T0 and T0 is T15 ?


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "B" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B"  , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B"  , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;




####### Data seems fine . Keep and remove note  ####


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" & 
            
            Data.2022$Sampling.Time == 0 , c( "Field.Notes" , "Note.Field") ] <- c( "NO" , NA ) ;


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" & 
            
            Data.2022$Sampling.Time == 15 , c( "Field.Notes" , "Note.Field") ] <- c( "NO" , NA ) ;



Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "C"
###### Sampling.Time == T0 and T15
###### In one of these T-15 is T0 and T0 is T15 ?


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "C" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C"  , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C"  , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


######## T15 is interchaged with T0. Change T0 to T15 and T15 to T0 ########


T15 <- Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                   
                   Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" &
                   
                   Data.2022$Sampling.Time == 0 , c("CO2.mg.m3" , "N2O.mg.m3") ] ;

T0 <- Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                   
                   Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" &
                   
                   Data.2022$Sampling.Time == 15 , c("CO2.mg.m3" , "N2O.mg.m3") ] ;


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" &
            
            Data.2022$Sampling.Time == 15 , c("CO2.mg.m3" , "N2O.mg.m3") ] <- T15 ;


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" &
            
            Data.2022$Sampling.Time == 0 , c("CO2.mg.m3" , "N2O.mg.m3") ] <- T0 ;

rm(T0 , T15)




Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 4 & $CoverCrop == "Trit" & Treatment == "A"
###### Sampling.Time == all
###### Vials not well evacuated

Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "A" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


######## All data seems OK. Leave and remove note #########


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "A" ,
          
          c("Field.Notes" , "Note.Field") ] <- c(rep("NO" , 4) , rep(NA , 4)) ;



Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 4 & $CoverCrop == "Trit" & Treatment == "B"
###### Sampling.Time == all
###### Vials not well evacuated


Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "B" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;




######## All data seems OK. Leave and remove note #########


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" ,
          
          c("Field.Notes" , "Note.Field") ] <- c(rep("NO" , 4) , rep(NA , 4)) ;





Notes.2022[ Notes.2022$Sampling.Date == "2022-06-09" ,]

###### Sampling.Date == "2022-06-09" & BLOCK == 4 & $CoverCrop == "Trit" & Treatment == "C"
###### Sampling.Time == all
###### Vials not well evacuated

Notes.2022[Notes.2022$Sampling.Date == "2022-06-09" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "C" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;




######## All data seems OK,  Leave and remove note #########


Data.2022[Data.2022$Sampling.Date == "2022-06-09" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" ,
          
          c("Field.Notes" , "Note.Field") ] <- c(rep("NO" , 4) , rep(NA , 4)) ;





unique(Notes.2022$Sampling.Date)

Notes.2022[ Notes.2022$Sampling.Date == "2022-06-15" ,]

###### Sampling.Date == "2022-06-15" & BLOCK == 4 & $CoverCrop == "Clover" & Treatment == "C"
###### Sampling.Time == All times 
###### Problem with Vials


Notes.2022[Notes.2022$Sampling.Date == "2022-06-15" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "C" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


Serie.1 <- Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" &
            
            (Data.2022$ID == 314 | Data.2022$ID == 315 | Data.2022$ID == 316 | Data.2022$ID == 317) ,] ;


plot( x = Serie.1$Sampling.Time,
      
      
      y = Serie.1$CO2.mg.m3,
      
      main = "CO2" ,
      
      col = "blue") ;



plot( x = Serie.1$Sampling.Time,
      
      
      y = Serie.1$N2O.mg.m3,
      
      main = "N2O" ,
      
      col = "blue") ;




Serie.2 <- Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
                       
                       Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" &
                       
                       (Data.2022$ID == 714 | Data.2022$ID == 715 | Data.2022$ID == 716 | Data.2022$ID == 717) ,] ;


plot( x = Serie.2$Sampling.Time,
      
      
      y = Serie.2$CO2.mg.m3,
      
      main = "CO2" ,
      
      col = "blue") ;



plot( x = Serie.2$Sampling.Time,
      
      
      y = Serie.2$N2O.mg.m3,
      
      main = "N2O" ,
      
      col = "blue") ;


rm(Serie.1 , Serie.2)



######## Data is wrong. It is duplicated and each duplicated is wrong too. Remove all data #########


Data.Remove.2022.T <- rbind(Data.Remove.2022 ,
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-06-15" & Data.2022$BLOCK == 4 &
                                        
                                        Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C", ]) ;




Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022






unique(Notes.2022$Sampling.Date)

Notes.2022[ Notes.2022$Sampling.Date == "2022-06-23" ,]

###### Sampling.Date == "2022-06-23" & BLOCK == 2 & $CoverCrop == "Trit" & Treatment == "C"
###### Sampling.Time == 30 
###### T30 Syringe broke and did not open


Notes.2022[Notes.2022$Sampling.Date == "2022-06-23" & Notes.2022$BLOCK == 2 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "C" &
             
             Notes.2022$Sampling.Time == 30 ,]




plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-23" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-23" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;




plot( x = Data.2022[Data.2022$Sampling.Date == "2022-06-23" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-06-23" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;



############## T 30 is wrong, delete data point ############


Data.Remove.2022.T <- rbind(Data.Remove.2022 ,
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-06-23" & Data.2022$BLOCK == 2 &
                                        
                                        Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" & 
                                        
                                        Data.2022$Sampling.Time == 30 , ] ) ;



Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022


browser()


unique(Notes.2022$Sampling.Date)

Notes.2022[ Notes.2022$Sampling.Date == "2022-07-07" ,]

###### Sampling.Date == "2022-07-07" & BLOCK == 2 & $CoverCrop == "Trit" & Treatment == "B"
###### Sampling.Time == 15 and 45 
###### T15 is T45 and T45 is T15

Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 2 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "B" &
             
             Notes.2022$Sampling.Time == 15 ,]

Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 2 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "B" &
             
             Notes.2022$Sampling.Time == 45 ,]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


###### It seems that it is T15 exchanged fro T30 and T30 for T15 instead of T15 is T45 and T45 is T15
###### Change T15 to T30 and T30 to T15



T15 <- Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
                   
                   Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" &
                   
                   Data.2022$Sampling.Time == 30 , c("CO2.mg.m3" , "N2O.mg.m3") ] ;


T30 <- Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
                   
                   Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" &
                   
                   Data.2022$Sampling.Time == 15 , c("CO2.mg.m3" , "N2O.mg.m3") ] ;


Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" &
            
            Data.2022$Sampling.Time == 15 , c("CO2.mg.m3" , "N2O.mg.m3") ] <- T15 ;


Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 2 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" &
            
            Data.2022$Sampling.Time == 30 , c("CO2.mg.m3" , "N2O.mg.m3") ] <- T30 ;


rm(T15, T30)



browser()

Notes.2022[ Notes.2022$Sampling.Date == "2022-07-07" ,]

###### Sampling.Date == "2022-07-07" & BLOCK == 3 and 4
###### B3 is B4 sampled the next day B4 is B3 


Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 3 ,] 

Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 ,] 


str(Data.2022$BLOCK)

B3.ID <- Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 3 , "ID" ] ;

B4.ID <- Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 , "ID" ] ;


####### B3 into B4 #####

Data.2022[Data.2022$ID %in% B3.ID ,  "BLOCK"] <- 4 ;

####### B4 into B3 #####

Data.2022[Data.2022$ID %in% B4.ID ,  "BLOCK"] <- 3 ;


rm(B3.ID , B4.ID )

Notes.2022[ Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 3 ,]


Notes.2022[ Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 ,]


###### Sampling.Date == "2022-07-07" & BLOCK == 3 & $CoverCrop == "3Spp" & Treatment == "A"
###### Sampling.Time == 15
###### B3 is B4 sampled the next day and T15 is no good. it is from B3.
###### The note is marked in block 4 but refers to block 3

Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "A" &
             
             Notes.2022$Sampling.Time == 15 ,]


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;




############T15 is no good. it is from B3 before B3 changed to B4. Remove point it is not OK #####


Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                                        
                                        Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" &
                                        
                                        Data.2022$Sampling.Time == 15 , ]) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022




Notes.2022[ Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 ,]


###### Sampling.Date == "2022-07-07" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "B"
###### Sampling.Time == 15
###### B4 is B34 sampled the next day and T15 from B3 is on T15 from B4 
###### This is a very special case that is easy to get confused with !!!!! ####


Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "B" &
             
             Notes.2022$Sampling.Time == 15 ,]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


####### It seems that T45 is bad. Remove T45 ###########

Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 3 &
                                        
                                        Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" &
                                        
                                        Data.2022$Sampling.Time == 45 , ]) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022


Notes.2022[ Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 ,]


###### Sampling.Date == "2022-07-07" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "B"
###### Sampling.Time == 15
###### B4 is B34 sampled the next day and T15 from B3 is on T15 from B4




plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;



####### It seems that T15 is bad. Remove T15 ###########

Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                                        
                                        Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "B" &
                                        
                                        Data.2022$Sampling.Time == 15 , ]) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022




Notes.2022[Notes.2022$Sampling.Date == "2022-07-07" & Notes.2022$BLOCK == 4 ,]


###### Sampling.Date == "2022-07-07" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "C"
###### Sampling.Time == 15
######  B4 is B34 sampled the next day and T15 from C has mixed sample


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


####### It seems that T45 is bad. Remove T45 ###########

Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-07-07" & Data.2022$BLOCK == 4 &
                                        
                                        Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" &
                                        
                                        Data.2022$Sampling.Time == 45 , ]) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022



unique(Notes.2022$Sampling.Date)

Notes.2022[ Notes.2022$Sampling.Date == "2022-07-19" ,]

###### Sampling.Date == "2022-07-19" & BLOCK == 4 & $CoverCrop == "3Spp" & Treatment == "C"
###### Sampling.Time == all 
###### Amanda and Sarah were helping. Their note is 'Did not work'


Notes.2022[Notes.2022$Sampling.Date == "2022-07-19" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "C" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


########## Data is fine, Keep and Remove note ################


Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , 
          
          c( "Field.Notes" , "Note.Field")] <- c(rep( "NO" , 4) , rep( NA , 4)) ;






Notes.2022[ Notes.2022$Sampling.Date == "2022-07-19" ,]

###### Sampling.Date == "2022-07-19" & BLOCK == 4 & $CoverCrop == "Clover" & Treatment == "A"
###### Sampling.Time == 15 
###### T15 was not vacuum 

Notes.2022[Notes.2022$Sampling.Date == "2022-07-19" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "A" &
             
             Notes.2022$Sampling.Time == 15 , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


############## Data seems fine. Keep and remove the note #########


Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
            
            Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "A" &
            
            Data.2022$Sampling.Time == 15 , c( "Field.Notes" , "Note.Field") ] <- c( "NO" , NA) ;







Notes.2022[ Notes.2022$Sampling.Date == "2022-07-19" ,]

###### Sampling.Date == "2022-07-19" & BLOCK == 4 & $CoverCrop == "Clover" & Treatment == "C"
###### Sampling.Time == T30 
###### T30 was not vacuum

Notes.2022[Notes.2022$Sampling.Date == "2022-07-19" & Notes.2022$BLOCK == 4 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "C" &
             
             Notes.2022$Sampling.Time == 30 , ]


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C", "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;

plot( x = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C", "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;



############## Data T30 is bad. Remove point #########

Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-07-19" & Data.2022$BLOCK == 4 &
                                        
                                        Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" &
                                        
                                        Data.2022$Sampling.Time == 30 , ]) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022






unique(Notes.2022$Sampling.Date)

Notes.2022[ Notes.2022$Sampling.Date == "2022-08-02" ,]

###### Sampling.Date == "2022-08-02" & BLOCK == 1 & $CoverCrop == "3Spp" & Treatment == A and C
###### Sampling.Time == all
###### A is C and C is A

Notes.2022[Notes.2022$Sampling.Date == "2022-08-02" & Notes.2022$BLOCK == 1 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "A" , ]

Notes.2022[Notes.2022$Sampling.Date == "2022-08-02" & Notes.2022$BLOCK == 1 &  
             
             Notes.2022$CoverCrop == "3Spp" & Notes.2022$Treatment == "C" , ]
             
             

############## Change treatment A to C and C to A  #############


BA.ID <- Data.2022[Data.2022$Sampling.Date == "2022-08-02" & Data.2022$BLOCK == 1 &
                   
                   Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "A" , "ID" ] ;


BC.ID <- Data.2022[Data.2022$Sampling.Date == "2022-08-02" & Data.2022$BLOCK == 1 &
                     
                     Data.2022$CoverCrop == "3Spp" & Data.2022$Treatment == "C" , "ID" ] ;

Data.2022[Data.2022$ID %in% BA.ID , "Treatment"] <- "C" ;


Data.2022[Data.2022$ID %in% BC.ID , "Treatment"] <- "A" ;

rm(BA.ID , BC.ID)




unique(Notes.2022$Sampling.Date)

Notes.2022[Notes.2022$Sampling.Date == "2022-08-19" ,]

###### Sampling.Date == "2022-08-19" & BLOCK == 2 & $CoverCrop == "Trit" & Treatment == "B"
###### Sampling.Time == 45 
###### Soil wet only in the first 1 to 2 inches in all blocks - 
###### Sampled Day Before 08/18/22 - T45 needle failed. Sample was Lost

Notes.2022[Notes.2022$Sampling.Date == "2022-08-19" & Notes.2022$BLOCK == 2 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "B" &
             
             Notes.2022$Sampling.Time == 45 , ] 



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-08-19" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-08-19" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-08-19" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-08-19" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


##########  T45 is bad, Remove ########

Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-08-19" & Data.2022$BLOCK == 2 &
                                        
                                        Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "B" &
                                        
                                        Data.2022$Sampling.Time == 45 , ]) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022







unique(Notes.2022$Sampling.Date)

Notes.2022[Notes.2022$Sampling.Date == "2022-09-01" ,]

###### Sampling.Date == "2022-09-01" & BLOCK == 2 and 4 
###### Sampled next Day


##### Leave without change #####





unique(Notes.2022$Sampling.Date)

Notes.2022[Notes.2022$Sampling.Date == "2022-09-10" ,]

###### Sampling.Date == "2022-09-10" & BLOCK == 2 & $CoverCrop == "Clover" & Treatment == "C"
###### Sampling.Time == all
###### Notice leak between the frame and the chamber


Notes.2022[Notes.2022$Sampling.Date == "2022-09-10" & Notes.2022$BLOCK == 2 &  
             
             Notes.2022$CoverCrop == "Clover" & Notes.2022$Treatment == "C" , ]



plot( x = Data.2022[Data.2022$Sampling.Date == "2022-09-10" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-09-10" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-09-10" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-09-10" & Data.2022$BLOCK == 2 &
                      
                      Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


####### Data is very bad. Remove data ########


Data.Remove.2022.T <- rbind(Data.Remove.2022 , 
                            
                            Data.2022[Data.2022$Sampling.Date == "2022-09-10" & Data.2022$BLOCK == 2 &
                                        
                                        Data.2022$CoverCrop == "Clover" & Data.2022$Treatment == "C" , ] ) ;


Data.Remove.2022 <- Data.Remove.2022.T ;

rm(Data.Remove.2022.T) ;

Data.Remove.2022




unique(Notes.2022$Sampling.Date)

Notes.2022[Notes.2022$Sampling.Date == "2022-09-27" ,]

tail(Notes.2022[Notes.2022$Sampling.Date == "2022-09-27" ,] , 30)

###### Sampling.Date == "2022-09-27" & BLOCK == 3 & $CoverCrop == "Trit" & Treatment == "C"
###### Sampling.Time == 30 
###### T2 Solenoid is Failing 

Notes.2022[Notes.2022$Sampling.Date == "2022-09-27" & Notes.2022$BLOCK == 3 &  
             
             Notes.2022$CoverCrop == "Trit" & Notes.2022$Treatment == "C" , ]


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-09-27" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-09-27" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "CO2.mg.m3"],
      
      main = "CO2" ,
      
      col = "blue") ;


plot( x = Data.2022[Data.2022$Sampling.Date == "2022-09-27" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "Sampling.Time"],
      
      
      y = Data.2022[Data.2022$Sampling.Date == "2022-09-27" & Data.2022$BLOCK == 3 &
                      
                      Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;


######## Data is Fine, Leave and remove note #############


Data.2022[Data.2022$Sampling.Date == "2022-09-27" & Data.2022$BLOCK == 3 &
            
            Data.2022$CoverCrop == "Trit" & Data.2022$Treatment == "C" &
            
            Data.2022$Sampling.Time == 30 , 
          
          c( "Field.Notes" , "Note.Field") ] <- c( "NO" , NA) ;
  
  


unique(Notes.2022$Sampling.Date)

Notes.2022[Notes.2022$Sampling.Date == "2022-10-05" ,]

###### Sampling.Date == "2022-10-05" & BLOCK == 2 and 4
###### Sampling.Time == Sampled the next day

########## Leave as is #######


browser()

##############################################################################################################
#                           Check Notes and Removals
###############################################################################################################

Data.Remove.2022

HMR.Data.2022 <- Data.2022[!Data.2022$ID %in% Data.Remove.2022$ID ,] ;


HMR.Data.2022[which(HMR.Data.2022$Field.Notes == "YES"),] 

str(HMR.Data.2022)



###############################################################################################################
#                           Data with GC notes 
###############################################################################################################
###### Select only necessary columns: Sampling.Date , Treatment , BLOCK , CoverCrop , Sampling.Time
# 
# GC.Notes, Note.GC , CO2.mg.m3, N2O.mg.m3  

str(Data.2022.1)

Data.2022.GC.1 <- Data.2022.1[ , c( "ID" , "Sampling.Date" , "BLOCK" , "CoverCrop" ,"Treatment" , "Sampling.Time" , 
                               
                               "GC.Notes", "Note.GC" , "CO2.mg.m3" ,"N2O.mg.m3" )] ;

str(Data.2022.GC.1)



###############################################################################################################
#                           Remove data that was already removed using field notes
###############################################################################################################

Data.Remove.2022

Data.2022.GC <- Data.2022.GC.1[!Data.2022.GC.1$ID %in% Data.Remove.2022$ID ,] ;


str(Data.2022.GC)


###############################################################################################################
#                          get data with GC notes 
###############################################################################################################

Data.2022.GC.Notes <- Data.2022.GC[Data.2022.GC$GC.Notes == "YES" , ] ;

Data.2022.GC.Notes <- Data.2022.GC.Notes[order(Data.2022.GC.Notes$Sampling.Date , Data.2022.GC.Notes$BLOCK ,
                                               
                                               Data.2022.GC.Notes$CoverCrop , Data.2022.GC.Notes$Treatment , 
                                               
                                               Data.2022.GC.Notes$Sampling.Time) ,]

unique(Notes.2022$Sampling.Date)


