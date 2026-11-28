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
#                           load the working data from GCANalysis 
###############################################################################################################


# load(file = paste0("GCAnalysis" , Year , ".RData"))


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
#                 Read data frame with the curated data GC_data_Curated_20212026_11_19_16_42_12.csv
###############################################################################################################

Data.2021.1 <- read.csv( file = "GC_data_Curated_20212026_11_23_15_16_41.csv" , header = T) ;

str(Data.2021.1)

Data.2021.1$ID <- seq(1,nrow(Data.2021.1)) ;


# ###### Select only necessary columns: Sampling.Date , Treatment , BLOCK , CoverCrop , Sampling.Time
# 
# Field.Notes, Note , CO2.mg.m3, N2O.mg.m3

Data.2021.2 <- Data.2021.1[ , c( "ID" , "Sampling.Date" , "BLOCK" , "CoverCrop" ,"Treatment" , "Sampling.Time" , 
                                 
                                "Field.Notes", "Note" , "CO2.mg.m3" ,"N2O.mg.m3" )] ;



###############################################################################################################
#                           Data with field notes 
###############################################################################################################

Notes.2021.1 <- Data.2021.2[Data.2021.2$Field.Notes == "YES" , ] ;

Notes.2021.1 <- Notes.2021.1[order(Notes.2021.1$Sampling.Date , Notes.2021.1$BLOCK , Notes.2021.1$CoverCrop , 
                 
                                   Notes.2021.1$Treatment , Notes.2021.1$Sampling.Time) ,]

unique(Notes.2021.1$Sampling.Date)

# ###############################################################################################################
# 
# [1] "2021-06-01" "2021-06-04" "2021-06-21" "2021-06-23" "2021-06-29" "2021-07-02" "2021-07-07"
# [8] "2021-07-15" "2021-07-20" "2021-07-30" "2021-08-05" "2021-08-12" "2021-08-19" "2021-09-02"
# [15] "2021-09-17"
# 
# ###############################################################################################################


###### Sampling.Date == "2021-06-01" & BLOCK == 1 & $CoverCrop == "Clover" & Treatment == "D"
###### Pump hose loose, data questionable 

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01",]

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01" & Notes.2021.1$BLOCK == 1 &  
               
            Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "D" ,]

plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 1 &
                        
                      Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" , "Sampling.Time"],
      
                        
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 1 & 
                        
                      Data.2021.2$CoverCrop == "Clover" &  Data.2021.2$Treatment == "D" , "CO2.mg.m3"],
      
      main = "CO2" ,
  
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 1 &
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" , "Sampling.Time"],
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" &  Data.2021.2$Treatment == "D" , "N2O.mg.m3"],
      
      main = "N2O" ,
      
      col = "blue") ;

#### Leave with the note #####


###############################################################################################################


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01",]

###### Sampling.Date == "2021-06-01" & BLOCK == 2 & CoverCrop == "3Spp" & Treatment == "A"
###### Sampling.Time == 45
###### Pump off at T3


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01" & Notes.2021.1$BLOCK == 2 &  
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "A" ,]


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01", ]

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01" & Notes.2021.1$BLOCK == 2 &  
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "A" ,]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                      Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
                         
                         
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"],
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "N2O.mg.m3"],
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time  == 45 point ######


###### Initialize data frame with points to be removed #####

Data.Remove.2021 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                                  
                                  Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" &
                                  
                                  Data.2021.2$Sampling.Time == 45,] ; 


###############################################################################################################


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01",]

##### Sampling.Date == "2021-06-01" & "BLOCK" == 2 & CoverCrop == "Trit" & Treatment == "C" ,
##### Sampling.Time == 45
##### Needle clogged no sample pushed in the vial


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01" & Notes.2021.1$BLOCK == 2 &  
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" ,]




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "N2O.mg.m3"],
      
      main = "N2O",
      
      col = "blue") ;


#### Leave with remove note #####


Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" &
              
              Data.2021.2$Sampling.Time == 45, c("Field.Notes" , "Note") ] <- c( "NO" , NA) ;



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01",]

##### Sampling.Date == "2021-06-01" & "BLOCK" == 3 & CoverCrop == "Trit" & Treatment == "A"  &
##### Sampling.Time == 15
##### T15 Syringe plunger Failed

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01" & Notes.2021.1$BLOCK == 2 &  
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" ,]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"],
      
      main = "N2O",
      
      col = "blue") ;



#### remove Sampling.Time == 15 point ######

Data.Remove.2021.T <- rbind(Data.Remove.2021 , 
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 3 & 
                                                             
                                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" & 
                                  
                                        Data.2021.2$Sampling.Time == 15,] ) ; 


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;



###############################################################################################################


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01",]


##### Sampling.Date == "2021-06-01" & $BLOCK == 4 & CoverCrop == "Clover" & Treatment == "C"
##### Sampling.Time == 15
##### T30 Needle clogged

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-01" & Notes.2021.1$BLOCK == 4 &  
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" &
             
             Notes.2021.1$Sampling.Time == 30 ,]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 30 point ######

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-01" & Data.2021.2$BLOCK == 4 & 
                                          
                            Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
                              
                            Data.2021.2$Sampling.Time == 30 ,])


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;


###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-04",]

##### Sampling.Date == "2021-06-04" & $BLOCK == 1 
##### Block 1 fell of the car and was left in the field from Friday to Monday

##### Leave with a note #####

unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-21",]

##### Sampling.Date == "2021-06-21" & $BLOCK == 3 & & CoverCrop == "Trit" & Treatment == "C"
##### Sampling.Time == 30 
##### Syringe stock open when taking vial

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-21" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" & 
               
               Notes.2021.1$Sampling.Time == 30,]




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-21" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-21" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-21" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-21" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 30 point ######

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-21" & Data.2021.2$BLOCK == 3 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" & 
                                        
                                        Data.2021.2$Sampling.Time == 30 ,] )
                            
                            
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;



###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-23",]

##### Sampling.Date == "2021-06-23" & $BLOCK == 1 & & CoverCrop == "3Spp" & Treatment == "B"
##### Sampling.Time == 45 
#####  * T45 ?

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-23" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 45, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;

                            
                            
plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;    



##### Delete note, everything OK

Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 45, c("Field.Notes","Note" ) ] <- c("NO" , NA) ;


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-23",]

##### Sampling.Date == "2021-06-23" & $BLOCK == 1 & & CoverCrop == "Clover" & Treatment == "A"
##### Sampling.Time == 15 
#####  Sample not fully drawn

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-23" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "A" &
               
               Notes.2021.1$Sampling.Time == 15, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



##### Delete note, everything OK

Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" &
              
              Data.2021.2$Sampling.Time == 15,]


Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-23" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" &
              
              Data.2021.2$Sampling.Time == 15, c("Field.Notes","Note" ) ] <- c("NO" , NA) ;



###############################################################################################################


unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 1 & & CoverCrop == "Clover" & Treatment == "B"
##### Sampling.Time == 45 
##### Needle Broke

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 45, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### Delete note, everything OK

Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 45 , c("Field.Notes","Note" ) ] <- c("NO" , NA) ;



###############################################################################################################

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 1 & & CoverCrop == "Trit" & Treatment == "B"
##### Sampling.Time == 15 
##### T15 Broke

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 15, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , ]



######  The series looks all bad, Remove the whole series #######

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" ,] )


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 2 & & CoverCrop == "Clover" & Treatment == "B"
##### Sampling.Time == 15 
##### No T 15

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 15, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 15 #######


Data.Remove.2021.T <- rbind(Data.Remove.2021 , 
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" &
                                          
                                          Data.2021.2$Sampling.Time == 15 ,] )

Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 2 & & CoverCrop == "Trit" & Treatment == "A"
##### Sampling.Time == 45 
##### No T 415

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "A" &
               
               Notes.2021.1$Sampling.Time == 45, ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 45 #######

Data.Remove.2021.T <- rbind(Data.Remove.2021 , 
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 2 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" &
                                          
                                          Data.2021.2$Sampling.Time == 45 , ] ) ;
                                          
                            
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;


###############################################################################################################


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 3 & & CoverCrop == "Clover" & Treatment == "A"
##### Sampling.Time == 45
##### No T3 Syringe Broke

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "A" &
               
               Notes.2021.1$Sampling.Time == 45, ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A"  , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A"  , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 45 #######

Data.Remove.2021.T <- rbind(Data.Remove.2021 , 
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" &
                                          
                                          Data.2021.2$Sampling.Time == 45 ,]) 

Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;
                            
Data.Remove.2021                            



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 3 & & CoverCrop == "Clover" & Treatment == "C"
##### Sampling.Time == 30 
##### T2 Needle Broke

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" &
               
               Notes.2021.1$Sampling.Time == 30, ] 


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 30  #######

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 3 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
                                          
                                          Data.2021.2$Sampling.Time == 30 ,]) ;
                            
                            
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021                            



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29",]

##### Sampling.Date == "2021-06-29" & $BLOCK == 4 & & CoverCrop == "Trit" & Treatment == "C"
##### Sampling.Time == 30 ?
##### T45 no sample ?

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-06-29" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" &
               
               Notes.2021.1$Sampling.Time == 30, ] 



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### The whole series looks very bad. Remove the whole series 

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-06-29" & Data.2021.2$BLOCK == 4 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C",] )
                            
                            

Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021       


###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02",]

##### Sampling.Date == "2021-07-02" & $BLOCK == 1 & & CoverCrop == "Clover" & Treatment == "C"
##### Sampling.Time == 15 
##### T15 no Sample Syringe broke 

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" &
               
               Notes.2021.1$Sampling.Time == 15, ] 




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 15  #######

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
                                          
                                          Data.2021.2$Sampling.Time == 15 , ]) ;
                                          

Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021  


###############################################################################################################




Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02",]

##### Sampling.Date == "2021-07-02" & $BLOCK == 3 & & CoverCrop == "3Spp" & Treatment == "B"
##### Sampling.Time == 45 
##### T3 valve did not open or Syringe Stuck

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 45, ] 


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;

##### Everything is OK. Delete note and leave #####

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 45 , c("Field.Notes","Note" ) ] <- c("NO" , NA) ;


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02",]

##### Sampling.Date == "2021-07-02" & $BLOCK == 3 & & CoverCrop == "Trit" & Treatment == "A"
##### Sampling.Time == 45 
##### T45 was leaking

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "A" &
               
               Notes.2021.1$Sampling.Time == 45, ] 


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### The whole series looks very bad. Remove the whole series 

Data.Remove.2021.T <- rbind(Data.Remove.2021 ,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" ,]) ;
                            
                            
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 


###############################################################################################################


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02",]

##### Sampling.Date == "2021-07-02" & $BLOCK == 3 & & CoverCrop == "Trit" & Treatment == "B"
##### Sampling.Time == 30
##### T2 did not fully draw sample 


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-02" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 30, ] 


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B"  , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B"  , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 30  #######

Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-02" & Data.2021.2$BLOCK == 3 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
                                          
                                          Data.2021.2$Sampling.Time == 30 , ]) ;
                            
                            
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 



###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-07",]

##### Sampling.Date == "2021-07-07" & $BLOCK == 4 & & CoverCrop == "Clover" & Treatment == "B"
##### Sampling.Time == 45 
##### T3 Syringe Stuck

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-07" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "B" &
               
               Notes.2021.1$Sampling.Time == 45, ] 


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 45  #######

Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" &
                                          
                                          Data.2021.2$Sampling.Time == 45, ]) ;
                                          
                                          
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-07",]

##### Sampling.Date == "2021-07-07" & $BLOCK == 4 & & CoverCrop == "Clover" & Treatment == "C"
##### Sampling.Time == 30 and 45 
##### T45 is T30 and T30 is T45

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-07" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" &
               
               Notes.2021.1$Sampling.Time == 30, ] 

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-07" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" &
               
               Notes.2021.1$Sampling.Time == 45, ] 



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;

                            
                            
plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;                            
                            
                            
##### Everything is OK. Delete note and leave #####

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                               
                               Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" & 
                               
                               Data.2021.2$Sampling.Time == 30 , c("Field.Notes","Note" ) ] <- c("NO" , NA) ;

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-07" & Data.2021.2$BLOCK == 4 & 
                                     
                                     Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" & 
                                     
                                     Data.2021.2$Sampling.Time == 45 , c("Field.Notes","Note" ) ] <- c("NO" , NA) ;
                               
                               


###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-15",]

##### Sampling.Date == "2021-07-15" & $BLOCK == 1 & & CoverCrop == "3Spp" & Treatment == "B"
##### Sampling.Time ==  0 , 15 , 30 , 45 

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-15" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "B" , ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
     y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
                       
                       Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
     
     main = "CO2",
     
     col = "blue") ;

               

plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;   


###### change T30 for T15, remove T15 and then remove note ######

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 30 ,
            
              c( "CO2.mg.m3" , "N2O.mg.m3" )] <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & 
                                                                   
                                                                   Data.2021.2$BLOCK == 1 & 
                                                                   
                                                                   Data.2021.2$CoverCrop == "3Spp" &
                                                                   
                                                                   Data.2021.2$Treatment == "B" &
                                                                   
                                                                   Data.2021.2$Sampling.Time == 15 ,
                                                             
                                                             c( "CO2.mg.m3" , "N2O.mg.m3" )] ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" ,] 


      
Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" &
                                          
                                          Data.2021.2$Sampling.Time == 15,]) ; 


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 

#### remove note ####


Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" ,
            
            c("Field.Notes" , "Note") ] <- c( rep("NO" , 4) , rep(NA , 4) )  ;


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-15",]

##### Sampling.Date == "2021-07-15" & $BLOCK == 4 & & CoverCrop == "Clover" & Treatment == "B"
##### Sampling.Time == All
##### did not run at 11:57; It was ran again at 3:00 pm 

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-15" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "B" , ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



##### Everything is OK. Delete note and leave #####

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-15" & Data.2021.2$BLOCK == 4 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "B" , 
            
            c("Field.Notes","Note" ) ] <- c(rep("NO",4) , rep(NA,4) ) ;



###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-20",]

##### Sampling.Date == "2021-07-20" & $BLOCK == 4 & & CoverCrop == "Trit" & Treatment == "A"
##### Sampling.Time == 15
##### T15 plunger

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-20" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "A" &
               
               Notes.2021.1$Sampling.Time == 15,]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


#### remove Sampling.Time == 45  #######

Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "A" &
                                          
                                          Data.2021.2$Sampling.Time == 15,] ) ;
                                          
                                          
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-20",]

##### Sampling.Date == "2021-07-20" & $BLOCK == 4 & & CoverCrop == "Trit" & Treatment == "B"
##### Sampling.Time == 15 and 45
##### T15 is T45 

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-20" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "B" , ]
               
              
plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



###### change T15 for T45 ######

T45 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
  
                  Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
  
                  Data.2021.2$Sampling.Time == 15, c("CO2.mg.m3" , "N2O.mg.m3")] ;
  
T15 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                     
                     Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
                     
                     Data.2021.2$Sampling.Time == 45, c("CO2.mg.m3" , "N2O.mg.m3")]  ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
              
              Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 15, c("CO2.mg.m3" , "N2O.mg.m3")] <- T15 ;

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
              
              Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 45, c("CO2.mg.m3" , "N2O.mg.m3")] <- T45 ;

rm(T15, T45)

### Check

plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



##### Remove Note ######

Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
              
              Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 15, c("Field.Notes" , "Note")] <- c("NO" , NA) ;
  
  
Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-20" & Data.2021.2$BLOCK == 4 & 
                
                Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
                
                Data.2021.2$Sampling.Time == 45, c("Field.Notes" , "Note")] <- c("NO" , NA) ;
  
  
  
###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-30",]

##### Sampling.Date == "2021-07-30" & $BLOCK == 1 & & CoverCrop == "3Spp" & Treatment == "A"
##### Sampling.Time == 45
##### T45 Syringe did not open

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-07-30" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "A" &
               
               Notes.2021.1$Sampling.Time == 45, ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-30" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-30" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-30" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-30" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



#### remove Sampling.Time == 45  #######


Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-07-30" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" &
                                          
                                          Data.2021.2$Sampling.Time == 45 ,])


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 



###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05",]

##### Sampling.Date == "2021-08-05" & $BLOCK == 1 & & CoverCrop == "Clover" & Treatment == "C"
##### Sampling.Time == 15 
##### No T30 put T30 into T 15

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" &
               
               Notes.2021.1$Sampling.Time == 15, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


####### Change T15 to T30 and remove T15 #######

Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
            
             Data.2021.2$Sampling.Time == 30 , c("CO2.mg.m3" , "N2O.mg.m3")] <- 
  
Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                
                Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
                
                Data.2021.2$Sampling.Time == 15 , c("CO2.mg.m3" , "N2O.mg.m3")] ;



Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
                                          
                                          Data.2021.2$Sampling.Time == 15, ]) ;
                            
 

Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 


######## Remove Note ##################


Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
              
              Data.2021.2$Sampling.Time == 30 , c("Field.Notes" , "Note" )] <- c( "NO" , NA) ;



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05",]

##### Sampling.Date == "2021-08-05" & $BLOCK == 1 & & CoverCrop == "Clover" & Treatment == "D"
##### Sampling.Time == 15
##### No T 15

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "D" &
               
               Notes.2021.1$Sampling.Time == 15, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



####### Remove T15 #######


Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
                                          
                                          Data.2021.2$Sampling.Time == 15 , ] ) ;


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05",]

##### Sampling.Date == "2021-08-05" & $BLOCK == 2 & & CoverCrop == "3Spp" & Treatment == "A"
##### Sampling.Time == all
##### Vials not well evacuated

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "A", ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### The whole series is bad. Remove the whole series  #####


Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                                          
                                          Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "A" ,] )  ;
                            
                            
                            
Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05",]

##### Sampling.Date == "2021-08-05" & $BLOCK == 2 & & CoverCrop == "3Spp" & Treatment == "B"
##### Sampling.Time == T30 and T45
##### T30 and T45 are interchanged

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "B" & 
               
               Notes.2021.1$Sampling.Time == 30, ]

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-05" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "B" & 
               
               Notes.2021.1$Sampling.Time == 45, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### It seems that T15 and T30 are interchanged instead of T30 and T45 are interchanged
##### Switch T15 to T30 and T30 to T15 #####



T30 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                     
                     Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" & 
                     
                     Data.2021.2$Sampling.Time == 15, c("CO2.mg.m3" , "N2O.mg.m3")] ;

T15 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
                     
                     Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" & 
                     
                     Data.2021.2$Sampling.Time == 30, c("CO2.mg.m3" , "N2O.mg.m3")] ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" & 
              
              Data.2021.2$Sampling.Time == 30, c("CO2.mg.m3" , "N2O.mg.m3")] <- T30 ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" & 
              
              Data.2021.2$Sampling.Time == 15, c("CO2.mg.m3" , "N2O.mg.m3")] <- T15 ;


rm(T15)

###### Remove Note ######


Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" & 
              
              Data.2021.2$Sampling.Time == 30, c("Field.Notes" , "Note")] <- c( "NO" , NA) ;

Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-05" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" & 
              
              Data.2021.2$Sampling.Time == 45, c("Field.Notes" , "Note")] <- c( "NO" , NA) ; 




###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-12",]

##### Sampling.Date == "2021-08-12" & $BLOCK == 3 & & CoverCrop == "Trit" & Treatment == "B"
##### Sampling.Time == 30
##### No T 30, Syringe did not work

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-12" & Notes.2021.1$BLOCK == 3 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "B" & 
               
               Notes.2021.1$Sampling.Time == 30, ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



##### T30 seems fine, leave it as is . Remove Note ##################


Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 3 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "B" &
              
              Data.2021.2$Sampling.Time == 30 , c("Field.Notes" , "Note" )] <- c( "NO" , NA) ;




###############################################################################################################


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-12",]

##### Sampling.Date == "2021-08-12" & $BLOCK == 4 & & CoverCrop == "Trit" & Treatment == "C"
##### Sampling.Time == 15 
##### T 15 no Sample, Syringe did not work

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-12" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" & 
               
               Notes.2021.1$Sampling.Time == 15, ]




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C"  , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C"  , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



####### Remove T15 #######


Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-12" & Data.2021.2$BLOCK == 4 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" & 
                                          
                                          Data.2021.2$Sampling.Time == 15, ] ) ;


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021 


###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-19",]

##### Sampling.Date == "2021-08-19" & $BLOCK == 4 & & CoverCrop == "Clover" & Treatment == "A"
##### Sampling.Time == 30 
##### Syringe 30 in did not take the full 30 ml 


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-08-19" & Notes.2021.1$BLOCK == 4 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "A" & 
               
               Notes.2021.1$Sampling.Time == 30, ]




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-19" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-19" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-19" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-19" & Data.2021.2$BLOCK == 4 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



##### T30 seems fine, leave it as is. Remove note ##################


Data.2021.2[Data.2021.2$Sampling.Date == "2021-08-19" & Data.2021.2$BLOCK == 4 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" &
              
              Data.2021.2$Sampling.Time == 30 , c("Field.Notes" , "Note" )] <- c( "NO" , NA) ;



###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02",]

##### Sampling.Date == "2021-09-02" & $BLOCK == 1 & & CoverCrop == "Trit" & Treatment == "C"
##### Sampling.Time == 15 
##### T15 Syringe did not go



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" & 
               
               Notes.2021.1$Sampling.Time == 15, ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


####### Remove T15 #######



Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" & 
                                          
                                          Data.2021.2$Sampling.Time == 15 ,] )



Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02",]

##### Sampling.Date == "2021-09-02" & $BLOCK == 2 & & CoverCrop == "Clover" & Treatment == "A"
##### Sampling.Time == 15 
##### T15 only took 15 ml


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "A" & 
               
               Notes.2021.1$Sampling.Time == 15, ]




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



##### T15 seems fine, leave it as is. Remove note ##################


Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "A" &
              
              Data.2021.2$Sampling.Time == 15, c("Field.Notes" , "Note" )] <- c( "NO" , NA) ;


###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02",]

##### Sampling.Date == "2021-09-02" & $BLOCK == 2 & & CoverCrop == "Clover" & Treatment == "C"
##### Sampling.Time == 45
##### T 45 Syringe did not go

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "C" & 
               
               Notes.2021.1$Sampling.Time == 45, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;



####### Remove T45 #######



Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                                          
                                          Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "C" &
                                          
                                          Data.2021.2$Sampling.Time == 45,] ) ;
                            
                            

Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02",]

##### Sampling.Date == "2021-09-02" & $BLOCK == 2 & & CoverCrop == "Clover" & Treatment == "D"
##### Sampling.Time == 30 and 45
##### T30 and T45 are exchanged; T30 is T45 and T45 is T 30


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "D" & 
               
               Notes.2021.1$Sampling.Time == 30, ]

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-02" & Notes.2021.1$BLOCK == 2 & 
               
               Notes.2021.1$CoverCrop == "Clover" & Notes.2021.1$Treatment == "D" & 
               
               Notes.2021.1$Sampling.Time == 45, ]




plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                        
                        Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;





##### Switch T30 to T45 and T45 to T30, And Remove Note #####


T30 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                    
                    Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
                     
                     Data.2021.2$Sampling.Time == 45 , c("CO2.mg.m3" , "N2O.mg.m3")] ;



T45 <- Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
                     
                     Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
                     
                     Data.2021.2$Sampling.Time == 30 , c("CO2.mg.m3" , "N2O.mg.m3")] ;



Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
              
              Data.2021.2$Sampling.Time == 45 , c("CO2.mg.m3" , "N2O.mg.m3")]  <- T45 ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
              
              Data.2021.2$Sampling.Time == 30 , c("CO2.mg.m3" , "N2O.mg.m3")] <- T30 ;

rm(T30,T45)



######### Remove Note  #####

Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
              
              Data.2021.2$Sampling.Time == 30 , c("Field.Notes" , "Note")] <- c("NO" , NA) ;


Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-02" & Data.2021.2$BLOCK == 2 & 
              
              Data.2021.2$CoverCrop == "Clover" & Data.2021.2$Treatment == "D" &
              
              Data.2021.2$Sampling.Time == 45 , c("Field.Notes" , "Note")] <- c("NO" , NA) ;



###############################################################################################################



unique(Notes.2021.1$Sampling.Date)

Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-17",]

##### Sampling.Date == "2021-09-17" & $BLOCK == 1 & & CoverCrop == "3Spp" & Treatment == "B"
##### Sampling.Time == 45 
##### T 45 no Sample


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-17" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "3Spp" & Notes.2021.1$Treatment == "B" & 
               
               Notes.2021.1$Sampling.Time == 45, ]



plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### Remove T45 ##########

Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "3Spp" & Data.2021.2$Treatment == "B" &
                                          
                                          Data.2021.2$Sampling.Time == 45,] ) ;



Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021



###############################################################################################################



Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-17",]

##### Sampling.Date == "2021-09-17" & $BLOCK == 1 & & CoverCrop == "Trit" & Treatment == "C"
##### Sampling.Time == 45


Notes.2021.1[Notes.2021.1$Sampling.Date == "2021-09-17" & Notes.2021.1$BLOCK == 1 & 
               
               Notes.2021.1$CoverCrop == "Trit" & Notes.2021.1$Treatment == "C" & 
               
               Notes.2021.1$Sampling.Time == 45, ]


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C", "CO2.mg.m3"] ,
      
      main = "CO2",
      
      col = "blue") ;


plot( x = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" , "Sampling.Time"] ,
      
      y = Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                        
                        Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C", "N2O.mg.m3"] ,
      
      main = "N2O",
      
      col = "blue") ;


##### Remove T45 ##########

Data.Remove.2021.T <- rbind(Data.Remove.2021,
                            
                            Data.2021.2[Data.2021.2$Sampling.Date == "2021-09-17" & Data.2021.2$BLOCK == 1 & 
                                          
                                          Data.2021.2$CoverCrop == "Trit" & Data.2021.2$Treatment == "C" &
                                          
                                          Data.2021.2$Sampling.Time == 45, ]) ;


Data.Remove.2021 <- Data.Remove.2021.T ;

rm(Data.Remove.2021.T) ;

Data.Remove.2021


###############################################################################################################



##############################################################################################################
#                           Check Notes and Removals
###############################################################################################################

HMR.Data.2021 <- Data.2021.2[!Data.2021.2$ID %in% Data.Remove.2021$ID ,]


HMR.Data.2021[which(HMR.Data.2021$Field.Notes == "YES"),] 

str(HMR.Data.2021)



###############################################################################################################
#                           
#                               Writing HMR.Data.2021
#
###############################################################################################################




# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
# 
# write.csv(x = HMR.Data.2021, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\HMR.Data.2021_",
# 
# Working.Date , ".csv"), row.names = F )
# 
# 
# read.csv(file = paste0("FluxDataAnalysisResults\\HMR.Data.2021_2026_11_26_17_17_56.csv" ))

