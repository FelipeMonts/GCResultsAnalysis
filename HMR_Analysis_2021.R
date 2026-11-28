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
# Felipe Montes 2023, Updated 2026/11/26
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

read.csv(file = "C:\\Users\\frm10\\Results\\HMR.Data.2021_2026_11_26_17_17_56.csv", header = T) ;

HMR.Data.2021 <- read.csv(file = "C:\\Users\\frm10\\Results\\HMR.Data.2021_2026_11_26_17_17_56.csv", header = T) ;

str(HMR.Data.2021)

HMR.Data.2021


###############################################################################################################
#
#               Reference data taken from Allison Kohele's Calculations Excel Files
#
###############################################################################################################



Chamber.Dimensions <- data.frame(DIMENSION = c("Length", "Width" , "Height", "Volume" , "Surface.Area"),
                                 
                                 VALUE.cm = c(52.705, 32.385, 10.16 , NA , NA )) ;
                           

Chamber.Dimensions$VALUE.m <- c((52.705/100) , (32.385/100) , (10.16/100), NA ,NA ) ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume", c("VALUE.cm")] <- 
  
  Chamber.Dimensions[1,2]*Chamber.Dimensions[2,2]*Chamber.Dimensions[3,2] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area", c("VALUE.cm")] <- 
  
  Chamber.Dimensions[1,2]*Chamber.Dimensions[2,2] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume", c("VALUE.m")] <- 
  
  Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3]*Chamber.Dimensions[3,3] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area", c("VALUE.m")] <- 
  
  Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3] ;


Chamber.Dimensions




###########################################################################################################################
#                           Format concentration data for HMR
# 
# HMR data files are semicolon or comma separated files organised in five columns with, respectively, the data series names, 
# 
# chamber volumes, chamber cross-sectional areas, observation time-points, and the observed chamber concentrations.
# 
# Series;V;A;Time;Concentration
# k0d;140.625;0.5625;0;15.60
# k0d;140.625;0.5625;10;15.62
# k0d;140.625;0.5625;20;16.53
# k0d;140.625;0.5625;30;16.90
###############################################################################################################


str(HMR.Data.2021)


###### Need to create name for the sample series that will be used in the HMR package


HMR.Data.2021$HMR.Series <- paste0( HMR.Data.2021$Sampling.Date, "_", HMR.Data.2021$BLOCK , "_" , 
                                    
                                    HMR.Data.2021$CoverCrop , "_" , HMR.Data.2021$Treatment) ;

Gas.Series.2021 <- data.frame(HMR.Data.2021$HMR.Series, Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume" , c("VALUE.m")],
                         
                         Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area" , c("VALUE.m")], 
                         
                         HMR.Data.2021$Sampling.Time , HMR.Data.2021$CO2.mg.m3 , HMR.Data.2021$N2O.mg.m3  ) ;

head(Gas.Series.2021)

names(Gas.Series.2021) <- c("Series" , "Volume.m3" , "Surface.Area.m2" , "Sampling.Time.min" , "CO2.mg.m3" , "N2O.mg.m3");

str(Gas.Series.2021)

head(Gas.Series.2021)

tail(Gas.Series.2021)



head(Gas.Series.2021)   ### Need to order the series 

Gas.Series.2021 <- Gas.Series.2021[order(Gas.Series.2021$Series,Gas.Series.2021$"Sampling.Time.min" ),]


str(Gas.Series.2021)

head(Gas.Series.2021)

tail(Gas.Series.2021,30)

dim(unique(Gas.Series.2021))

anyDuplicated(Gas.Series.2021, MARGIN = 0)

which(duplicated(Gas.Series.2021 , MARGIN = 0 ) )

Gas.Series.2021[233:391,]


browser()

Gas.Series.2021.N2O <- subset( x = Gas.Series.2021 , select = -c(CO2.mg.m3)) ;

Gas.Series.2021.CO2 <- subset( x = Gas.Series.2021 , select = -c(N2O.mg.m3)) ;

###############################################################################################################
#   Calculate the Variance of the ambient concentration measurements to estimate pfvar, 
#   the assumed variance of replicate measurements of the ambient trace gas concentration
#   at the chamber site in case of no gas emission used for prefiltering of data series before analysis
###############################################################################################################

######### Variance of the ambient concentration measurements #############

str(HMR.Data.2021)


HMR.Data.2021$Sampling.Date.F <- as.factor(HMR.Data.2021$Sampling.Date );


bwplot(CO2.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0, ] )

plot(CO2.mg.m3 ~ ID , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , ] )

text( x = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "ID"] ,
      
      y = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "CO2.mg.m3"],
      
      labels = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "ID"] )


bwplot(N2O.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0, ] )


plot(N2O.mg.m3 ~ ID , 
     
     data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , ] )

text( x = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "ID"] ,
      
      y = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "N2O.mg.m3"],
      
      labels = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "ID"] )



HMR.Data.2021[ HMR.Data.2021$ID %in% c(1438:1444) ,]

plot( CO2.mg.m3 ~ Sampling.Time ,
      
      data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C", ] )

HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C", ]

browser()

##### Ambient data concentration on sampling date 20210715 is an outlier
##### The problem is that samples T0 and T30 are switched
##### Changing T0 to T30 and T30 to T0

T30 <- HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C" & 
                       
                       HMR.Data.2021$Sampling.Time == 0  , c("CO2.mg.m3" , "N2O.mg.m3") ] ;
                       
                       
T0 <- HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C" & 
                      
                      HMR.Data.2021$Sampling.Time == 30  , c("CO2.mg.m3" , "N2O.mg.m3") ] ;


HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C" & 
                
                HMR.Data.2021$Sampling.Time == 0  , c("CO2.mg.m3" , "N2O.mg.m3") ]  <- T0 ;


HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C" & 
                
                HMR.Data.2021$Sampling.Time == 30  , c("CO2.mg.m3" , "N2O.mg.m3") ] <- T30 ;



rm(T0 , T30)


plot( CO2.mg.m3 ~ Sampling.Time ,
      
      data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.2.3Spp.C", ] )


##### Check #####


bwplot(CO2.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0, ] )

bwplot(N2O.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0, ] )


plot(N2O.mg.m3 ~ ID , 
     
     data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , ] )

text( x = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "ID"] ,
      
      y = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "N2O.mg.m3"],
      
      labels = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-07-15" , "ID"] )


HMR.Data.2021[ HMR.Data.2021$ID == 1397 ,]

HMR.Data.2021[ HMR.Data.2021$HMR.Series == "2021-07-15.1.3Spp.B" ,]


plot(N2O.mg.m3 ~ Sampling.Time , 
     
     data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.1.3Spp.B", ] )

plot(CO2.mg.m3 ~ Sampling.Time , 
     
     data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-07-15.1.3Spp.B", ] )

####################################################################################

bwplot(CO2.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0, ] )


bwplot(CO2.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-08-05" , ] )

plot(CO2.mg.m3  ~  ID , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-08-05" , ] )

text( x = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-08-05" , "ID"] ,
      
      y = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-08-05" , "CO2.mg.m3"],
      
      labels = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-08-05" , "ID"] )


HMR.Data.2021[HMR.Data.2021$ID == 2797,]

HMR.Data.2021[HMR.Data.2021$ID == 2692,]

HMR.Data.2021[HMR.Data.2021$ID == 2723,]

HMR.Data.2021[HMR.Data.2021$ID == 2749,]

HMR.Data.2021[HMR.Data.2021$ID == 2604,]

HMR.Data.2021[HMR.Data.2021$ID == 2056,]

HMR.Data.2021[HMR.Data.2021$ID == 2060,]

HMR.Data.2021[HMR.Data.2021$ID == 2116,]

HMR.Data.2021[HMR.Data.2021$ID == 2155,]

HMR.Data.2021[HMR.Data.2021$ID == 1976,]

HMR.Data.2021[HMR.Data.2021$ID == 1923,]

HMR.Data.2021[HMR.Data.2021$ID == 1900,]

HMR.Data.2021[HMR.Data.2021$ID == 1892,]




plot(CO2.mg.m3 ~ Sampling.Time , 
     
     data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-08-05.1.Clover.C", ] )

plot(N2O.mg.m3 ~ Sampling.Time , 
     
     data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-08-05.1.Clover.C", ] )


HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-08-05.1.Clover.C", ]

###################################################################################


bwplot(CO2.mg.m3  ~  Sampling.Date.F , 
       
       data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-09-02" , ] )

plot(CO2.mg.m3  ~  ID , 
     
     data = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-09-02" , ] )

text( x = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-09-02" , "ID"] ,
      
      y = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-09-02" , "CO2.mg.m3"],
      
      labels = HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 & HMR.Data.2021$Sampling.Date.F == "2021-09-02" , "ID"] )



HMR.Data.2021[HMR.Data.2021$ID == 2472,]


plot(CO2.mg.m3 ~ Sampling.Time , 
     
     data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-09-02.4.3Spp.A", ] )

plot(N2O.mg.m3 ~ Sampling.Time , 
     
     data = HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-09-02.4.3Spp.A", ] )

HMR.Data.2021[HMR.Data.2021$HMR.Series == "2021-09-02.4.3Spp.A", ]



# Ambient data concentration on sampling date 20220623 is an outlier
# and should not be included in the calculation of the variance  

# sigma02.1 <- var( GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, c(paste0(Gas,".ppm"))])
# 
# sigma02 <- var(GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0 & GC.Data.NoSTD$Sampling.Day.F != "20210715",
# 
#                                 c(paste0(Gas,".ppm"))]) ;
# 
# sigma02.n <- length(GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0 & GC.Data.NoSTD$Sampling.Day.F != "20210715",
# 
#                                   c(paste0(Gas,".ppm"))]) ;


sigma02 <- var(HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 , "N2O.mg.m3"]) ;

# sigma02.n <- length(GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0 & GC.Data.NoSTD$Sampling.Day.F != "20220623",
# 
#                                c(paste0(Gas,".ppm"))]) ;

sigma02.n <- length(HMR.Data.2021[HMR.Data.2021$Sampling.Time == 0 , "N2O.mg.m3"]) ;
                                  

# Variance for Ambient data concentration year 2021 2021.N2O.sigma02 = 0.006041534 sigma02.n = 700
# Variance for Ambient data concentration year 2022 2022.N2O.sigma02 = 0.01460439, sigma02.n = 442


####### Pooled Variance 2021 & 2022 ##############

Sigma02.all <- (((700 - 1) * 0.006041534) + ( (442 - 1) * 0.01460439 )) / (700 + 442 - 2) ;



####### Write out data series for HMR ##############


write.table( x = Gas.Series.2021.N2O , file = "Gas_Series_2021_N2O.csv" , row.names = F , sep = ";" )

###############################################################################################################
#                          Calculate flux with the HMR package
###############################################################################################################

read.table(file = "Gas_Series_2021_N2O.csv", header = T, sep = ";")


str(read.csv(file = "Gas_Series_2021_N2O.csv", header = T, sep = ";"))

HMR(filename='Gas_Series_2021_N2O.csv')


Gas.HRM.Results <- HMR(filename = "Gas.Series.2021.N2O.csv", dec = "." , sep = ";", SatPct = NA,
                       
                       SatTimeMin = NA, pfvar = Sigma02.all, pfalpha = 0.05,  LR.always = T , FollowHMR = F,
                       
                       IfNoValidHMR = 'LR', IfNoFlux = 'No flux',  IfNoSignal = 'No flux') ;

str(Gas.HRM.Results)


str(Gas.HRM.Results$Series)


######## Results that have errors  #########

str(Gas.HRM.Results[Gas.HRM.Results$Warning == "Data error" ,]) ;


Gas.HRM.Results[Gas.HRM.Results$Warning == "Data error" , c("Series")] ;


###############################################################################################################
#                   Calculate flux of records with Warning "Data error" 
#                   using manual option of the  HMR package
###############################################################################################################

Gas.HRM.Results.Error <- Gas.HRM.Results[Gas.HRM.Results$Warning == "Data error" , c("Series")]  ;

Gas.Series.HMR.Error <- unique(Gas.Series.HMR[Gas.Series.HMR$Series %in% Gas.HRM.Results.Error, ]) ;

str(Gas.Series.HMR.Error)

unique(Gas.Series.HMR.Error$Series)

length(unique(Gas.Series.HMR.Error$Series))



write.table(x = Gas.Series.HMR.Error , sep = ";", dec = "." , 
            
            file = paste0(Gas,"_" , Year , "_Series_Error.csv"), row.names = F)  ;

read.table(file = paste0(Gas,"_" , Year , "_Series_Error.csv"), header = T, sep = ";")




Gas.HRM.Results.Error <- HMR(filename = paste0(Gas,"_" , Year , "_Series_Error.csv") , sep = ";" , dec = "." , SatPct = NA,
                             SatTimeMin = NA, pfvar = Sigma02.all, pfalpha = 0.05,  LR.always = T , FollowHMR = F,
                             IfNoValidHMR = 'LR', IfNoFlux = 'No flux',  IfNoSignal = 'No flux') ;

str(Gas.HRM.Results)

#series that need revision:
#








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

