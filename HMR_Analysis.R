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

setwd(paste0("D:\\Felipe\\Current_Projects\\CCC Based Experiments\\" ,
             
             "StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\"))

getwd()

###############################################################################################################
#                           Select the N2O Sampling year
###############################################################################################################

Year = 2021

# Year = 2022


###############################################################################################################
#                           load the working data from GCANalysis 
###############################################################################################################


load(file = paste0("GCAnalysis" , Year , ".RData"))

# ########### Ad chamber dimensions for the calculations
# 
# str(Chamber.Dimensions)
# 
# V <- Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume" , c("VALUE")]
# 
# A <- Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area" , c("VALUE")] 
# 
# h = V / A

# Series.1 <- HMR.Test.Data[1:11, c( "Time" , "Concentration")];
# 
# plot(Concentration ~ Time , data = Series.1)

#  Ct = phi + f0(exp(-kt) / -kh ) 



####### GAS  HMR analysis ############

 # Gas = "CO2"
 # 
 # Gas = "N2O"
 # 
 Gas = "CH4"

Gas.Series <- data.frame(GC.Data.NoSTD[, c( "Series") ], Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume" , c("VALUE")],
                                     
                                     Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area" , c("VALUE")], 
                                     
                                     GC.Data.NoSTD[, c( "Sampling.Time" , paste0(Gas,".ppm")) ])


                         
                        

names(Gas.Series) <- c("Series" , "V" , "A" , "Time" , "Concentration") ;

str(Gas.Series)

head(Gas.Series)   ### Need to order the series 

Gas.Series.HMR <- Gas.Series[order(Gas.Series$Series,Gas.Series$Time ),]

head(Gas.Series.HMR)


write.table(x = Gas.Series.HMR , sep = ";", dec = "." ,file = paste0(Gas,"_" , Year , ".Series.csv"), row.names = F)  ;

######### Variance of the ambient concentration measurements #############

str(GC.Data.NoSTD)

GC.Data.NoSTD$Sampling.Day.F <- as.factor(GC.Data.NoSTD$Sampling.Day );

sigma02 <- var( GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, c(paste0(Gas,".ppm"))])

bwplot(as.formula(paste0(Gas,".ppm", " ~ ", "Sampling.Day.F" )) , 
       
       data = GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, ] )

bwplot(as.formula(paste0(" ~ ", Gas,".ppm")),  data = GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Time == 0, ], horizontal = T )


Gas.HRM.Results <- HMR(filename = paste0(Gas,"_" , Year , ".Series.csv") , sep = ";" , dec = "." ,Display.Message = F , FollowHMR = T, 
                       
                       pfvar = sigma02,  LR.always = T , IfNoValidHMR = 'LR', IfNoFlux = 'No flux', 
                       
                       IfNoSignal = 'No flux') ;

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


save.image(file = paste0(Gas,"_GCAnalysis_" , Year, ".RData"))

