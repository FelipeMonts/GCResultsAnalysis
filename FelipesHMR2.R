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
#
# Pedersen, Asger R. “HMR: Flux Estimation with Static Chamber Data,” May 20, 2020. https://CRAN.R-project.org/package=HMR.
#
#
#  The code in the package only uses "," or ";" separated values read from a text file as inputs. This version will read
# 
#   the input from data frame and do the calculations all with in R without any external code in C or C++
#
#   which allows to study the fitted model thoroughly.
# 
#
#############################################################################################################


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
#                           Select the N2O Sampling year
###############################################################################################################

# Year = 2021

Year = 2022


###############################################################################################################
#                           Select the Gas
###############################################################################################################


Gas = "CO2"
# 
# Gas = "N2O"
# 
# Gas = "CH4"




###############################################################################################################
#                           load the working data from GCANalysis 
###############################################################################################################


load(paste0("FluxDataAnalysisResults\\" , Gas , "_GCAnalysis_" , Year, ".RData"))



# ########### Ad chamber dimensions for the calculations
# 
# str(Chamber.Dimensions)
# 
V <- Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Volume" , c("VALUE")]

A <- Chamber.Dimensions[Chamber.Dimensions$DIMENSION == "Surface.Area" , c("VALUE")]

h = V / A

# Series.1 <- HMR.Test.Data[1:11, c( "Time" , "Concentration")];
# 
# plot(Concentration ~ Time , data = Series.1)

#  Ct = phi + f0(exp(-kt) / -kh ) 



##############################################################################################################
# 
#             Getting all the data with errors in the HMR procedure
# 
###############################################################################################################

str(Flux.Data)

unique(Flux.Data$Warning)

Flux.Data$Sample.Name

######## converting fo from character to numeric

Flux.Data$f0 <- as.numeric(Flux.Data$f0) ;

Flux.Data$LR.f0<- as.numeric(Flux.Data$LR.f0) ;

####### use row names to identify rows in the primary data frame ######

Flux.Data$Row.Names <- row.names.data.frame(Flux.Data) ;

Flux.Data[Flux.Data$Warning == "Data error", c("Series")] ;

unique(Flux.Data[Flux.Data$Warning == "Data error", c("Series")])

Flux.Data.Error <- unique(Flux.Data[which(Flux.Data$Warning == "Data error") , c("Series")]) ;

str(Flux.Data.Error)

#unique(Flux.Data[Flux.Data$Series %in% Flux.Data.Error, c("Series")])



#Flux.Data[Flux.Data$Series == Flux.Data.Error[[1]] ,]


##############################################################################################################
# 
#    Initializing the data frame to collect all the revised data in Flux.Data.Error.Revised
# 
###############################################################################################################

Flux.Data.Error.Revised.1 <- Flux.Data[0,]


Flux.Data.Error.Revised <- cbind(Flux.Data.Error.Revised.1 , data.frame(Rev.Prefilter.p = double(), Rev.LR.f0 = double()))


str(Flux.Data.Error.Revised)

##############################################################################################################
# 
#    Pre-filtering for discarding no fluxes based on the variance of the t0 concentration measurements
# 
###############################################################################################################
# j=1

for (j in seq(2,length(Flux.Data.Error))) {
  
  Flux.Data.Process <- Flux.Data[Flux.Data$Series == Flux.Data.Error[[j]] ,]
  
  # str(Flux.Data.Process)
  
  
  sigma2 <- var(Flux.Data.Process[,paste0(Gas,".ppm")])
  
  
  n <- dim(Flux.Data.Process)[1]
  
  P.Noflux <- dchisq( x = n-1 * (sigma2 / sigma02), df = n-1 ) 
  
  
  # 
  # plot(CO2.ppm ~ Sampling.Time ,  data = Flux.Data.Process)
  # 
  # points(CO2.ppm ~ Sampling.Time ,  data = Flux.Data.Process[Flux.Data.Process$Sampling.Time == 0,], pch = 16, col = 'red')
  # 
  # points(CO2.ppm ~ Sampling.Time ,  data = Flux.Data.Process[Flux.Data.Process$Sampling.Time == 15,], pch = 16, col = 'blue')
  # 
  # points(CO2.ppm ~ Sampling.Time ,  data = Flux.Data.Process[Flux.Data.Process$Sampling.Time == 30,], pch = 16,col = 'cyan')
  # 
  # points(CO2.ppm ~ Sampling.Time ,  data = Flux.Data.Process[Flux.Data.Process$Sampling.Time == 45,], pch = 16,col = 'magenta')
  
  
  
  ###############################################################################################################
  # 
  #    Testing the fitting of the k parameter with the 
  # 
  ###############################################################################################################
  
  ########### Doing K MSE minimization ######################
  
  # k <- seq(0.01,0.1, by = 0.001) ;
  
  # k <- seq(0.02,0.06, by = 0.001) ;
  
  k <- seq(0.000001,0.04, by = 0.0001) 
  
  
  length(k)
  str(Flux.Data.Process)
  
  
  ######## initializing the dataframe to collect the results from k
  
  HMR.k.0 <- data.frame( k.i = double() , MSQE = double() , phi = double() , f0 = double()) ;
  
  str(HMR.k.0)
  
  # i = 1
  
  for (i in seq(1 : length(k) ) ) {
    
    # as.formula(Concentration ~ phi + f0*(exp(-k*Time)/-k*h))
    #  
    #  HMR.nls1 <- nls(formula = Concentration ~ phi + f0*(exp(-i*Time)/-i*h) ,  start = list(phi = 19, f0 = 0.003 ),
    #                  
    #                  data = Series.1 , trace = T);
    #  
    
    Flux.Data.Process$xi <- exp(-k[[i]] * Flux.Data.Process$Sampling.Time) / (-k[[i]]*h)
    
    
    #  plot(Concentration ~ xi , data = Max.Flux.Data)
    
    HMR.lm.xi <- lm(as.formula(paste0(Gas , ".ppm" , " ~ ", "xi")) , data = Flux.Data.Process )
    
    deviance(HMR.lm.xi)
    
    
    HMR.k.1 <- data.frame( k.i = k[[i]], MSQE = deviance(HMR.lm.xi) , phi = coef(HMR.lm.xi)[1] , f0 = coef(HMR.lm.xi)[2] )
    
    HMR.k.2 <- rbind(HMR.k.0 , HMR.k.1)
    
    HMR.k.0 <-  HMR.k.2
    
  }
  
  # str(HMR.k.0)
  
  plot(MSQE ~ k.i , data = HMR.k.0, log = "x", main = paste0(Flux.Data.Error[[j]]," p-Noise ", 
                                                             
                                                             signif(as.numeric(P.Noflux),3)))
  
  plot(MSQE ~ k.i , data = HMR.k.0 , main = paste0(Flux.Data.Error[[j]]," p-Noise ", 
                                                   
                                                   signif(as.numeric(P.Noflux),3)))
  
  
  # LM.prediction.xi <- lm(CO2.ppm ~ xi , data = Flux.Data.Process)
  # 
  # f0 <-  LM.prediction.xi$coefficients [[2]] 
  
 ######## linear prediction for the raw data 
  
 
  
  LM.prediction <- lm(as.formula(paste0(Gas , ".ppm" , " ~ ", "Sampling.Time")) , data = Flux.Data.Process)
  
  plot(as.formula(paste0(Gas , ".ppm" , " ~ ", "Sampling.Time")) , data = Flux.Data.Process, main = paste0(Flux.Data.Error[[j]]," p-Noise ", 
                                                                         
                                                                         signif(as.numeric(P.Noflux),3)))
  
  
  abline(a = LM.prediction$coefficients [[1]] , b = LM.prediction$coefficients [[2]], col = 'red' )
  
  
  ###############################################################################################################
  # 
  #   
  #  collect the data  in a dataframe to join with the rest of the data
  # 
  ###############################################################################################################
  
  
  
  # str(Flux.Data)
  
  
  # Flux.Data.Error.Revised.0$Rev.Prefilter.p <- P.Noflux
  # 
  # Flux.Data.Error.Revised.0$Rev.LR.f0[1] <- LM.prediction$coefficients [[2]] 
  # 
  
  Flux.Data.Process$Rev.Prefilter.p <- P.Noflux ;
  
  Flux.Data.Process$Rev.LR.f0 <- LM.prediction$coefficients [[2]]  ;
  
  Flux.Data.Error.Revised.1 <- Flux.Data.Process ;
  
  
  Flux.Data.Error.Revised.2 <- rbind(Flux.Data.Error.Revised , Flux.Data.Error.Revised.1) ;
  
  Flux.Data.Error.Revised <- Flux.Data.Error.Revised.2 ;
  
  
  rm(Flux.Data.Error.Revised.1 ,  Flux.Data.Error.Revised.2, Flux.Data.Process)
  
   
}

str(Flux.Data)

str(Flux.Data.Error.Revised )


###############################################################################################################
# 
#   Gather together the original data Flux.Data and the data that was corrected due to
#   
#                              "Data error" Flux.Data.Error.Revised
#  
# 
###############################################################################################################


Flux.Data.Corrected <- merge( x = Flux.Data ,  y = Flux.Data.Error.Revised, all.x = T) ;

str(Flux.Data.Corrected)

#Flux.Data.Error<- unique(Flux.Data[which(Flux.Data$Warning == "Data error") , c("Series")]) ;

head(Flux.Data.Corrected,5)

tail(Flux.Data.Corrected,5)

unique(Flux.Data.Corrected$Series)

Flux.Data.Corrected[Flux.Data.Corrected$Series == "20210812_4_Clover_A", 
                    
                    c( "Row.Names" , "xi" , "Rev.Prefilter.p" ,  "Rev.LR.f0" )]

#Flux.Data.No.Error<- unique(Flux.Data[which(Flux.Data$Warning == "Data error") , c("Series")]) ;

Flux.Data.No.Error<- unique(Flux.Data[which(!Flux.Data$Warning == "Data error") , c("Series")])

head(Flux.Data.No.Error,5)

tail(Flux.Data.No.Error,5)

Flux.Data.Corrected[Flux.Data.Corrected$Series == "20210917_4_Clover_A", 
                    
                    c( "Row.Names" , "xi" , "Rev.Prefilter.p" ,  "Rev.LR.f0" )]

str(Flux.Data.Corrected )


# write.csv( x = Flux.Data.Corrected , file = paste0("FluxDataAnalysisResults\\" , Gas, "_Flux_Data_Corrected.csv")) ;
# 
# 
# write.csv( x = Flux.Data , file = "Flux_Data.csv")

 # Flux.Data.Corrected <- read.csv(file = "Flux_Data_Corrected.csv" , as.is = T )
 # 
 # Flux.Data <- read.csv(file = "Flux_Data.csv" , as.is = T )
 # 
 # 

str(Flux.Data)

str(Flux.Data.Corrected )


###############################################################################################################
# 
#   simplify the data frame to have only one line with the flux per record and discard the multiple
#   
#                              sampling times for each record
#  
# 
###############################################################################################################


Unique.Rows.Flux <- row.names(unique.data.frame(Flux.Data.Corrected[, 
                                                                    
                                                                    c( "Sampling.Day", "Sampling.Date" , "GC.Date" , 
                                                                       
                                                                       "Treatment.F" , "BLOCK.F" , "CoverCrop.F")])) ;
                                  

Concentration.Flux.Data <- Flux.Data.Corrected[which(row.names(Flux.Data.Corrected) %in% Unique.Rows.Flux), 
                                               
                                               c( "Sampling.Day", "Sampling.Date" , "GC.Date" , "Treatment.F" , "BLOCK.F" , "CoverCrop.F" ,
                                                 
                                                 "f0" , "f0.se" , "f0.p" , "f0.lo95" , "f0.up95" , "Method" , "Warning", 
                                                 
                                                 "Prefilter" , "Prefilter.p" , "LR.f0" , "LR.f0.se" , "LR.f0.p" ,
                                                 
                                                 "LR.f0.lo95" , "LR.f0.up95", "Rev.Prefilter.p" , "Rev.LR.f0")]


str(Concentration.Flux.Data)

head(Concentration.Flux.Data)

tail(Concentration.Flux.Data)


# sapply(Concentration.Flux.Data[, c("f0" , "f0.se" , "f0.p" , "f0.lo95" , "f0.up95" , "Method" , "Prefilter" ,
# 
#                             "Prefilter.p" , "LR.f0" , "LR.f0.se" , "LR.f0.p" , "LR.f0.lo95" , "LR.f0.up95",
# 
#                             "Rev.Prefilter.p" , "Rev.LR.f0")], as.numeric)
# 

write.csv( x = Concentration.Flux.Data , file = paste0("FluxDataAnalysisResults\\" , Gas, "_Concentration.Flux.Data_", Year , ".csv")) ;

