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
#                           Select the N2O Sampling year
###############################################################################################################

Year = 2021

# Year = 2022


###############################################################################################################
#                           Select the Gas
###############################################################################################################


Gas = "CO2"
# 
# Gas = "N2O"
# 
# Gas = "CH4"


 
 
###############################################################################################################
#                           load the working data from Concentration.Flux.Data analysis 
###############################################################################################################

Concentration.Flux.Data.1 <- read.csv( file = paste0("FluxDataAnalysisResults\\" , 
                                                   
                                                   Gas, "_Concentration.Flux.Data_", Year , ".csv"))  ;

#  str(Concentration.Flux.Data.1)


############# order the data by date ############

Concentration.Flux.Data.1$Sampling.Date <- as.Date(Concentration.Flux.Data.1$Sampling.Date) ;

Concentration.Flux.Data <- Concentration.Flux.Data.1[order(Concentration.Flux.Data.1$Sampling.Date),]

str(Concentration.Flux.Data)

head(Concentration.Flux.Data)



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

R = 8.31446261815324

Patm = 101325



Concentration.Flux.Data$Temp.K <- 293.15 ;

###############################################################################################################
#        Calculating based on the different flux rate method determination "f0" , "LR.f0" , "Rev.LR.f0"
###############################################################################################################

str(Concentration.Flux.Data)

head(Concentration.Flux.Data)

####################  f0 #################


Concentration.Flux.Data$f0.mol.min <- Concentration.Flux.Data$f0 * Patm / (R * Concentration.Flux.Data$Temp.K *1e6) ;


Concentration.Flux.Data[which(is.na(Concentration.Flux.Data$f0.mol.min)),]



####################  LR.f0 measurements with the warning "Data error"    ##############

Concentration.Flux.Data[Concentration.Flux.Data$Warning == "Data error" , c("LR.f0")] <-  
  
  Concentration.Flux.Data[Concentration.Flux.Data$Warning == "Data error" , c("Rev.LR.f0")] ;


Concentration.Flux.Data[which(is.na(Concentration.Flux.Data$LR.f0)),]

####################  LR.f0 ##############

Concentration.Flux.Data$LR.f0.mol.min <- Concentration.Flux.Data$LR.f0 * Patm / (R * Concentration.Flux.Data$Temp.K *1e6) ;



####################  Measurements with the probability of being noise >= 0.05 ################


Concentration.Flux.Data[Concentration.Flux.Data$Warning == "Data error" , c("Prefilter.p")] <-  
  
  Concentration.Flux.Data[Concentration.Flux.Data$Warning == "Data error" , c("Rev.Prefilter.p")] ;


Concentration.Flux.Data[which(is.na(Concentration.Flux.Data$Prefilter.p)),]


Concentration.Flux.Data[Concentration.Flux.Data$Prefilter.p >= 0.05 , c("LR.f0.mol.min")] <- 1e-10 ;


str(Concentration.Flux.Data)

head(Concentration.Flux.Data)

tail(Concentration.Flux.Data)


###############################################################################################################
#                           Calculating mass flux
##############################################################################################################


if (Gas == "CO2") {
  
  Gas.Substance.MW <- 44.0095  # g mol-1 
    
  Gas.Element.MW <- 12.0107  # g mol-1   
  
  
} else {
  
  if (Gas == "N2O") {
    
    Gas.Substance.MW <- 44.0128  # g mol-1 
    
    Gas.Element.MW <- 14.0067  # g mol-1   
    
    
  } else {
    
    if (Gas == "CH4")
    
    Gas.Substance.MW <- 16.0425  # g mol-1 
    
    Gas.Element.MW <- 12.0107  # g mol-1   
    
  }
  
  
}


Element.Subtance.MW.Ratio <- Gas.Substance.MW / Gas.Element.MW 


####################  f0 #################

Concentration.Flux.Data$f0.KgElement.Ha.day <- Concentration.Flux.Data$f0.mol.min * Gas.Element.MW * 1440 * 10000 / 1000 ;

head(Concentration.Flux.Data$f0.KgElement.Ha.day, 50)


Concentration.Flux.Data$f0.KgSubstance.Ha.day <-  Concentration.Flux.Data$f0.KgElement.Ha.day * Element.Subtance.MW.Ratio;

# head(Concentration.Flux.Data$f0.KgSubstance.Ha.day, 50)
# 
# head(Concentration.Flux.Data$f0.mol.min * Gas.Substance.MW * 1440 * 10000 / 1000 , 50)

str(Concentration.Flux.Data)

head(Concentration.Flux.Data)

####################  LR.f0 ##############

Concentration.Flux.Data$LR.f0.KgElement.Ha.day <- Concentration.Flux.Data$LR.f0.mol.min * Gas.Element.MW * 1440 * 10000 / 1000 ;

Concentration.Flux.Data$LR.f0.KgSubstance.Ha.day <- Concentration.Flux.Data$LR.f0.KgElement.Ha.day * Element.Subtance.MW.Ratio ;

str(Concentration.Flux.Data)

head(Concentration.Flux.Data)



###############################################################################################################
#                           Calculate cumulative emissions
###############################################################################################################


############ Create unique experimental unit identifier ########################################

Concentration.Flux.Data$Exp.Unit.ID <- as.factor(paste0(Concentration.Flux.Data$BLOCK.F, ".",
                       
                       Concentration.Flux.Data$CoverCrop.F, "." ,
                       
                       Concentration.Flux.Data$Treatment.F) ) 

levels(Concentration.Flux.Data$Exp.Unit.ID)

#  i = "2.3Spp.A"  


for (i in levels(Concentration.Flux.Data$Exp.Unit.ID)) {
  
  
  plot(LR.f0.KgElement.Ha.day ~ Sampling.Date, 
       
       data = Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == i , 
                                      
              c("Sampling.Date"  ,"LR.f0.KgElement.Ha.day")  ] , col = "blue",
       
       ylab = paste0(Gas , "_KgElement.Ha.day"), main = i    ) ;
  
  
  
  
  ######### Area under the curve  - trapezoidal rule ######################
  
  # https://stackoverflow.com/questions/4954507/calculate-the-area-under-a-curve
  # 
  # 
  # sum(diff(x) * (head(y,-1)+tail(y,-1)))/2
  

 Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == i ,];
  
  Flux.Calc.Data <- Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == i , ]
  
  x.1 <- Flux.Calc.Data[seq(1,dim.data.frame(Flux.Calc.Data)[1]-1), c("Sampling.Date")] ;
  
  x.2 <- Flux.Calc.Data[seq(2,dim.data.frame(Flux.Calc.Data)[1]), c("Sampling.Date")] ;
  
  dim.data.frame(Flux.Calc.Data)
  
  length(x.1)
  
  length(x.2)
  
  Delta.x <- c(1, (x.2 - x.1)) 
  
  str(Delta.x)
  
  y.1 <- Flux.Calc.Data[seq(1,dim.data.frame(Flux.Calc.Data)[1]-1), c("LR.f0.KgElement.Ha.day")] ;
  
  y.2 <- Flux.Calc.Data[seq(2,dim.data.frame(Flux.Calc.Data)[1]), c("LR.f0.KgElement.Ha.day")] ;
  
  length(y.1)
  
  length(y.2)
  
  y.1.y.2 <- c(y.1[1], ((y.1 + y.2)/2)) ; 
  
  length(y.1.y.2)
  
  Total.Cum.Emissions <- sum((delta.x *(y.1 + y.2))/2, na.rm = T)
  
  Total.Cum.Emissions <- Total.Cum.Emissions.1 + Order.by.Date[1,c("LR.f0.KgElement.Ha.day") ] +
   
    Order.by.Date[dim(Order.by.Date)[1],c("LR.f0.KgElement.Ha.day") ] ;
    
  
  Incre.Cum.Emissions <- cumsum((delta.x *(y.1 + y.2))/2) ;
  
  Incre.Cum.Emissions[1] <- Order.by.Date[ ,c("LR.f0.KgElement.Ha.day") ][1] ;
  
  Incre.Cum.Emissions[length(Incre.Cum.Emissions)] <- Total.Cum.Emissions ;
  
  
  # Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == i , 
                          
                          c("Cumm.Emissions.KgElement.Ha") ] <- sum((delta.x *(y.1 + y.2))/2, na.rm = T)
  
  cumsum((delta.x *(y.1 + y.2))/2)
  
  
  Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == i , 
                          
                          c("Cumm.Emissions.KgSubstance.Ha")]
  
  
  
  
  
  # points(LR.f0.KgElement.Ha.day ~ Sampling.Date, 
  #        
  #        data = Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == "3.Trit.A" ,
  #                                       
  #                                       c("Sampling.Date"  ,"LR.f0.KgElement.Ha.day")  ] ,col = "red") ;
  # 
  # 
  # points(LR.f0.KgElement.Ha.day ~ Sampling.Date,
  #        
  #        data = Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == "3.Spp.A" ,
  #                                       
  #                                       c("Sampling.Date"  ,"LR.f0.KgElement.Ha.day")  ] ,col = "magenta");
  # 
  # plot(LR.f0.KgElement.Ha.day ~ Sampling.Date,
  # 
  #        data = Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == "3.Spp.A" ,
  # 
  #                                       c("Sampling.Date"  ,"LR.f0.KgElement.Ha.day")  ] ,col = "cyan")
  # 
  # 
  # points(LR.f0.KgElement.Ha.day ~ Sampling.Date, 
  #        
  #        data = Concentration.Flux.Data[Concentration.Flux.Data$Exp.Unit.ID == "3.Clover.D" ,
  #                                       
  #                                       c("Sampling.Date"  ,"LR.f0.KgElement.Ha.day")  ] ,col = "cyan") ;
  # 
  # 
  # 
  
}

str(Concentration.Flux.Data)

head(Concentration.Flux.Data)



write.csv( x = Concentration.Flux.Data , file = paste0("FluxDataAnalysisResults\\" , Gas, "_Mass.Flux.Data_" , Year, ".csv")) ;
