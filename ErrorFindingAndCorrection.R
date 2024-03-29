##############################################################################################################
# 
# 
#          Program to track and correct errors in the GC analysis results 
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

setwd("D:\\Felipe\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults")




###############################################################################################################
#                           Select the Gas
###############################################################################################################


# Gas = "CO2"
# 
Gas = "N2O"
# 
# Gas = "CH4"


###############################################################################################################
#                           Select the Year
###############################################################################################################

Year = 2021

# Year = 2022

###############################################################################################################
#                         Load  results from the HMR Analysis
###############################################################################################################

load(paste0( Gas ,"_HMR_Analysis_" , Year , ".RData"))



##### The dataframe Flux.Data has the individual measurements and the fluxes combined ###############

str(Flux.Data)


##### Convert characters to numbers ####

Flux.Data$f0 <- as.numeric(Flux.Data$f0) ;

Flux.Data$LR.f0 <- as.numeric(Flux.Data$LR.f0) ;

Flux.Data$Flux <- as.numeric(Flux.Data$Flux ) ;

###### Order the data from high to low f0 fluxes ############


Flux.Data.Ord.f0 <- Flux.Data[order(Flux.Data$f0, decreasing = T),]


##### inspect the higher and the lower values ###


head(Flux.Data.Ord.f0)

plot(Flux.Data.Ord.f0$f0 )

text( x = Flux.Data.Ord.f0$f0, labels = Flux.Data.Ord.f0$Sampling.Day , pos = 4)

##### which flux is so high ###


Flux.Data.Ord.f0[Flux.Data.Ord.f0 <= 100 ,  ]


Flux.Data.Ord.f0[Flux.Data.Ord.f0$f0 >= 100 ,  ]  



xyplot(N2O.ppm ~ Sampling.Time  | BLOCK.F + CoverCrop.F, 
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == 20210629,]  , main = "20210629" ) ;

plot(N2O.ppm ~ Sampling.Time , data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Series == "20210629_2_Clover_C",] , main = "20210629") ;

text( x = 25, y = 8 , labels = paste0("f0 = ", Flux.Data.Ord.f0[Flux.Data.Ord.f0$Series == "20210629_2_Clover_C", c("f0")] [1] )) ;

text( x = 25, y = 6 , labels = paste0("f0 = ", Flux.Data.Ord.f0[Flux.Data.Ord.f0$Series == "20210629_2_Clover_C", c("LR.f0")] [1] )) ;




###### Order the data from high to low LR.f0 fluxes ############

Flux.Data.Ord.LRf0 <- Flux.Data[order(Flux.Data$LR.f0, decreasing = T),] ;


##### inspect the higher and the lower values ###


head(Flux.Data.Ord.LRf0$LR.f0,20)


plot(Flux.Data.Ord.LRf0$LR.f0) ;

text( x = Flux.Data.Ord.LRf0$LR.f0, labels = Flux.Data.Ord.LRf0$Sampling.Day , pos = 4)




###### Order the data from high to low Flux fluxes ############

Flux.Data.Ord.Flux <- Flux.Data[order(Flux.Data$Flux, decreasing = T),] ;


head(Flux.Data.Ord.Flux$Flux,20)


plot(Flux.Data.Ord.Flux$Flux) ; 

text( x = Flux.Data.Ord.Flux$Flux , labels = Flux.Data.Ord.f0$Sampling.Day , pos = 4)


########## The final data for the mass flux calculations was taken from f0 and the missing data ##########
# 
########## from “data error” from LR.f0. For N2O and CH4 all the data has to come from LR.f0.   ##########


######### 20210629_2_Clover_C #######

plot(N2O.ppm ~ Sampling.Time, data = Flux.Data.Ord.LRf0[Flux.Data.Ord.LRf0$Series == "20210629_2_Clover_C" ,],
     
     col = "blue" , main = "20210629_2_Clover_C" )  ;

lm.20210629_2_Clover_C <- lm(N2O.ppm ~ Sampling.Time + 0, data = Flux.Data.Ord.LRf0[Flux.Data.Ord.LRf0$Series == "20210629_2_Clover_C" ,])

abline(a =0 , b = lm.20210629_2_Clover_C$coefficients, col = "red" ,reg =lm.20210629_2_Clover_C ) ;

text(x =30 , y = 6, labels = paste0("slope = " , signif(lm.20210629_2_Clover_C$coefficients , 4) ) );

lm.20210629_2_Clover_C.1 <- lm(N2O.ppm ~ Sampling.Time + 0, 
                               
                               data = Flux.Data.Ord.LRf0[Flux.Data.Ord.LRf0$Series == "20210629_2_Clover_C" 
                                                         
                                                         & Flux.Data.Ord.LRf0$Sampling.Time <= 30 ,]) ;

abline(a = 0 , b = lm.20210629_2_Clover_C.1$coefficients, col = "red" ,reg =lm.20210629_2_Clover_C.1 ) ;

text(x =30 , y = 12, labels = paste0("slope = " , signif(lm.20210629_2_Clover_C.1$coefficients , 4) ) );



######### data without 20210629_2_Clover_C   ##############################

Flux.Data.Ord.LRf0.1 <- Flux.Data.Ord.LRf0[!Flux.Data.Ord.LRf0$Series == "20210629_2_Clover_C" ,]

plot(Flux.Data.Ord.LRf0.1$LR.f0, col = "red") ;

text( x = Flux.Data.Ord.LRf0.1$LR.f0, labels = Flux.Data.Ord.LRf0.1$Sampling.Day , pos = 4)



######### checking  2021072   ##############################


xyplot(N2O.ppm ~ Sampling.Time  | BLOCK.F + CoverCrop.F, 
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210702",]  , main = "20210702" ) ;

plot(N2O.ppm ~ Sampling.Time , data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Series == "20210629_2_Clover_C",] , main = "20210629") ;

text( x = 25, y = 8 , labels = paste0("f0 = ", Flux.Data.Ord.f0[Flux.Data.Ord.f0$Series == "20210629_2_Clover_C", c("f0")] [1] )) ;

text( x = 25, y = 6 , labels = paste0("f0 = ", Flux.Data.Ord.f0[Flux.Data.Ord.f0$Series == "20210629_2_Clover_C", c("LR.f0")] [1] )) ;


######### checking  2021072 , Clover block 2  ##############################


xyplot(N2O.ppm ~ Sampling.Time  | Treatment.F, groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210702" &
                                 
                                 Flux.Data.Ord.f0$BLOCK.F == 2 & Flux.Data.Ord.f0$CoverCrop.F == "Clover",]  , 
       
       main = "20210702_Clover_2", auto.key =  T ) ;


# On 20210702 there was a problem with the GC and samples were processed twice. Thet led to some problems. 
# 
# The fact that there were two series with the same name caused data problems with the HMR process. 
# 
# Needs to check the HMR data error processed data


############# Checking other data with two GC dates ###########################

unique(paste0(Flux.Data$Sampling.Day,".", Flux.Data$GC.Date))

# [1] "20210528.2021-07-01" "20210604.2021-07-01" "20210614.2021-07-01" "20210614.2021-06-30" "20210616.2021-07-16" "20210621.2021-07-16"
# [7] "20210623.2021-07-16" "20210629.2021-07-16" "20210702.2021-07-22" "20210702.2021-08-01" "20210702.2021-07-28" "20210707.2021-08-01"
# [13] "20210715.2021-08-01" "20210720.2021-08-06" "20210730.2021-08-14" "20210730.2021-08-16" "20210805.2021-08-16" "20210805.2021-08-17"
# [19] "20210812.2021-08-18" "20210812.2021-08-19" "20210819.2021-08-24" "20210819.2021-08-25" "20210902.2021-09-20" "20210917.2021-09-22"
# [25] "20210929.2021-10-01" "20210929.2021-10-04" "20211027.2021-11-19" "20211027.2021-11-20"
# 

############# These sampling days have two GC analysis dates ################################################
# 
# "20210614.2021-07-01" "20210614.2021-06-30"
# 
# "20210702.2021-07-22" "20210702.2021-08-01" "20210702.2021-07-28
# 
# "20210805.2021-08-16" "20210805.2021-08-17"
# 
# "20210812.2021-08-18" "20210812.2021-08-19"
# 
# "20210819.2021-08-24" "20210819.2021-08-25"
# 
# "20210929.2021-10-01" "20210929.2021-10-04" "20211027.2021-11-19" "20211027.2021-11-20"
#
###########################################################################################################


############ inspecting these sampling days ##############################



##### 20210614 #####

str(Flux.Data)

xyplot(N2O.ppm ~ Sampling.Time  | BLOCK.F  + CoverCrop.F + Treatment.F , groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210614",]  , 
       
       main = "20210614", auto.key = list(title = "GC.Date", space = "top", columns = 2), 
       
       par.settings = list(superpose.symbol=list(pch = 19) )) ;


##### 20210702 #####

xyplot(N2O.ppm ~ Sampling.Time  | BLOCK.F  + CoverCrop.F + Treatment.F  , groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210702",]  , 
       
       main = "20210702", auto.key = list(title = "GC.Date", space = "top", columns = 3), 
       
       par.settings = list(superpose.symbol=list(pch = 19) ) ) ;


##### 20210805 #####

xyplot(N2O.ppm ~ Sampling.Time  |  BLOCK.F  + CoverCrop.F + Treatment.F  , groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210805",]  , 
       
       main = "20210805", auto.key =  list(title = "GC.Date", space = "top", columns = 3), 
       
       par.settings = list(superpose.symbol=list(pch = 19) ) ) ;


##### 20210812 #####

xyplot(N2O.ppm ~ Sampling.Time  |  BLOCK.F  + CoverCrop.F + Treatment.F  , groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210812",]  , 
       
       main = "20210812", auto.key =  list(title = "GC.Date", space = "top", columns = 3), 
       
       par.settings = list(superpose.symbol=list(pch = 19) ) ) ;


##### 20210819 #####

xyplot(N2O.ppm ~ Sampling.Time  |  BLOCK.F  + CoverCrop.F + Treatment.F  , groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210819",]  , 
       
       main = "20210819", auto.key =  list(title = "GC.Date", space = "top", columns = 3), 
       
       par.settings = list(superpose.symbol=list(pch = 19) ) ) ;


##### 20210929 #####

xyplot(N2O.ppm ~ Sampling.Time  |  BLOCK.F  + CoverCrop.F + Treatment.F  , groups = GC.Date, pch = 19 ,
       
       data = Flux.Data.Ord.f0[Flux.Data.Ord.f0$Sampling.Day == "20210929",]  , 
       
       main = "20210929", auto.key =  list(title = "GC.Date", space = "top", columns = 3), 
       
       par.settings = list(superpose.symbol=list(pch = 19) ) ) ;




###############################################################################################################
#                         Load results from the FelipesHMR2 Analysis
###############################################################################################################

load(file = paste0(Gas,"_FelipesHMR2_" , Year, ".RData"))

str(Flux.Data.Corrected )

levels(as.factor(Flux.Data.Corrected$Warning))


unique(Flux.Data.Corrected[Flux.Data.Corrected$Warning == "Data error" , c("Series")] )

Flux.Data.Corrected[Flux.Data.Corrected$Warning == "Data error" , c("Series")]

Flux.Data.Corrected.1 <- Flux.Data.Corrected[Flux.Data.Corrected$Warning == "Data error" , ] ;


###### Order the data from high to low LR.f0 fluxes ############

Flux.Data.Corrected.1.Ord.Rev.LR.f0 <- Flux.Data.Corrected.1[order(Flux.Data.Corrected.1$Rev.LR.f0, decreasing = T),] ;

Flux.Data.Corrected.1.Ord.Rev.LR.f0.1 <- Flux.Data.Corrected.1.Ord.Rev.LR.f0[Flux.Data.Corrected.1.Ord.Rev.LR.f0$Sampling.Time == 0,] ;

duplicated(Flux.Data.Corrected.1.Ord.Rev.LR.f0.1$Series)

Flux.Data.Corrected.1.Ord.Rev.LR.f0.2 <- Flux.Data.Corrected.1.Ord.Rev.LR.f0.1[duplicated(Flux.Data.Corrected.1.Ord.Rev.LR.f0.1$Series), ] ;

##### inspect the higher and the lower values ###

plot(Flux.Data.Corrected.1.Ord.Rev.LR.f0.2$Rev.LR.f0, col = "red") ;

text( x = Flux.Data.Corrected.1.Ord.Rev.LR.f0.2$Rev.LR.f0, labels = Flux.Data.Corrected.1.Ord.Rev.LR.f0.2$Sampling.Day , pos = 4)



###############################################################################################################
#                         Read results from the Mass.Flux
###############################################################################################################


write.csv( x = Flux.Calc.Data.0 , file = paste0("FluxDataAnalysisResults\\" , Gas, "_Mass.Flux.Data_" , Year, ".csv")) ;

Mass.Flux.Data.1 <- read.csv( file = paste0("FluxDataAnalysisResults\\" , Gas, "_Mass.Flux.Data_" , Year, ".csv"))  ; 


str(Mass.Flux.Data.1)


###### Order the data from high to low LR.f0 fluxes ############


Mass.Flux.Data.2 <- Mass.Flux.Data.1[order(Mass.Flux.Data.1$LR.f0.KgElement.Ha.day , decreasing = T),]

head(Mass.Flux.Data.2)

tail(Mass.Flux.Data.2)

plot(Mass.Flux.Data.2$LR.f0.KgElement.Ha.day)

# Mass.Flux.Data.2[Mass.Flux.Data.2$LR.f0.KgElement.Ha.day >= 8, c("Sampling.Day" , "Exp.Unit.ID" , "LR.f0.KgElement.Ha.day")] ;
                 
Mass.Flux.Data.2[Mass.Flux.Data.2$LR.f0.KgElement.Ha.day >= 2, c("Sampling.Day" , "Exp.Unit.ID" , "LR.f0.KgElement.Ha.day")] ;

                
# Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20210614 & Mass.Flux.Data.2$Exp.Unit.ID == "1.Trit.B" ,]
# 
# 
# Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20210614 & Mass.Flux.Data.2$Exp.Unit.ID == "2.3Spp.A" ,]
# 
# Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20210614 & Mass.Flux.Data.2$Exp.Unit.ID == "2.Clover.C" ,]
# 
# 
# Mass.Flux.Data.2[Mass.Flux.Data.2$LR.f0.KgElement.Ha.day >= 4 & Mass.Flux.Data.2$LR.f0.KgElement.Ha.day <= 6,
#                  
#                  c("Sampling.Day" , "Exp.Unit.ID" , "LR.f0.KgElement.Ha.day")] ;
# 


# Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20210614,]

Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20220630,]

# points(Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20210614,c("LR.f0.KgElement.Ha.day")] , col = "red")

points(Mass.Flux.Data.2[Mass.Flux.Data.2$Sampling.Day == 20220630,c("LR.f0.KgElement.Ha.day")] , col = "red")

###############################################################################################################
#                          Load results from the GC analysis and GC concentration data
###############################################################################################################



paste0("FluxDataAnalysisResults\\GCAnalysis" , Year , ".RData")

load(paste0("GCAnalysis" , Year , ".RData"))


ls()

str(GC.Data.NoSTD)



###### plotting the GC data ####

boxplot(N2O ~ Treatment.F * CoverCrop.F , data =  GC.Data.NoSTD, main = Year)

boxplot(CO2 ~ Treatment.F * CoverCrop.F , data =  GC.Data.NoSTD, main = Year)

###### plotting the GC calibrated data ####

boxplot(N2O.ppm ~ Treatment.F * CoverCrop.F   , data =  GC.Data.NoSTD, main = Year)

boxplot(CO2.ppm ~ Treatment.F * CoverCrop.F , data =  GC.Data.NoSTD, main = Year)


###############################################################################################################
#                   Cases of particular interest  2021                     
###############################################################################################################


# DIMENSION UNITS      VALUE
# 1       Length     m 0.52705000
# 2        Width     m 0.32385000
# 3       Height     m 0.10160000
# 4       Volume     m 0.01734161
# 5 Surface.Area     m 0.17068514

# 20210629 2 clover c

unique(GC.Data.NoSTD$Sample.Name)

Case.20210614 <- GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Day == 20210614, ];

levels(Case.20210614$Treatment.F)

xyplot(CO2.ppm ~ Sampling.Time | BLOCK.F * CoverCrop.F, groups =  Treatment.F , 
       
       data = Case.20210614 , auto.key = T, type = c( "p" , "r"), main = as.character(Case.20210614$Sampling.Day[1])) ;


Case.20210614[, c("Series.Sampling.Time" , "CH4" , "CO2" , "N2O")]

Case.20210614[Case.20210614$BLOCK.F ==  2 & Case.20210614$CoverCrop.F == "Clover" &
                
                Case.20210614$Treatment.F == "B", c("Series.Sampling.Time" , "CH4" , "CO2" , "N2O")]

###############################################################################################################
#           Cases of particular interest  2022                    
###############################################################################################################

str(GC.Data.NoSTD)

Case.20220630 <- GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Day == 20220630, ];

levels(Case.20220630$Treatment.F)

xyplot(CO2.ppm ~ Sampling.Time | BLOCK.F * CoverCrop.F, groups =  Treatment.F , 
       
       data = Case.20220630 , auto.key = T, type = c( "p" , "r"), main = as.character(Case.20220630$Sampling.Day[1])) ;

levels(as.factor(GC.Data.NoSTD$Sampling.Day))

Case.20220629 <- GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Day == 20220629, ] ;

xyplot(CO2.ppm ~ Sampling.Time | BLOCK.F * CoverCrop.F, groups =  Treatment.F , 
       
       data = Case.20220629 , auto.key = T, type = c( "p" , "r"), main = as.character(Case.20220629$Sampling.Day[1])) ;

GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Day == 20220629 , c("GC.Date") ]

GC.Data.NoSTD[GC.Data.NoSTD$Sampling.Day == 20220630 , c("GC.Date") ]


