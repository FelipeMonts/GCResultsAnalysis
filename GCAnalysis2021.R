##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused on measurements on year 2021
# 
# 
#  Felipe Montes 2026/06/02
# 
# 
# 
# 
############################################################################################################### 


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

setwd("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis");


#### Read data  #####


PeakArea.results.2021 <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults2021.csv" ) , header = T) ;

###############################################################################################################
#                           
#                              Checking for duplicated records
#
###############################################################################################################

str(PeakArea.results.2021)

head(PeakArea.results.2021)

anyDuplicated(PeakArea.results.2021, MARGIN = c(1,2))



##################################  Remove duplicates  ######################################################

PeakArea.results.2021.1 <- PeakArea.results.2021[!duplicated(PeakArea.results, MARGIN = c(1,2)),] ;

str(PeakArea.results.2021.1) 

anyDuplicated(PeakArea.results.2021.1, MARGIN = c(1,2))

PeakArea.results.2021 <- PeakArea.results.2021.1  ;

str(PeakArea.results.2021) 

rm(PeakArea.results.2021.1)


#################################################################################################################
#                           
#                              Check for repeated measurements 
#
###############################################################################################################

names(PeakArea.results.2021)

anyDuplicated(PeakArea.results.2021[, c(4,5,6,8)], MARGIN = c(1,2))

which(duplicated(PeakArea.results.2021[, c(4,5,6)], MARGIN = c(1,2)))

PeakArea.results.2021.Repeated <- PeakArea.results.2021[duplicated(PeakArea.results.2021[, c(4,5,6)], MARGIN = c(1,2)),c(4,5,6) ]

PeakArea.results.2021[PeakArea.results.2021$CH4 %in% PeakArea.results.2021.Repeated$CH4 &
                      
                        PeakArea.results.2021$CO2 %in% PeakArea.results.2021.Repeated$CO2 &
                        
                        PeakArea.results.2021$N2O %in% PeakArea.results.2021.Repeated$N2O,]
                        
                        

#### All the duplicated measurements are in 20210614B1B2summaryreport1.pdf  and in 20210614B1B2peakareasMERGED.pdf ###################################

#### The 20210614B1B2peakareasMERGED.pdf GC analyxix was doen on 06/30/2021 and the 20210614B1B2peakareasMERGED.pdf on 07/01/2021

### Comparing the two data sets 20210614B1B2summaryreport1.pdf and 20210614B1B2peakareasMERGED.pdf #############


D.20210614B1B2summaryreport1 <- PeakArea.results.2021[PeakArea.results.2021$File == "20210614B1B2summaryreport1.pdf" ,] ;

D.20210614B1B2peakareasMERGED <- PeakArea.results.2021[PeakArea.results.2021$File == "20210614B1B2peakareasMERGED.pdf" ,] ;


str(D.20210614B1B2summaryreport1)

str(D.20210614B1B2peakareasMERGED)


###  CH4  #### 

range(D.20210614B1B2summaryreport1$CH4)

range(D.20210614B1B2peakareasMERGED$CH4)

plot(D.20210614B1B2summaryreport1$CH4, col = "red", cex = 1.2)

points(D.20210614B1B2peakareasMERGED$CH4, pch = 19 , col = "blue" ,  cex = 0.9)

### CO2 ####

range(D.20210614B1B2summaryreport1$CO2)

range(D.20210614B1B2peakareasMERGED$CO2)

plot(D.20210614B1B2summaryreport1$CO2, col = "red", cex = 1.2)

points(D.20210614B1B2peakareasMERGED$CO2, pch = 19 , col = "blue" ,  cex = 0.9)


### N2O ####

range(D.20210614B1B2summaryreport1$N2O)

range(D.20210614B1B2peakareasMERGED$N2O)

plot(D.20210614B1B2summaryreport1$N2O, col = "red", cex = 1.2)

points(D.20210614B1B2peakareasMERGED$N2O, pch = 19 , col = "blue" ,  cex = 0.9)



### The data are identical, one of the data sets can be removed ####


PeakArea.results.2021.2 <- PeakArea.results.2021 ;

str(PeakArea.results.2021.2)

str(PeakArea.results.2021.2[PeakArea.results.2021.2$AnalysisName == "20210614B1B2peakareasMERGED.pdf" ,])


PeakArea.results.2021 <- PeakArea.results.2021.2[PeakArea.results.2021.2$AnalysisName != "20210614B1B2peakareasMERGED.pdf" ,] 

str(PeakArea.results.2021)

PeakArea.results.2021[PeakArea.results.2021$AnalysisName == "20210614B1B2peakareasMERGED.pdf" ,]

rm(PeakArea.results.2021.2)


#################################################################################################################
#                           
#                              Exploring the data without standards
#
###############################################################################################################


GC.Data.NoSTD.2021 <- PeakArea.results.2021[grep( pattern = "B" , x = PeakArea.results.2021$Sample.Name, invert = F) ,] ;

##### Data with no standards included

str(GC.Data.NoSTD.2021)



plot.CH4.hist.dat <- hist(GC.Data.NoSTD.2021$CH4)

plot.CH4.density.dat <- density(GC.Data.NoSTD.2021$CH4, na.rm=T) 

plot(plot.CH4.density.dat)

plot.CO2.hist.dat <- hist(GC.Data.NoSTD.2021$CO2)

plot.CO2.density.dat <- density(GC.Data.NoSTD.2021$CO2, na.rm=T)

plot(plot.CO2.density.dat)


plot.N2O.hist.dat <- hist(GC.Data.NoSTD.2021$N2O)

plot.N2O.density.dat<-density(GC.Data.NoSTD.2021$N2O, na.rm=T)

plot(plot.N2O.density.dat)

###############################################################################################################
#                          
#            Calculation of concentration based on the Standard gas concentrations
#             
#            The calibration curves were calculated with the StandardCalibration.R code
#  
#
###############################################################################################################


str(GC.Data.NoSTD.2021)


##### CO2 ######

GC.Data.NoSTD.2021.CO2.Intercept <- -182.031;

GC.Data.NoSTD.2021.CO2.Slope <- 0.279;

GC.Data.NoSTD.2021$CO2.ppm <- (GC.Data.NoSTD.2021$CO2 * GC.Data.NoSTD.2021.CO2.Slope) + GC.Data.NoSTD.2021.CO2.Intercept ;

plot(GC.Data.NoSTD.2021$CO2.ppm )


##### N2O ######

GC.Data.NoSTD.2021.N2O.Intercept <- -0.18282 ;

GC.Data.NoSTD.2021.N2O.Slope <- 0.00155 ;

GC.Data.NoSTD.2021$N2O.ppm <- (GC.Data.NoSTD.2021$N2O * GC.Data.NoSTD.2021.N2O.Slope) + GC.Data.NoSTD.2021.N2O.Intercept ;

plot(GC.Data.NoSTD.2021$N2O.ppm )



##############################################################################################################
#                           
#                               Organizing the data for visualization
#
###############################################################################################################

str(GC.Data.NoSTD.2021)



### Organizing the data according to Treatments

unique(GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$Treatment<-c("NONE");


GC.Data.NoSTD.2021[grep("AT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T), c("Treatment")]<-c("A");

GC.Data.NoSTD.2021[grep("BT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T), c("Treatment")]<-c("B");

GC.Data.NoSTD.2021[grep("CT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T) , c("Treatment")]<-c("C");

GC.Data.NoSTD.2021[grep("DT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T) , c("Treatment")]<-c("D");

### Check if there was any treatment left with "NONE" label

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Treatment == "NONE"), ];

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B4TritC30",]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B4TritC30", c("Treatment")] <- c("C") ;


GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Treatment == "NONE"), ];

### Organizing the data according to Blocks

grep("B1",GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$BLOCK<-c(9999);

GC.Data.NoSTD.2021[grep("B1",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(1);

GC.Data.NoSTD.2021[grep("B2",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(2);

GC.Data.NoSTD.2021[grep("B3",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(3);

GC.Data.NoSTD.2021[grep("B4",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(4);

### Check if there was any BLOCK labeled 9999

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$BLOCK == 9999 ), ];



### Organizing the data according to CoverCrop

grep("3Spp",GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$CoverCrop <- c("NONE");

GC.Data.NoSTD.2021[grep("3Spp",GC.Data.NoSTD.2021$Sample.Name), c("CoverCrop")] <- c("3Spp");

GC.Data.NoSTD.2021[grep("Clover",GC.Data.NoSTD.2021$Sample.Name), c("CoverCrop")] <- c("Clover");

GC.Data.NoSTD.2021[grep("Trit",GC.Data.NoSTD.2021$Sample.Name), c("CoverCrop")] <- c("Trit");


### Check if there was any  CoverCrop labeled "NONE"

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$CoverCrop == "NONE" ), ];


### Organizing the data according to Sampling Time Order

grep("T0",GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$Sampling.Time <- c(9999);

GC.Data.NoSTD.2021[grep("T0",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(0);

GC.Data.NoSTD.2021[grep("T15",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(15);

GC.Data.NoSTD.2021[grep("T30",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(30);

GC.Data.NoSTD.2021[grep("T45",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(45);


### Check if there was any Sampling.Time left with "NONE" label

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Sampling.Time==9999),];


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B4TritC30",] ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B4TritC30", c("Sampling.Time")] <- c(30) ;


GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Sampling.Time==9999),];



### Converting experimental designations into factors

GC.Data.NoSTD.2021$Treatment.F <- as.factor(GC.Data.NoSTD.2021$Treatment) ;

GC.Data.NoSTD.2021$BLOCK.F <- as.factor(GC.Data.NoSTD.2021$BLOCK) ;

GC.Data.NoSTD.2021$CoverCrop.F <- as.factor(GC.Data.NoSTD.2021$CoverCrop) ;









###############################################################################################################
#                           
#                               Exploratory Data visualization
#
###############################################################################################################

GC.Data.NoSTD.2021$Series <- paste( GC.Data.NoSTD.2021$Sampling.Day , GC.Data.NoSTD.2021$BLOCK.F , 
                               
                                    GC.Data.NoSTD.2021$CoverCrop.F , GC.Data.NoSTD.2021$Treatment.F, sep = "_") ;

head(GC.Data.NoSTD.2021)



str(GC.Data.NoSTD.2021)


levels(GC.Data.NoSTD.2021$Treatment.F)

levels(GC.Data.NoSTD.2021$BLOCK.F)

levels(GC.Data.NoSTD.2021$CoverCrop.F)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$CoverCrop.F == "Clover" ,]


xyplot(CO2.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD.2021 , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD.2021$CO2.ppm)) ,   
       
       type="o", auto.key = T, main = "CO2");


xyplot(N2O.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD.2021 , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD.2021$N2O.ppm)) ,   
       
       type="o", auto.key = T , main = "N2O");




###############################################################################################################
#
#               Reference data taken from Allison Kohele's Calculations Excel Files
#
###############################################################################################################





Chamber.Dimensions<-data.frame(DIMENSION=c("Length", "Width" , "Height", "Volume" , "Surface.Area"), UNITS = c("m"), VALUE=c(0.52705, 0.32385, 0.1016, 9999, 9999));

Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Volume", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3]*Chamber.Dimensions[3,3] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Surface.Area", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3] ;

Molar.Mass<-data.frame(GAS=c("CH4" , "CO2" , "N2O"), UNITS=c("g/mol"), VALUE=c(16.04, 44.01, 44.013));

Gas.Law<-data.frame(UNITS=c("L-atm/Mol-K", "J/K-Mol", "m3-Pa/K-Mol", "Kg-m2-s2/K-Mol", "m3-atm/K-Mol"), VALUE=c(0.08205736, 8.314462,8.314462, 8.314462, 8.205736e-5 ))  ;


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
#
###############################################################################################################

# write.table(x = Test.data.HMR.CO2.1, sep = ";", dec = "." ,file = "TEST_DATA.csv", row.names = F)



# save.image(file = paste0("FluxDataAnalysisResults\\GCAnalysis" , Year , ".RData"))


 
  