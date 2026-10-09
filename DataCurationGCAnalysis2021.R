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

setwd(paste0( "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\" , 

     "CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis"));


#### Read data  #####


PeakArea.results.2021 <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults_2021_2026_09_30_18_21_47.csv" ) , header = T) ;

###############################################################################################################
#                           
#                              Checking for duplicated records
#
###############################################################################################################

str(PeakArea.results.2021)

head(PeakArea.results.2021)

anyDuplicated(PeakArea.results.2021, MARGIN = c(1,2))



##################################  Remove duplicates  ######################################################

PeakArea.results.2021.1 <- PeakArea.results.2021[!duplicated(PeakArea.results.2021, MARGIN = c(1,2)),] ;

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

anyDuplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = c(1,2))

# There are 174 duplicate measurements. Not duplicate records, but GC measurements 
# 
# that were repeated in two different analysis

which(duplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = c(1,2)))

duplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = 0)

which(duplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = 0))

#  Find out which measurements are repeated and why

str(PeakArea.results.2021)

PeakArea.results.2021.Repeated <- PeakArea.results.2021[duplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = c(1,2)),c(4,5,6,7,8) ] ;

str(PeakArea.results.2021.Repeated)

PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]

PeakArea.results.2021[which(PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]), ]

PeakArea.results.2021.Repeated.measures <- PeakArea.results.2021[which(PeakArea.results.2021[,6] %in%
                                                                         
                                                                         PeakArea.results.2021.Repeated[,3]), ] ;


PeakArea.results.2021.Repeated.measures[order(PeakArea.results.2021.Repeated.measures$N2O),]

                        

#### Some of the duplicated measurements are in 20210614B1B2summaryreport1.pdf  and in 20210614B1B2peakareasMERGED.pdf ###################################

#### The 20210614B1B2peakareasMERGED.pdf GC analysis was done on 06/30/2021 and the 20210614B1B2peakareasMERGED.pdf on 07/01/2021

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

str(PeakArea.results.2021.2[PeakArea.results.2021.2$File == "20210614B1B2peakareasMERGED.pdf" ,])


PeakArea.results.2021 <- PeakArea.results.2021.2[PeakArea.results.2021.2$File != "20210614B1B2peakareasMERGED.pdf" ,] 

str(PeakArea.results.2021)

PeakArea.results.2021[PeakArea.results.2021$File == "20210614B1B2peakareasMERGED.pdf" ,]

rm(PeakArea.results.2021.2)

# #################################################################################################################
# 
# ### After removing the duplicates from 20210614B1B2, what repeated measures are still remain in the data set?
# 
# #################################################################################################################


PeakArea.results.2021.Repeated <- PeakArea.results.2021[duplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = c(1,2)),c(4,5,6,7,8) ] ;

str(PeakArea.results.2021.Repeated)

str(PeakArea.results.2021)

PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]

PeakArea.results.2021[which(PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]), ]

PeakArea.results.2021.Repeated.measures <- PeakArea.results.2021[which(PeakArea.results.2021[,6] %in%
                                                                         
                                                                         PeakArea.results.2021.Repeated[,3]), ] ;


PeakArea.results.2021.Repeated.measures[order(PeakArea.results.2021.Repeated.measures$N2O),]

unique(PeakArea.results.2021.Repeated.measures$File)

# The next repreated measures are the ones from 20210601. Mosty of them are repeated because 
# the original data sets20210601B1B2sample24-84summaryreport and 20210601B1B2sample1-22summaryreport.pdf
# does not have sample names. The sample names were taken from the 20210601calculations.xls file.
# 
# Check which records are duplicated and remove them.

PeakArea.results.2021[PeakArea.results.2021$File == "20210601B1B2sample24-84summaryreport.pdf" |
                        
                        PeakArea.results.2021$File == "20210601B1B2sample1-22summaryreport.pdf",] 

                      
Data.20210601B1B2sample24_84summaryreport <- PeakArea.results.2021[PeakArea.results.2021$File == "20210601B1B2sample24-84summaryreport.pdf",] ;

Data.20210601B1B2sample1_22summaryreport <- PeakArea.results.2021[PeakArea.results.2021$File == "20210601B1B2sample1-22summaryreport.pdf",] ;

Data.20210601calculations <- PeakArea.results.2021[PeakArea.results.2021$File == "20210601calculations.xlsx",] ;

str(Data.20210601B1B2sample24_84summaryreport)

str(Data.20210601B1B2sample1_22summaryreport)

str(Data.20210601calculations)

###  CH4  #### 

range(Data.20210601B1B2sample24_84summaryreport$CH4)

range(Data.20210601B1B2sample1_22summaryreport$CH4)

range(Data.20210601calculations$CH4, na.rm = T)

plot(Data.20210601calculations$CH4, Data.20210601calculations$N2O, col = "red", cex = 1.2)

points(Data.20210601B1B2sample24_84summaryreport$CH4,Data.20210601B1B2sample24_84summaryreport$N2O,
       
       pch = 19 , col = "blue" ,  cex = 0.9)



### CO2 ####

range(Data.20210601B1B2sample24_84summaryreport$CO2)

range(Data.20210601B1B2sample1_22summaryreport$CO2)

range(Data.20210601calculations$CO2, na.rm = T)

plot(Data.20210601calculations$CO2,Data.20210601calculations$N2O, col = "red", cex = 1.2) ;

points(Data.20210601B1B2sample1_22summaryreport$CO2,Data.20210601B1B2sample1_22summaryreport$N2O,
       
       pch = 19 , col = "blue" ,  cex = 0.9) ;

### The data from Data.20210601calculations contains all the data from 20210601B1B2sample1-22summaryreport.pdf
###  and 20210601B1B2sample24-84summaryreport.pdf, therefore the data from 20210601B1B2sample1-22summaryreport.pdf
###  and 20210601B1B2sample24-84summaryreport.pdf can be removed 


### removing data from 20210601B1B2sample1-22summaryreport.pdf

PeakArea.results.2021.3 <- PeakArea.results.2021 ;

str(PeakArea.results.2021.3)

str(PeakArea.results.2021.3[PeakArea.results.2021.3$File == "20210601B1B2sample1-22summaryreport.pdf",])


PeakArea.results.2021 <- PeakArea.results.2021.3[PeakArea.results.2021.3$File != "20210601B1B2sample1-22summaryreport.pdf" ,] 

str(PeakArea.results.2021)

PeakArea.results.2021[PeakArea.results.2021$File == "20210601B1B2sample1-22summaryreport.pdf" ,]

rm(PeakArea.results.2021.3,PeakArea.results.2021.Repeated)


### removing data from 20210601B1B2sample24-84summaryreport.pdf


PeakArea.results.2021.4 <- PeakArea.results.2021 ;

str(PeakArea.results.2021.4)

str(PeakArea.results.2021.4[PeakArea.results.2021.4$File == "20210601B1B2sample24-84summaryreport.pdf",])


PeakArea.results.2021 <- PeakArea.results.2021.4[PeakArea.results.2021.4$File != "20210601B1B2sample24-84summaryreport.pdf" ,] 

str(PeakArea.results.2021)

PeakArea.results.2021[PeakArea.results.2021$File == "20210601B1B2sample24-84summaryreport.pdf" ,]

rm(PeakArea.results.2021.4, PeakArea.results.2021.Repeated)


# #################################################################################################################
# 
#### After removing the duplicates from 20210601B1B2sample1-22summaryreport.pdf  and 20210601B1B2sample24-84summaryreport.pdf,
# 
#### what repeated measures are still remain in the data set?
#  
# #################################################################################################################

PeakArea.results.2021.Repeated <- PeakArea.results.2021[duplicated(PeakArea.results.2021[,c(4,5,6)], MARGIN = c(1,2)),c(4,5,6,7,8) ] ;

str(PeakArea.results.2021.Repeated)

str(PeakArea.results.2021)

PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]


PeakArea.results.2021[which(PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]), ]

PeakArea.results.2021.Repeated.measures <- PeakArea.results.2021[which(PeakArea.results.2021[,6] %in%
                                                                         
                                                                         PeakArea.results.2021.Repeated[,3]), ] ;

PeakArea.results.2021.Repeated.measures[order(PeakArea.results.2021.Repeated.measures$N2O),]


unique(PeakArea.results.2021.Repeated.measures$File)



# The next repeated measures are the ones from 20210528. The repeated samples are in datasets 
# sets 20210528B1B4peakareasMERGED.pdf and 20210528peakareas.pdf
# 
# Check which records are duplicated and remove them.

PeakArea.results.2021[PeakArea.results.2021$File == "20210528B1B4peakareasMERGED.pdf" |
                        
                        PeakArea.results.2021$File == "20210528peakareas.pdf",] 


Data.20210528B1B4peakareasMERGED <- PeakArea.results.2021[PeakArea.results.2021$File == "20210528B1B4peakareasMERGED.pdf",] ;

Data.20210528peakareas <- PeakArea.results.2021[PeakArea.results.2021$File == "20210528peakareas.pdf",] ;


str(Data.20210528B1B4peakareasMERGED)

str(Data.20210528peakareas)


###  CH4  #### 

range(Data.20210528B1B4peakareasMERGED$CH4)

range(Data.20210528peakareas$CH4)


plot(Data.20210528B1B4peakareasMERGED$CH4, Data.20210528B1B4peakareasMERGED$N2O, col = "red", cex = 1.2)

points(Data.20210528peakareas$CH4,Data.20210528peakareas$N2O,
       
       pch = 19 , col = "blue" ,  cex = 0.9)



### CO2 ####

range(Data.20210528B1B4peakareasMERGED$CO2)

range(Data.20210528peakareas$CO2)


plot(Data.20210528B1B4peakareasMERGED$CO2,Data.20210528B1B4peakareasMERGED$N2O, col = "red", cex = 1.2) ;

points(Data.20210528peakareas$CO2,Data.20210528peakareas$N2O,
       
       pch = 19 , col = "blue" ,  cex = 0.9) ;


#### There are some data points that are duplicated and some that are not. Need to figure out which are 
#### repeated and which are not. Afterwards the records that are not repeated need to be verified.

str(Data.20210528B1B4peakareasMERGED)

str(Data.20210528peakareas)


which(!Data.20210528peakareas$CH4 %in% Data.20210528B1B4peakareasMERGED$CH4)

which(!Data.20210528B1B4peakareasMERGED$CH4 %in% Data.20210528peakareas$CH4)

#### It seems that all data is in both data sets. It might be the case that some of the data occurs
#### in different rows in either set. 

range(Data.20210528B1B4peakareasMERGED$N2O)

range(Data.20210528peakareas$N2O)

range(c(range(Data.20210528B1B4peakareasMERGED$N2O) ,range(Data.20210528peakareas$N2O) ))

plot(Data.20210528B1B4peakareasMERGED$Position , Data.20210528B1B4peakareasMERGED$N2O, col = "red", cex = 1.2,
     
     ylim = range(c(range(Data.20210528B1B4peakareasMERGED$N2O) , range(Data.20210528peakareas$N2O) ))) ;

points(Data.20210528peakareas$Position , Data.20210528peakareas$N2O,
       
       pch = 19 , col = "blue" ,  cex = 0.9) ;


plot(Data.20210528B1B4peakareasMERGED$Position,Data.20210528B1B4peakareasMERGED$CO2, col = "red", cex = 1.2,
     
     ylim = range(c(range(Data.20210528B1B4peakareasMERGED$CO2) , range(Data.20210528peakareas$CO2) ))) ;

points(Data.20210528peakareas$Position,Data.20210528peakareas$CO2,
       
       pch = 19 , col = "blue" ,  cex = 0.9) ;



plot(Data.20210528B1B4peakareasMERGED$Position , Data.20210528B1B4peakareasMERGED$CH4, col = "red", cex = 1.2,
     
     ylim = range(c(range(Data.20210528B1B4peakareasMERGED$CH4) , range(Data.20210528peakareas$CH4) ))) ;

points(Data.20210528peakareas$Position,Data.20210528peakareas$CH4,
       
       pch = 19 , col = "blue" ,  cex = 0.9) ;


#### It seems that the problem is with the N2O data #####################################

which(! Data.20210528B1B4peakareasMERGED$N2O %in% Data.20210528peakareas$N2O )

which(! Data.20210528peakareas$N2O  %in% Data.20210528B1B4peakareasMERGED$N2O )


Data.20210528B1B4peakareasMERGED[which(! Data.20210528B1B4peakareasMERGED$N2O %in% Data.20210528peakareas$N2O ),]

Data.20210528peakareas[which(! Data.20210528peakareas$N2O  %in% Data.20210528B1B4peakareasMERGED$N2O ),]


##### How different are the results between each other?

Data.20210528B1B4peakareasMERGED[which(! Data.20210528B1B4peakareasMERGED$N2O %in% Data.20210528peakareas$N2O ),"N2O"] - 
  
  Data.20210528peakareas[which(! Data.20210528peakareas$N2O  %in% Data.20210528B1B4peakareasMERGED$N2O ), "N2O"]


#### It seems that all the N2O data from Data.20210528B1B4peakareasMERGED is higher. Therefore I am going to keep
#### only the data from Data.20210528B1B4peakareasMERGED and discard the data from   Data.20210528peakareas
  
### removing the data from Data.20210528peakareas 


PeakArea.results.2021.5 <- PeakArea.results.2021 ;

str(PeakArea.results.2021.5)

str(PeakArea.results.2021.5[PeakArea.results.2021.5$File == "20210528peakareas.pdf",])


PeakArea.results.2021 <- PeakArea.results.2021.5[PeakArea.results.2021.5$File != "20210528peakareas.pdf" ,] 

str(PeakArea.results.2021)

PeakArea.results.2021[PeakArea.results.2021$File == "20210528peakareas.pdf" ,]

rm(PeakArea.results.2021.5, PeakArea.results.2021.Repeated, PeakArea.results.2021.Repeated.measures )


# #################################################################################################################
# 
#### After removing the duplicates from "20210528peakareas.pdf"
# 
#### what repeated measures are still remain in the data set?
#  
# #################################################################################################################

PeakArea.results.2021.Repeated <- PeakArea.results.2021[duplicated(PeakArea.results.2021[,c(4,5,6)], 
                                                                   
                                                                   MARGIN = c(1,2)),c(4,5,6,7,8) ] ;

str(PeakArea.results.2021.Repeated)

str(PeakArea.results.2021)

PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]


PeakArea.results.2021[which(PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]), ]

PeakArea.results.2021.Repeated.measures <- PeakArea.results.2021[which(PeakArea.results.2021[,6] %in%
                                                                         
                                                                         PeakArea.results.2021.Repeated[,3]), ] ;

PeakArea.results.2021.Repeated.measures[order(PeakArea.results.2021.Repeated.measures$N2O),]


unique(PeakArea.results.2021.Repeated.measures$File)


# It seems that there are repeated measures in the 20210601calculations.xlsx data set
# 
# Check which records are duplicated and remove them.


PeakArea.results.2021[PeakArea.results.2021$File == "20210601calculations.xlsx" , ]


Data.20210601calculations <- PeakArea.results.2021[PeakArea.results.2021$File == "20210601calculations.xlsx",] ;


str(Data.20210601calculations)

str(PeakArea.results.2021)

PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]

which(PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3])

PeakArea.results.2021[which(PeakArea.results.2021[,6] %in% PeakArea.results.2021.Repeated[,3]) , ]

which(is.na(PeakArea.results.2021$N2O))

#### It seems that the only data left as duplicates are NA ###

### removing NA records

PeakArea.results.2021.6 <- PeakArea.results.2021 ;

str(PeakArea.results.2021.6)

str(PeakArea.results.2021.6[which(is.na(PeakArea.results.2021.6$N2O)),])

str(which(is.na(PeakArea.results.2021.6$N2O)))


PeakArea.results.2021 <- PeakArea.results.2021.6[-( which(is.na(PeakArea.results.2021.6$N2O))),] 

str(PeakArea.results.2021)

which(is.na(PeakArea.results.2021$N2O))

rm(PeakArea.results.2021.6, PeakArea.results.2021.Repeated, PeakArea.results.2021.Repeated.measures )

                                                                   
                                                                   





#################################################################################################################
#                           
#                              Removing standards data
#
###############################################################################################################


GC.Data.NoSTD.2021 <- PeakArea.results.2021[grep( pattern = "B" , x = PeakArea.results.2021$Sample.Name, invert = F) ,] ;

##### Data with no standards included

str(GC.Data.NoSTD.2021)



##############################################################################################################
#                           
#                              Converting to factors the columns that have discrete values
#                              
#                              
#
##############################################################################################################

GC.Data.NoSTD.2021


### Add treatment information ###


unique(GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$Treatment<-c("NONE");


GC.Data.NoSTD.2021[grep("AT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T), c("Treatment")]<-c("A");

GC.Data.NoSTD.2021[grep("BT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T), c("Treatment")]<-c("B");

GC.Data.NoSTD.2021[grep("CT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T) , c("Treatment")]<-c("C");

GC.Data.NoSTD.2021[grep("DT",GC.Data.NoSTD.2021$Sample.Name , ignore.case = T) , c("Treatment")]<-c("D");

### Check if there was any treatment left with "NONE" label

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Treatment == "NONE"), ];

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B4TritC30",] <- "B4TritCT30" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B4TritCT30", c("Treatment")] <- c("C") ;


GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Treatment == "NONE"), ];

GC.Data.NoSTD.2021$Treatment <- factor(GC.Data.NoSTD.2021$Treatment) ;


levels(GC.Data.NoSTD.2021$Treatment)

str(GC.Data.NoSTD.2021)


###  Add Block information ####

grep("B1",GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$BLOCK<-c(9999);

GC.Data.NoSTD.2021[grep("B1",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(1);

GC.Data.NoSTD.2021[grep("B2",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(2);

GC.Data.NoSTD.2021[grep("B3",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(3);

GC.Data.NoSTD.2021[grep("B4",GC.Data.NoSTD.2021$Sample.Name), c("BLOCK")] <- c(4);

### Check if there was any BLOCK labeled 9999

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$BLOCK == 9999 ), ];

GC.Data.NoSTD.2021$BLOCK <- factor(GC.Data.NoSTD.2021$BLOCK) ;

levels(GC.Data.NoSTD.2021$BLOCK)

str(GC.Data.NoSTD.2021)


### Adding CoverCrop Data ####

grep("3Spp",GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$CoverCrop <- c("NONE");

GC.Data.NoSTD.2021[grep("3Spp",GC.Data.NoSTD.2021$Sample.Name), c("CoverCrop")] <- c("3Spp");

GC.Data.NoSTD.2021[grep("Clover",GC.Data.NoSTD.2021$Sample.Name), c("CoverCrop")] <- c("Clover");

GC.Data.NoSTD.2021[grep("Trit",GC.Data.NoSTD.2021$Sample.Name), c("CoverCrop")] <- c("Trit");


### Check if there was any  CoverCrop labeled "NONE"

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$CoverCrop == "NONE" ), ];


GC.Data.NoSTD.2021$CoverCrop <- factor(GC.Data.NoSTD.2021$CoverCrop) ;

levels(GC.Data.NoSTD.2021$CoverCrop)

str(GC.Data.NoSTD.2021)




### Adding Sampling Time information  ###

grep("T0",GC.Data.NoSTD.2021$Sample.Name)

GC.Data.NoSTD.2021$Sampling.Time <- c(9999);

GC.Data.NoSTD.2021[grep("T0",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(0);

GC.Data.NoSTD.2021[grep("T15",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(15);

GC.Data.NoSTD.2021[grep("T30",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(30);

GC.Data.NoSTD.2021[grep("T45",GC.Data.NoSTD.2021$Sample.Name), c("Sampling.Time")] <- c(45);


### Check if there was any Sampling.Time left with "NONE" label

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Sampling.Time==9999),];

str(GC.Data.NoSTD.2021)



#### Adding plot  and location information  ###


### Plot 202 ###

GC.Data.NoSTD.2021$Plot <- NA ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 
                   

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" 
                   
                   , "Plot"] <- "202"  ;


GC.Data.NoSTD.2021$Location <- NA ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "202" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "202" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "202" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 



### Plot 210 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp", "Plot"] <- "210"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "210" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "210" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "210" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 


### Plot 212 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover", "Plot"] <- "212"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "212" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "212" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "212" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "D" &  GC.Data.NoSTD.2021$Plot == "212" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 





### Plot 406 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit", "Plot"] <- "406"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "406" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "406" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "406" , 
                   
                   "Location"] <- "Middle" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 




### Plot 401 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp", "Plot"] <- "401"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "401" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "401" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "401" , 
                   
                   "Location"] <- "Middle" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 




### Plot 412 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover", "Plot"] <- "412"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "412" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "412" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "412" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "D" &  GC.Data.NoSTD.2021$Plot == "412" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 







### Plot 910 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit", "Plot"] <- "910"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "910" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "910" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "910" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 





### Plot 907 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp", "Plot"] <- "907"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "907" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "907" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "907" , 
                   
                   "Location"] <- "Middle" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 




### Plot 903 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover", "Plot"] <- "903"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "903" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "903" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "903" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "D" &  GC.Data.NoSTD.2021$Plot == "903" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 



### Plot 1205 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit", "Plot"] <- "1205"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "1205" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "1205" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "1205" , 
                   
                   "Location"] <- "Middle" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ]





### Plot 1201 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp", "Plot"] <- "1201"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "1201" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "1201" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "1201" , 
                   
                   "Location"] <- "Middle" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ] 






### Plot 1203 ###

#  levels(GC.Data.NoSTD.2021$CoverCrop)

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover", "Plot"] <- "1203"  ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "A" &  GC.Data.NoSTD.2021$Plot == "1203" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "1203" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "1203" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "D" &  GC.Data.NoSTD.2021$Plot == "1203" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ]


####   Do a Check #####

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Plot == NA ), ]   ;

GC.Data.NoSTD.2021[which(GC.Data.NoSTD.2021$Location == NA ), ]   ;


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" , ]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" , ]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" , ]



GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "D" &  GC.Data.NoSTD.2021$Plot == "1203" , 
                   
                   "Location"]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "D" &  GC.Data.NoSTD.2021$Plot == "202" , 
                   
                   "Location"]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "B" &  GC.Data.NoSTD.2021$Plot == "202" , 
                   
                   "Location"]


GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2021$Treatment == "C" &  GC.Data.NoSTD.2021$Plot == "907" , 
                   
                   "Location"]

#####  Converting Plot and Location to factors  ####

GC.Data.NoSTD.2021$Plot <- factor(GC.Data.NoSTD.2021$Plot) ;

GC.Data.NoSTD.2021$Location <- factor(GC.Data.NoSTD.2021$Location) ;

##############################################################################################################
#                           
#                              Adding field sampling notes to the data
#                              
#                              Making adjustments when appropriate according to the notes
#
##############################################################################################################

GC.Data.NoSTD.2021$Field.Notes <- "NO" ;

GC.Data.NoSTD.2021$Note <- NA ;
  


str(GC.Data.NoSTD.2021)
  
#### Adding Notes #####


### Sampling.Date == 2021-06-01  ; Sample.Name == B1ClovD ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D" , ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D" , "Field.Notes" ] <- "YES"  ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D" , "Note" ] <- "Pump hose loose, data questionable"  ;


#### Adding Notes #####


### Sampling.Date == 2021-06-01  ; Sample.Name == B2TritCT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, 
                    
                    "Note" ] <- "Needle clogged no sample pusshed in the vial" ;   




### Sampling.Date == 2021-06-01  ; Sample.Name == B23sppAT45 ;

# Selecting set with the conditions 




GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "Pump off at T3";   




### Sampling.Date == 2021-06-01  ; Sample.Name == B3TritAT15 ;

# Selecting set with the conditions 




GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 Syringe plunger Failed"; 



### Sampling.Date == 2021-06-01  ; Sample.Name == B4CloverCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-01")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, 
                    
                    "Note" ] <- "T30 Needle clogged" ;










### Sampling.Date == 2021-06-04  ; Sample.Name == B1   all of block 1;

# Selecting set with the conditions 




GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-04")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" , ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-04")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" ,  "Field.Notes" ] <- "YES"   ; 
                    
                   

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-04")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" ,  "Note" ] <- 
  
  "Block 1 fell of the car and was left in the field from Fryday to Monday" ;




### Sampling.Date == 2021-06-04  ; Sample.Name == B1CloverB.. ;

# Selecting set with the conditions 




GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-04")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B", ]


# There are no samples with both  Sampling date "2021-06-04" and B1CloverB  because the glass
# Sampling vials broke



# Add NA data to those broken vials records

Comment.06_04.B1CloverB <- GC.Data.NoSTD.2021[0,]

Comment.06_04.B1CloverB[c(1:4),] <- NA ;

Comment.06_04.B1CloverB$Sample.Name <- paste0("B1CloverB",c("T0" , "T15", "T30" , "T45"));

Comment.06_04.B1CloverB$Sampling.Date <- "2021-06-04" ;

Comment.06_04.B1CloverB$Sampling.Day <- "20210604" ;

Comment.06_04.B1CloverB$Field.Notes <- "YES" ;

Comment.06_04.B1CloverB$Note <- "Broken glass vials no samples"  ;

Comment.06_04.B1CloverB



### Sampling.Date == 2021-06-04  ; Sample.Name == B1CloverD ;

# Selecting set with the conditions 




GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-04")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D", ]


# There are no samples with both  Sampling date "2021-06-04" and B1CloverB  because the glass
# Sampling vials broke

# Add NA data to those broken vials records


Comment.06_04.B1CloverD <- GC.Data.NoSTD.2021[0,] ;

Comment.06_04.B1CloverD[c(1:4),] <- NA ;

Comment.06_04.B1CloverD$Sample.Name <- paste0("B1CloverD",c("T0" , "T15", "T30" , "T45"));

Comment.06_04.B1CloverD$Sampling.Date <- "2021-06-04" ;

Comment.06_04.B1CloverD$Sampling.Day <- "20210604" ;

Comment.06_04.B1CloverD$Field.Notes <- "YES" ;

Comment.06_04.B1CloverD$Note <- "Broken glass vials no samples" ;

Comment.06_04.B1CloverD


#  combining the two data frames Comment.06_04.B1CloverB.B1CloverD  and Comment.06_04.B1CloverB  #


Comment.06_04.B1CloverB.B1CloverD <- rbind(Comment.06_04.B1CloverB , Comment.06_04.B1CloverD )

Comment.06_04.B1CloverB.B1CloverD


# Adding to the data  

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B1CloverD" &  GC.Data.NoSTD.2021$Sampling.Date == "2021-06-04",]


GC.Data.NoSTD.2021.add <- rbind(GC.Data.NoSTD.2021 , Comment.06_04.B1CloverB.B1CloverD) ;

GC.Data.NoSTD.2021 <- GC.Data.NoSTD.2021.add ;

GC.Data.NoSTD.2021.add[GC.Data.NoSTD.2021.add$Sample.Name == "B1CloverDT0" &  GC.Data.NoSTD.2021.add$Sampling.Date == "2021-06-04",]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B1CloverDT0" &  GC.Data.NoSTD.2021$Sampling.Date == "2021-06-04",]

GC.Data.NoSTD.2021[GC.Data.NoSTD.2021$Sample.Name == "B1CloverBT0" &  GC.Data.NoSTD.2021$Sampling.Date == "2021-06-04",]



rm(GC.Data.NoSTD.2021.add)

### Sampling.Date == 2021-06-21  ; Sample.Name == B3TriCT30 ;

# Selecting set with the conditions 



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-21")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-21")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-21")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "Syringe stock open when taking vial" ;




### Sampling.Date == 2021-06-23  ; Sample.Name == B1ClovAT15 ;

# Selecting set with the conditions 



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-23")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-23")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-23")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "Sample not fully drawn" ;



### Sampling.Date == 2021-06-23  ; Sample.Name == B33SppBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-23")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-23")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-23")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "* T45 ?"   ;



### Sampling.Date == 2021-06-29  ; Sample.Name == B1TritBT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 Broke" ;


### Sampling.Date == 2021-06-29  ; Sample.Name == B1ClovBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "Needle Broke" ;






### Sampling.Date == 2021-06-29  ; Sample.Name == B1ClovBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "No T 45" ;




### Sampling.Date == 2021-06-29  ; Sample.Name == B2ClovBT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "No T 15" ;



### Sampling.Date == 2021-06-29  ; Sample.Name == B3ClovAT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, 
                    
                    "Note" ] <- "No T3 Syringe Broke" ;



### Sampling.Date == 2021-06-29  ; Sample.Name == B3ClovCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, 
                    
                    "Note" ] <- "T2 Needle Broke" ;




### Sampling.Date == 2021-06-29  ; Sample.Name == B4TritCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 Vial not fully evacuated" ;



### Sampling.Date == 2021-06-29  ; Sample.Name == B43SppCT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-06-29")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "T45 no sample" ;



### Sampling.Date == 2021-07-02  ; Sample.Name == B1CloverCT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 no Sample Syringe broke" ;






### Sampling.Date == 2021-07-02  ; Sample.Name == B3TritAT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- " T45 was leaking"  ; 



### Sampling.Date == 2021-07-02  ; Sample.Name == B3TritBT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "T2 did not fully draw sample" ;




### Sampling.Date == 2021-07-02  ; Sample.Name == B33SppBT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T3 valve did not open or Syringe Stuck"  ; 
   
  
  
  
  
  
### Sampling.Date == 2021-07-07  ; All of block 1 was not Sampled ;

# Selecting set with the conditions 
  
GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" , ]  

### Add rows with NA for the missing records. The records of Block 1 that was not measured

## start with a template from 2021-07-02


Data.210707.Missing <- GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-02")  & 
                                             
                                             GC.Data.NoSTD.2021$BLOCK == "1" , ] ;

names(Data.210707.Missing)


Data.210707.Missing[ , c("Position" , "Vial", "CH4" , "CO2" , "N2O" , "File", "GC.Date"  )] <- NA ;


Data.210707.Missing$Sampling.Day <- 20210707 ;


Data.210707.Missing$Sampling.Date <- "2021-07-07" ;


Data.210707.Missing$Field.Notes <- "YES"  ;


Data.210707.Missing$Note <- "Block 1 was not sampled on 2021-07-07" ;

str(Data.210707.Missing)


GC.Data.NoSTD.2021.add.2 <- rbind(GC.Data.NoSTD.2021 , Data.210707.Missing ) ;

str(GC.Data.NoSTD.2021.add.2)


GC.Data.NoSTD.2021 <-  GC.Data.NoSTD.2021.add.2 ;

str(GC.Data.NoSTD.2021)


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" , ] 


rm(GC.Data.NoSTD.2021.add.2)





### Sampling.Date == 2021-07-07  ; Sample.Name == B4CloverBT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T3 Syringe Stuck" ;
  
  
  
  
### Sampling.Date == 2021-07-07  ; Sample.Name == B4CloverCT45  and B4CloverC30 ; T45 is T30 and T30 is T45

# Selecting set with the conditions #


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C" , ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C" & (GC.Data.NoSTD.2021$Sampling.Time == 30 | 
                      
                      GC.Data.NoSTD.2021$Sampling.Time == 45),]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C" & (GC.Data.NoSTD.2021$Sampling.Time == 30 | 
                                                               
                                                               GC.Data.NoSTD.2021$Sampling.Time == 45),
                    
                    "Field.Notes" ] <- "YES" ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-07")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C" & (GC.Data.NoSTD.2021$Sampling.Time == 30 | 
                                                               
                                                               GC.Data.NoSTD.2021$Sampling.Time == 45),
                    
                    "Note" ]  <- "T45 is T30 and T30 is T45" ;                                                          
                                                             




### Sampling.Date == 2021-07-15  ; Sample.Name == B13SppB ;

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-15")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B" , ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-15")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B" , "Field.Notes" ] <- "YES"   ;



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-15")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B", "Note" ] <- "T15 is bad, T30 is T15, T45 and T0 Fine" ; 




### Sampling.Date == 2021-07-15  ; Sample.Name == B4CloverB ;

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-15")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B" , ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-15")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B", "Field.Notes" ] <- "YES"   ;



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-15")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B", "Note" ] <- "did not run at 11:57; It was ran again at 3:00 pm" ;




### Sampling.Date == 2021-07-20  ; Sample.Name == B4TritAT15 ;

# Selecting set with the conditions



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 plunger" ;




### Sampling.Date == 2021-07-20  ; Sample.Name == B4TritBT45 and B4TritBT15  ;

# Selecting set with the conditions



#### B4TritBT15 ####

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 is T45" ;


#### B4TritBT45 ####


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-20")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T15 is T45" ;



### Sampling.Date == 2021-07-30  ; Sample.Name == B13SppAT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]





##???????????????????????????????????????????????????????????????????????????????????????????????????????????????

##### Samples  B13SppA were measured two different times in the GC  and Blocks 3 and 4 are missing ????????#####

##???????????????????????????????????????????????????????????????????????????????????????????????????????????????



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30"),] 

tail(GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30"),] )


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                      
                      GC.Data.NoSTD.2021$File == "20210730B1B2peakareas.pdf", ]

Data.20210730.B1B2 <- GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                      
                      GC.Data.NoSTD.2021$File == "20210730B1B2peakareas.pdf", ] ;


tail(Data.20210730.B1B2[ order(Data.20210730.B1B2$BLOCK, Data.20210730.B1B2$CoverCrop , 
                               
                               Data.20210730.B1B2$Treatment , Data.20210730.B1B2$Sampling.Time ),],25)



#### Data in the file 20210730B1B2peakareas.pdf is OK #######

#### Data from the files 20210730B3B4peakareasP1.pdf , 20210730B3B4peakareasP2.pdf and 20210730B3B4peakareasP3.pdf

### has the GC Sample Name mislabeled. Instead of being B1.... B2... ist should be B3.... and B4.... respectively.

### to solve that problem, the Block numbers in the files 20210730...P1, P2, P3 will be relabeled with B3... and B4....


Data.20210730.B3B4.P1 <- GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                                            
                                            GC.Data.NoSTD.2021$File == "20210730B3B4peakareasP1.pdf", ] ;


#### Data in 20210730B3B4peakareasP1.pdf is correctly labeled; it does not need to be corrected ###


### Collecting the data from  "20210730B3B4peakareasP2.pdf"  ###


Data.20210730.B3B4.P2 <- GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                                            
                                            GC.Data.NoSTD.2021$File == "20210730B3B4peakareasP2.pdf", ] ;



Data.20210730.B3B4.P2[Data.20210730.B3B4.P2$BLOCK == "1" , "BLOCK"] <- "3" ;

#### Check ###

Data.20210730.B3B4.P2[Data.20210730.B3B4.P2$BLOCK == "1" , "BLOCK"]

Data.20210730.B3B4.P2[Data.20210730.B3B4.P2$BLOCK == "3" , "BLOCK"]

### Correcting the Sample.Name  ###

Data.20210730.B3B4.P2[, "Sample.Name"] 

paste0( "B" ,Data.20210730.B3B4.P2$BLOCK , Data.20210730.B3B4.P2$CoverCrop , 
        
        Data.20210730.B3B4.P2$Treatment , "T" ,Data.20210730.B3B4.P2$Sampling.Time )

Data.20210730.B3B4.P2[, "Sample.Name"] <- paste0( "B" ,Data.20210730.B3B4.P2$BLOCK , Data.20210730.B3B4.P2$CoverCrop , 
                                                  
                                                  Data.20210730.B3B4.P2$Treatment , "T" ,Data.20210730.B3B4.P2$Sampling.Time );

Data.20210730.B3B4.P2



### Collecting the data from  "20210730B3B4peakareasP3.pdf"  ###

Data.20210730.B3B4.P3 <- GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                                               
                                               GC.Data.NoSTD.2021$File == "20210730B3B4peakareasP3.pdf", ] ;


Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "1" , "BLOCK"] 

Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "1" , "BLOCK"] <- "3" ;

#### Check ###

Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "3" , "BLOCK"] 

Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "2" , "BLOCK"] 

Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "2" , "BLOCK"] <- "4" ;

#### Check ###

Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "2" , "BLOCK"]

Data.20210730.B3B4.P3[Data.20210730.B3B4.P3$BLOCK == "4" , "BLOCK"]


### Correcting the Sample.Name  ###

Data.20210730.B3B4.P3[, "Sample.Name"] 

paste0( "B" ,Data.20210730.B3B4.P3$BLOCK , Data.20210730.B3B4.P3$CoverCrop , 
        
        Data.20210730.B3B4.P3$Treatment , "T" ,Data.20210730.B3B4.P3$Sampling.Time )

Data.20210730.B3B4.P3[, "Sample.Name"] <- paste0( "B" ,Data.20210730.B3B4.P3$BLOCK , Data.20210730.B3B4.P3$CoverCrop , 
                                                  
                                                  Data.20210730.B3B4.P3$Treatment , "T" ,Data.20210730.B3B4.P3$Sampling.Time ) ;

Data.20210730.B3B4.P3



### Adding the Corrected data from Data.20210730.B3B4.P2 to the data frame GC.Data.NoSTD.2021 ###

GC.Data.NoSTD.2021.4 <- GC.Data.NoSTD.2021 ;


GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                      
                      GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP2.pdf", ]



str(GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                            
                            GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP2.pdf", ])



GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                        
                        GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP2.pdf",  c("Sample.Name" , "BLOCK")]

str(Data.20210730.B3B4.P2)


GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                        
                        GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP2.pdf",  
                      
                      c("Sample.Name" , "BLOCK")] <- Data.20210730.B3B4.P2[,c("Sample.Name" , "BLOCK")] ;
  


### Adding the Corrected data from Data.20210730.B3B4.P3 to the data frame GC.Data.NoSTD.2021 ###

GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                        
                        GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP3.pdf", ]

str(GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                            
                            GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP3.pdf", ])

str(Data.20210730.B3B4.P3)


GC.Data.NoSTD.2021.4[ GC.Data.NoSTD.2021.4$Sampling.Date == paste0("2021","-07-30") & 
                        
                        GC.Data.NoSTD.2021.4$File == "20210730B3B4peakareasP3.pdf",  
                      
                      c("Sample.Name" , "BLOCK")] <- Data.20210730.B3B4.P3[,c("Sample.Name" , "BLOCK")] ;


GC.Data.NoSTD.2021 <-GC.Data.NoSTD.2021.4 ;

rm(GC.Data.NoSTD.2021.4)

### Check ###


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                       
                       GC.Data.NoSTD.2021$File == "20210730B3B4peakareasP2.pdf",  c("Sample.Name" , "BLOCK")]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30") & 
                      
                      GC.Data.NoSTD.2021$File == "20210730B3B4peakareasP3.pdf",  c("Sample.Name" , "BLOCK")]





### Re doing Sampling.Date == 2021-07-30  ; Sample.Name == B13SppAT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ] 




GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-07-30")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T45 Syringe did not open" ;








### Sampling.Date == 2021-08-05  ; Sample.Name == B1ClovCT30  and  B1ClovCT15;

# Selecting set with the conditions


#### B1ClovCT30 ###

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "No T30 put T30 into T 15" ;

#### B1ClovCT15 ####



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "No T30 put T30 into T 15" ;





### Sampling.Date == 2021-08-05  ; Sample.Name == B1ClovDT15

# Selecting set with the conditions



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "No T 15" ;






### Sampling.Date == 2021-08-05  ; Sample.Name == B23SppA

# Selecting set with the conditions



GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A", ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A",
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A",
                    
                    "Note" ] <- "Vials not well evacuated"  ;




### Sampling.Date == 2021-08-05  ; Sample.Name == B23SppBT30 and B23SppBT345

# Selecting set with the conditions

#####  B23SppBT30 #####

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 and T45 are interchanged" ;


#####  B23SppBT45 #####

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-05")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T30 and T45 are interchanged" ;






### Sampling.Date == 2021-08-12  ; Sample.Name == B3TritBT30 

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-12")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-12")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-12")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "3" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "No T 30, Syringe did not work" ;




### Sampling.Date == 2021-08-12  ; Sample.Name == B4TritCT15 

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-12")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-12")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-12")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T 15 no Sample, Syringe did not work" ;




### Sampling.Date == 2021-08-19  ; Sample.Name == B4ClovAT30 

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-19")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-19")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-08-19")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "4" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "Syringe 30 in did not take the full 30 ml" ;
  
  


### Sampling.Date == 2021-09-02  ; Sample.Name == B1TritCT15  

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, 
                    
                    "Note" ] <- "T15 Syringe did not go" ;
  
  
  
  



### Sampling.Date == 2021-09-02  ; Sample.Name == B2ClovAT15  

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "A"  &   GC.Data.NoSTD.2021$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 only took 15 ml"  ;
  
  
  
### Sampling.Date == 2021-09-02  ; Sample.Name == B2ClovCT45  

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T 45 Syringe did not go" ;  
  


### Sampling.Date == 2021-09-02  ; Sample.Name ==  B2ClovDT30  and B2ClovDT45  

# Selecting set with the conditions


###### B2ClovDT30 ######

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 30, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 and T45 are exchanged; T30 is T45 and T45 is T 30" ;
  
  
###### B2ClovDT45 ######

GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-02")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "2" & GC.Data.NoSTD.2021$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2021$Treatment == "D"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T30 and T45 are exchanged; T30 is T45 and T45 is T 30" ;

  
  
  

### Sampling.Date == 2021-09-17  ; Sample.Name ==  B1TritCT45

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-17")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-17")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-17")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2021$Treatment == "C"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T 45 no Sample" ;
  
  



### Sampling.Date == 2021-09-17  ; Sample.Name ==  B13SppBT45

# Selecting set with the conditions


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-17")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45, ]


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-17")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2021[ GC.Data.NoSTD.2021$Sampling.Date == paste0("2021","-09-17")  & 
                      
                      GC.Data.NoSTD.2021$BLOCK == "1" & GC.Data.NoSTD.2021$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2021$Treatment == "B"  &   GC.Data.NoSTD.2021$Sampling.Time == 45,
                    
                    "Note" ] <- "T 45 no Sample"  ;
   
  
  

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






###############################################################################################################
#                           
#                               Corrections based on Field measurements notes 
#
###############################################################################################################

Block 1, Clover, D,










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


 
  