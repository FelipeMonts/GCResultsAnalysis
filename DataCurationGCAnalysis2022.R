##############################################################################################################
# 
# 
# Program to collect data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused on curating measurements on year 2022
# 
# 
#  Felipe Montes 2026/10/22
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


PeakArea.results.2022 <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults_2022_2026_10_23_11_06_11.csv" ) , header = T) ;

###############################################################################################################
#                           
#                              Checking for duplicated records
#
###############################################################################################################

str(PeakArea.results.2022)

head(PeakArea.results.2022)

names(PeakArea.results.2022)

anyDuplicated(PeakArea.results.2022, MARGIN = c(1,2))

anyDuplicated(PeakArea.results.2022, MARGIN = 0)




# ###############################  Remove duplicates  ######################################################
# 
# PeakArea.results.2022.1 <- PeakArea.results.2022[!duplicated(PeakArea.results.2022, MARGIN = c(1,2)),] ;
# 
# str(PeakArea.results.2022.1)
# 
# 
# anyDuplicated(PeakArea.results.2022.1, MARGIN = c(1,2))
# 
# PeakArea.results.2022 <- PeakArea.results.2022.1  ;
# 
# str(PeakArea.results.2022) 
# 
# rm(PeakArea.results.2022.1)
# 
# 
# #################################################################################################################
#                           
#                              Check for repeated measurements 
#
###############################################################################################################

names(PeakArea.results.2022)

anyDuplicated(PeakArea.results.2022[,c(4,5,6)], MARGIN = c(1,2))

anyDuplicated(PeakArea.results.2022[,c(4,5,6)], MARGIN = 0)





#################################################################################################################
#                           
#                              Removing standards data
#
###############################################################################################################


GC.Data.NoSTD.2022 <- PeakArea.results.2022[grep( pattern = "B" , x = PeakArea.results.2022$Sample.Name, invert = F) ,] ;

#### Check ####

PeakArea.results.2022[which(PeakArea.results.2022$Sample.Name %in% GC.Data.NoSTD.2022$Sample.Name), "Sample.Name" ]

PeakArea.results.2022[which(!PeakArea.results.2022$Sample.Name %in% GC.Data.NoSTD.2022$Sample.Name), "Sample.Name" ]




##### Data with no standards included

str(GC.Data.NoSTD.2022)



##############################################################################################################
#                           
#                              Converting to factors the columns that have discrete values
#                              
#                              
#
##############################################################################################################



### Add treatment information ###


unique(GC.Data.NoSTD.2022$Sample.Name)

GC.Data.NoSTD.2022$Treatment<-c("NONE");


GC.Data.NoSTD.2022[grep("AT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T), c("Treatment")]<-c("A");

GC.Data.NoSTD.2022[grep("BT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T), c("Treatment")]<-c("B");

GC.Data.NoSTD.2022[grep("CT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T) , c("Treatment")]<-c("C");

GC.Data.NoSTD.2022[grep("DT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T) , c("Treatment")]<-c("D");

### Check if there was any treatment left with "NONE" label

GC.Data.NoSTD.2022[which(GC.Data.NoSTD.2022$Treatment == "NONE"), ];

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$Sample.Name == "B4TritCT30",] 

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$Sample.Name == "B4TritCT30", c("Treatment")]


### convert to a factor  ####

GC.Data.NoSTD.2022$Treatment <- factor(GC.Data.NoSTD.2022$Treatment) ;


levels(GC.Data.NoSTD.2022$Treatment)

str(GC.Data.NoSTD.2022)


###  Add Block information ####


grep("B1",GC.Data.NoSTD.2022$Sample.Name)

GC.Data.NoSTD.2022$BLOCK<-c(9999);

GC.Data.NoSTD.2022[grep("B1",GC.Data.NoSTD.2022$Sample.Name), c("BLOCK")] <- c(1);

GC.Data.NoSTD.2022[grep("B2",GC.Data.NoSTD.2022$Sample.Name), c("BLOCK")] <- c(2);

GC.Data.NoSTD.2022[grep("B3",GC.Data.NoSTD.2022$Sample.Name), c("BLOCK")] <- c(3);

GC.Data.NoSTD.2022[grep("B4",GC.Data.NoSTD.2022$Sample.Name), c("BLOCK")] <- c(4);

### Check if there was any BLOCK labeled 9999

GC.Data.NoSTD.2022[which(GC.Data.NoSTD.2022$BLOCK == 9999 ), ];

### convert to a factor  ####

GC.Data.NoSTD.2022$BLOCK <- factor(GC.Data.NoSTD.2022$BLOCK) ;

levels(GC.Data.NoSTD.2022$BLOCK)

str(GC.Data.NoSTD.2022)




### Adding CoverCrop Information ####


grep("3Spp",GC.Data.NoSTD.2022$Sample.Name)

GC.Data.NoSTD.2022$CoverCrop <- c("NONE");

GC.Data.NoSTD.2022[grep("3Spp",GC.Data.NoSTD.2022$Sample.Name), c("CoverCrop")] <- c("3Spp");

GC.Data.NoSTD.2022[grep("Clover",GC.Data.NoSTD.2022$Sample.Name), c("CoverCrop")] <- c("Clover");

GC.Data.NoSTD.2022[grep("Trit",GC.Data.NoSTD.2022$Sample.Name), c("CoverCrop")] <- c("Trit");


### Check if there was any  CoverCrop labeled "NONE"

GC.Data.NoSTD.2022[which(GC.Data.NoSTD.2022$CoverCrop == "NONE" ), ];


### convert to a factor  ####

GC.Data.NoSTD.2022$CoverCrop <- factor(GC.Data.NoSTD.2022$CoverCrop) ;

levels(GC.Data.NoSTD.2022$CoverCrop)

str(GC.Data.NoSTD.2022)



### Adding Sampling Time information  ###

grep("T0",GC.Data.NoSTD.2022$Sample.Name)

GC.Data.NoSTD.2022$Sampling.Time <- c(9999);

GC.Data.NoSTD.2022[grep("T0",GC.Data.NoSTD.2022$Sample.Name), c("Sampling.Time")] <- c(0);

GC.Data.NoSTD.2022[grep("T15",GC.Data.NoSTD.2022$Sample.Name), c("Sampling.Time")] <- c(15);

GC.Data.NoSTD.2022[grep("T30",GC.Data.NoSTD.2022$Sample.Name), c("Sampling.Time")] <- c(30);

GC.Data.NoSTD.2022[grep("T45",GC.Data.NoSTD.2022$Sample.Name), c("Sampling.Time")] <- c(45);


### Check if there was any Sampling.Time left with "NONE" label

GC.Data.NoSTD.2022[which(GC.Data.NoSTD.2022$Sampling.Time==9999),];

str(GC.Data.NoSTD.2022)


#### Adding plot  and location information  ###


### Plot 305 ###

GC.Data.NoSTD.2022$Plot <- NA ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 
                   

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" ,  "Plot"] <- "305"  ;


GC.Data.NoSTD.2022$Location <- NA ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "305" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "305" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "305" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 



### Plot 306 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp", "Plot"] <- "306"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "306" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "306" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "306" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 


### Plot 301 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover", "Plot"] <- "301"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "301" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "301" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "301" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "D" &  GC.Data.NoSTD.2022$Plot == "301" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 





### Plot 504 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit", "Plot"] <- "504"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "504" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "504" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "504" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 




### Plot 506 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp", "Plot"] <- "506"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "506" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "506" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "506" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 




### Plot 512 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover", "Plot"] <- "512"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "512" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "512" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "512" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "D" &  GC.Data.NoSTD.2022$Plot == "512" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 







### Plot 803 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit", "Plot"] <- "803"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "803" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "803" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "803" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 





### Plot 812 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp", "Plot"] <- "812"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "812" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "812" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "812" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 




### Plot 808 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover", "Plot"] <- "808"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "808" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "808" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "808" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "D" &  GC.Data.NoSTD.2022$Plot == "808" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 



### Plot 1002 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit", "Plot"] <- "1002"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "1002" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "1002" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "1002" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ]





### Plot 1009 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp", "Plot"] <- "1009"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "1009" , 
                   
                   "Location"] <- "Middle" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "1009" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "1009" , 
                   
                   "Location"] <- "Border" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ] 






### Plot 1003 ###

#  levels(GC.Data.NoSTD.2022$CoverCrop)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ] 


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover", "Plot"] <- "1003"  ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "A" &  GC.Data.NoSTD.2022$Plot == "1003" , 
                   
                   "Location"] <- "Middle"

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "1003" , 
                   
                   "Location"] <- "Border" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "1003" , 
                   
                   "Location"] <- "Inside" ;

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "D" &  GC.Data.NoSTD.2022$Plot == "1003" , 
                   
                   "Location"] <- "Inside" ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ]


####   Do a Check #####

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$Plot)), ]   ;

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$Location)), ]   ;


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" , ]

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" , ]

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" , ]



GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "D" &  GC.Data.NoSTD.2022$Plot == "1003" , 
                   
                   "Location"]

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "D" &  GC.Data.NoSTD.2022$Plot == "305" , 
                   
                   "Location"]

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "B" &  GC.Data.NoSTD.2022$Plot == "305" , 
                   
                   "Location"]


GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                     
                     GC.Data.NoSTD.2022$Treatment == "C" &  GC.Data.NoSTD.2022$Plot == "812" , 
                   
                   "Location"]

#####  Converting Plot and Location to factors  ####

GC.Data.NoSTD.2022$Plot <- factor(GC.Data.NoSTD.2022$Plot) ;

GC.Data.NoSTD.2022$Location <- factor(GC.Data.NoSTD.2022$Location) ;



##############################################################################################################
#                           
#                              Adding field sampling notes to the data
#                              
#                              Making adjustments when appropriate according to the notes
#
##############################################################################################################

GC.Data.NoSTD.2022$Field.Notes <- "NO" ;

GC.Data.NoSTD.2022$Note <- NA ;
  


str(GC.Data.NoSTD.2022)
  
#### Adding Notes #####


### Sampling.Date == 2022-06-09  ; Sample.Name == B3TritA ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , "Field.Notes" ] <- "YES"  ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A", "Note" ] <- "Leakage in all Ts"  ;


### Sampling.Date == 2022-06-09  ; Sample.Name == B3TritB ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , "Field.Notes" ] <- "YES"  ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B", "Note" ] <- "Leakage in all Ts"  ;



### Sampling.Date == 2022-06-09  ; Sample.Name == B3ClovAT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Note" ] <- "Leaking and not well evacuated T30" ;   


### Sampling.Date == 2022-06-09  ; Sample.Name == B3ClovBT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Note" ] <- "Leaking and not well evacuated T30" ;  


### Sampling.Date == 2022-06-09  ; Sample.Name == B3ClovBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, 
                    
                    "Note" ] <- "Leaking and not well evacuated T45" ; 



### Sampling.Date == 2022-06-09  ; Sample.Name == B3ClovCT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, 
                    
                    "Note" ] <- "Leaking and not well evacuated T45" ; 



### Sampling.Date == 2022-06-09  ; Sample.Name == B4Trit A, B, C ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit",
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit",
                    
                    "Note" ] <- "Vials not well evacuated"   ;



### Sampling.Date == 2022-06-09  ; Sample.Name == B43spp A ,B, C  T0;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" 
                    
                    & GC.Data.NoSTD.2022$Sampling.Time == 0 , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" 
                    
                    & GC.Data.NoSTD.2022$Sampling.Time == 0 ,  
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" 
                    
                    & GC.Data.NoSTD.2022$Sampling.Time == 0 , 
                    
                    "Note" ] <- "In one of these T-15 is T0 and T0 is T15 ?";   

### Sampling.Date == 2022-06-09  ; Sample.Name == B43spp A ,B, C  T15;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                    
                      GC.Data.NoSTD.2022$Sampling.Time == 15 , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp"  &
                    
                      GC.Data.NoSTD.2022$Sampling.Time == 15 ,  
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-09")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                    
                     GC.Data.NoSTD.2022$Sampling.Time == 0 , 
                    
                    "Note" ] <- "In one of these T-15 is T0 and T0 is T15 ?"; 





### Sampling.Date == 2022-06-15  ; Sample.Name == B4ClovT30 ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "Problem with Vials"; 


### Sampling.Date == 2022-06-15  ; Sample.Name == B4ClovCT45 ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "Problem with Vials"; 


### Sampling.Date == 2022-06-23  ; Sample.Name == B2TritCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Note" ] <- "T30 Syringe broke and did not open" ;



### Sampling.Date == 2022-07-07  ; Sample.Name == B2TritBT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ; 
                    
                   

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 is T45 and T45 is T15" ;


### Sampling.Date == 2022-07-07  ; Sample.Name == B2TritBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ; 



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T15 is T45 and T45 is T15" ;



### Sampling.Date == 2022-07-07  ; Sample.Name == B3 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3",
                    
                    "Field.Notes" ] <- "YES"   ; 



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3",
                    
                    "Note" ] <- "B3 is B4 sampled the next day" ;



### Sampling.Date == 2022-07-07  ; Sample.Name == B4 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4",
                    
                    "Field.Notes" ] <- "YES"   ; 



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4",
                    
                    "Note" ] <- "B4 is B34 sampled the next day" ;



### Sampling.Date == 2022-07-07  ; Sample.Name == B43sppA ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "B3 is B4 sampled the next day and T15 is no good. it is from B3 " ;
  

### Sampling.Date == 2022-07-07  ; Sample.Name == B43sppB ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "B4 is B34 sampled the next day and T15 from B3 is on T15 from B4 " ;
                    


### Sampling.Date == 2022-07-07  ; Sample.Name == B43sppC ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "B4 is B34 sampled the next day and T15 from C has mixed sample" ;
                    




### Sampling.Date == 2022-07-19  ; Sample.Name == B43SppC ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C", 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C",
                      
                      "Note" ] <- "Amanda and Sarah were helping. Their note is 'Did not work'" ;



### Sampling.Date == 2022-07-19  ; Sample.Name == B4ClovAT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 was not vacuum" ;
  


### Sampling.Date == 2022-07-19  ; Sample.Name == B4ClovCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 was not vacuum" ;


### Sampling.Date == 2022-08-02  ; Sample.Name == B13SppA ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A", 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A",
                    
                    "Note" ] <- "A is C and C is A" ;



### Sampling.Date == 2022-08-02  ; Sample.Name == B13SppC ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C", 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C",
                    
                    "Note" ] <- "C is A and A is C" ;


### Sampling.Date == 2022-08-19  ; Sample.Name == All ;

# Selecting set with the conditions 

# View.All.GC.Data.NoSTD.2022 <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19"),]
# 
# 
# rm( View.All.GC.Data.NoSTD.2022)



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19"),] 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19"), "Field.Notes" ] <- "YES"; 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19"),  "Note" ] <- 
  
  "Soil wet only in the first 1 to 2 inches in all blocks" ;

                      
          

### Sampling.Date == 2022-08-19  ; Sample.Name == B1 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1", ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1",
                    
                    "Note" ] <- "Soil wet only in the first 1 to 2 inches in all blocks -  Sampled Next Day 08/19/22" ;



### Sampling.Date == 2022-08-19  ; Sample.Name == B2 , B3 , B4  

# Selecting set with the conditions 


# View.All.GC.Data.NoSTD.2022 <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  &
# 
#                      ( GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "3" |
# 
#                          GC.Data.NoSTD.2022$BLOCK == "4"), ]
# 
# rm( View.All.GC.Data.NoSTD.2022)


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  &
                      
                      ( GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "3" |
                          
                          GC.Data.NoSTD.2022$BLOCK == "4"), ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  &
                      
                      ( GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "3" |
                          
                          GC.Data.NoSTD.2022$BLOCK == "4"),
                    
                    "Note" ] <- "Soil wet only in the first 1 to 2 inches in all blocks - Sampled Day Before 08/18/22" ;



### Sampling.Date == 2022-08-19  ; Sample.Name == B2TritBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, 
                    
                    "Note" ] <- "Soil wet only in the first 1 to 2 inches in all blocks - Sampled Day Before 08/18/22 - T45 needle failed. Sample was Lost" ;



### Sampling.Date == 2022-09-01  ; Sample.Name == B2TritA ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , "Field.Notes" ] <- "YES";

                    
GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , "Note" ] <- "Sampled next Day" ;                
                    
                    

### Sampling.Date == 2022-09-01  ; Sample.Name == B2TritC ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Note" ] <- "Sampled next Day" ;  


                    
                    
### Sampling.Date == 2022-09-01  ; Sample.Name == B2CloverD ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D" , "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D" , "Note" ] <- "Sampled next Day" ;                      




### Sampling.Date == 2022-09-01  ; Sample.Name == B43SppA ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A" , "Note" ] <- "Sampled next Day" ;  


### Sampling.Date == 2022-09-01  ; Sample.Name == B43SppC ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Note" ] <- "Sampled next Day" ;  




### Sampling.Date == 2022-09-01  ; Sample.Name == B43CloverD ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D" , "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-01")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D" , "Note" ] <- "Sampled next Day" ; 


### Sampling.Date == 2022-09-10  ; Sample.Name == B2 ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", "Note" ] <- "Sampled 2022 09 08, two days before" ;



### Sampling.Date == 2022-09-10  ; Sample.Name == B2 ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", "Note" ] <- "Sampled one day before 2022-09-09" ;



### Sampling.Date == 2022-09-10  ; Sample.Name == B2ClovC ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Field.Notes" ] <- "YES";



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Note" ] <- "Notice leak between the frame and the chamber";




### Sampling.Date == 2022-09-10  ; Sample.Name == B4TritC ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-10")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T45 Syringe stucked, took Sample from the manifold" ;



### Sampling.Date == 2022-09-27  ; Sample.Name == B1, B2, B4 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                     (GC.Data.NoSTD.2022$BLOCK == "1" | GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "4"), ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      (GC.Data.NoSTD.2022$BLOCK == "1" | GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "4"), 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      (GC.Data.NoSTD.2022$BLOCK == "1" | GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "4"),
                    
                    "Note" ] <- "Sampled the next day 2022-09-28" ;


### check ####

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", ] 



### Sampling.Date == 2022-10-05  ; Sample.Name == B2, B4 ;

# Selecting set with the conditions 

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      ( GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "4"), ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      ( GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "4"),
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      ( GC.Data.NoSTD.2022$BLOCK == "2" | GC.Data.NoSTD.2022$BLOCK == "4"),
                    
                    "Note" ] <- "Sampled the next day 2022-10-06" ;
                    


### Sampling.Date == 2022-10-05  ; Sample.Name == B3TritCT30

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                    
                      GC.Data.NoSTD.2022$Treatment == "C" & GC.Data.NoSTD.2022$Sampling.Time == 30 , ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & GC.Data.NoSTD.2022$Sampling.Time == 30 , 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & GC.Data.NoSTD.2022$Sampling.Time == 30 , 
                    
                    "Note" ] <- " T2 Solenoid is Failing" ;




### Sampling.Date == 2022-10-05  ; Sample.Name == B23SppAT30

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "A" & GC.Data.NoSTD.2022$Sampling.Time == 30 , ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "A" & GC.Data.NoSTD.2022$Sampling.Time == 30 , 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-10-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "A" & GC.Data.NoSTD.2022$Sampling.Time == 30 ,
                    
                    "Note" ] <- " Sampled the next day 2022-10-06 --  T2 Solenoid is Failing" ;






# There are no samples with both  Sampling date "2022-06-04" and B1CloverB  because the glass
# Sampling vials broke

# Add NA data to those broken vials records


Comment.06_04.B1CloverD <- GC.Data.NoSTD.2022[0,] ;

Comment.06_04.B1CloverD[c(1:4),] <- NA ;

Comment.06_04.B1CloverD$Sample.Name <- paste0("B1CloverD",c("T0" , "T15", "T30" , "T45"));

Comment.06_04.B1CloverD$Sampling.Date <- "2022-06-04" ;

Comment.06_04.B1CloverD$Sampling.Day <- "20220604" ;

Comment.06_04.B1CloverD$Field.Notes <- "YES" ;

Comment.06_04.B1CloverD$Note <- "Broken glass vials no samples" ;

Comment.06_04.B1CloverD


#  combining the two data frames Comment.06_04.B1CloverB.B1CloverD  and Comment.06_04.B1CloverB  #


Comment.06_04.B1CloverB.B1CloverD <- rbind(Comment.06_04.B1CloverB , Comment.06_04.B1CloverD )

Comment.06_04.B1CloverB.B1CloverD


# Adding to the data  

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$Sample.Name == "B1CloverD" &  GC.Data.NoSTD.2022$Sampling.Date == "2022-06-04",]


GC.Data.NoSTD.2022.add <- rbind(GC.Data.NoSTD.2022 , Comment.06_04.B1CloverB.B1CloverD) ;

GC.Data.NoSTD.2022 <- GC.Data.NoSTD.2022.add ;

GC.Data.NoSTD.2022.add[GC.Data.NoSTD.2022.add$Sample.Name == "B1CloverDT0" &  GC.Data.NoSTD.2022.add$Sampling.Date == "2022-06-04",]

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$Sample.Name == "B1CloverDT0" &  GC.Data.NoSTD.2022$Sampling.Date == "2022-06-04",]

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$Sample.Name == "B1CloverBT0" &  GC.Data.NoSTD.2022$Sampling.Date == "2022-06-04",]



rm(GC.Data.NoSTD.2022.add)

### Sampling.Date == 2022-06-21  ; Sample.Name == B3TriCT30 ;

# Selecting set with the conditions 



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-21")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-21")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-21")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "Syringe stock open when taking vial" ;




### Sampling.Date == 2022-06-23  ; Sample.Name == B1ClovAT15 ;

# Selecting set with the conditions 



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "Sample not fully drawn" ;



### Sampling.Date == 2022-06-23  ; Sample.Name == B33SppBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-23")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "* T45 ?"   ;



### Sampling.Date == 2022-06-29  ; Sample.Name == B1TritBT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 Broke" ;


### Sampling.Date == 2022-06-29  ; Sample.Name == B1ClovBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "Needle Broke" ;






### Sampling.Date == 2022-06-29  ; Sample.Name == B1ClovBT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "No T 45" ;




### Sampling.Date == 2022-06-29  ; Sample.Name == B2ClovBT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "No T 15" ;



### Sampling.Date == 2022-06-29  ; Sample.Name == B3ClovAT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, 
                    
                    "Note" ] <- "No T3 Syringe Broke" ;



### Sampling.Date == 2022-06-29  ; Sample.Name == B3ClovCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, 
                    
                    "Note" ] <- "T2 Needle Broke" ;




### Sampling.Date == 2022-06-29  ; Sample.Name == B4TritCT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 Vial not fully evacuated" ;



### Sampling.Date == 2022-06-29  ; Sample.Name == B43SppCT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "T45 no sample" ;



### Sampling.Date == 2022-07-02  ; Sample.Name == B1CloverCT15 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 no Sample Syringe broke" ;






### Sampling.Date == 2022-07-02  ; Sample.Name == B3TritAT45 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- " T45 was leaking"  ; 



### Sampling.Date == 2022-07-02  ; Sample.Name == B3TritBT30 ;

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "T2 did not fully draw sample" ;




### Sampling.Date == 2022-07-02  ; Sample.Name == B33SppBT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T3 valve did not open or Syringe Stuck"  ; 
   
  
  
  
  
  
### Sampling.Date == 2022-07-07  ; All of block 1 was not Sampled ;

# Selecting set with the conditions 
  
GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" , ]  

### Add rows with NA for the missing records. The records of Block 1 that was not measured

## start with a template from 2022-07-02


Data.210707.Missing <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-02")  & 
                                             
                                             GC.Data.NoSTD.2022$BLOCK == "1" , ] ;

names(Data.210707.Missing)


Data.210707.Missing[ , c("Position" , "Vial", "CH4" , "CO2" , "N2O" , "File", "GC.Date"  )] <- NA ;


Data.210707.Missing$Sampling.Day <- 20220707 ;


Data.210707.Missing$Sampling.Date <- "2022-07-07" ;


Data.210707.Missing$Field.Notes <- "YES"  ;


Data.210707.Missing$Note <- "Block 1 was not sampled on 2022-07-07" ;

str(Data.210707.Missing)


GC.Data.NoSTD.2022.add.2 <- rbind(GC.Data.NoSTD.2022 , Data.210707.Missing ) ;

str(GC.Data.NoSTD.2022.add.2)


GC.Data.NoSTD.2022 <-  GC.Data.NoSTD.2022.add.2 ;

str(GC.Data.NoSTD.2022)


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" , ] 


rm(GC.Data.NoSTD.2022.add.2)





### Sampling.Date == 2022-07-07  ; Sample.Name == B4CloverBT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T3 Syringe Stuck" ;
  
  
  
  
### Sampling.Date == 2022-07-07  ; Sample.Name == B4CloverCT45  and B4CloverC30 ; T45 is T30 and T30 is T45

# Selecting set with the conditions #


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & (GC.Data.NoSTD.2022$Sampling.Time == 30 | 
                      
                      GC.Data.NoSTD.2022$Sampling.Time == 45),]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & (GC.Data.NoSTD.2022$Sampling.Time == 30 | 
                                                               
                                                               GC.Data.NoSTD.2022$Sampling.Time == 45),
                    
                    "Field.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-07")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & (GC.Data.NoSTD.2022$Sampling.Time == 30 | 
                                                               
                                                               GC.Data.NoSTD.2022$Sampling.Time == 45),
                    
                    "Note" ]  <- "T45 is T30 and T30 is T45" ;                                                          
                                                             




### Sampling.Date == 2022-07-15  ; Sample.Name == B13SppB ;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , "Field.Notes" ] <- "YES"   ;



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B", "Note" ] <- "T15 is bad, T30 is T15, T45 and T0 Fine" ; 




### Sampling.Date == 2022-07-15  ; Sample.Name == B4CloverB ;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B", "Field.Notes" ] <- "YES"   ;



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B", "Note" ] <- "did not run at 11:57; It was ran again at 3:00 pm" ;




### Sampling.Date == 2022-07-20  ; Sample.Name == B4TritAT15 ;

# Selecting set with the conditions



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 plunger" ;




### Sampling.Date == 2022-07-20  ; Sample.Name == B4TritBT45 and B4TritBT15  ;

# Selecting set with the conditions



#### B4TritBT15 ####

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 is T45" ;


#### B4TritBT45 ####


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-20")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T15 is T45" ;



### Sampling.Date == 2022-07-30  ; Sample.Name == B13SppAT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]





##???????????????????????????????????????????????????????????????????????????????????????????????????????????????

##### Samples  B13SppA were measured two different times in the GC  and Blocks 3 and 4 are missing ????????#####

##???????????????????????????????????????????????????????????????????????????????????????????????????????????????



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30"),] 

tail(GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30"),] )


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                      
                      GC.Data.NoSTD.2022$File == "20220730B1B2peakareas.pdf", ]

Data.20220730.B1B2 <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                      
                      GC.Data.NoSTD.2022$File == "20220730B1B2peakareas.pdf", ] ;


tail(Data.20220730.B1B2[ order(Data.20220730.B1B2$BLOCK, Data.20220730.B1B2$CoverCrop , 
                               
                               Data.20220730.B1B2$Treatment , Data.20220730.B1B2$Sampling.Time ),],25)



#### Data in the file 20220730B1B2peakareas.pdf is OK #######

#### Data from the files 20220730B3B4peakareasP1.pdf , 20220730B3B4peakareasP2.pdf and 20220730B3B4peakareasP3.pdf

### has the GC Sample Name mislabeled. Instead of being B1.... B2... ist should be B3.... and B4.... respectively.

### to solve that problem, the Block numbers in the files 20220730...P1, P2, P3 will be relabeled with B3... and B4....


Data.20220730.B3B4.P1 <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                                            
                                            GC.Data.NoSTD.2022$File == "20220730B3B4peakareasP1.pdf", ] ;


#### Data in 20220730B3B4peakareasP1.pdf is correctly labeled; it does not need to be corrected ###


### Collecting the data from  "20220730B3B4peakareasP2.pdf"  ###


Data.20220730.B3B4.P2 <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                                            
                                            GC.Data.NoSTD.2022$File == "20220730B3B4peakareasP2.pdf", ] ;



Data.20220730.B3B4.P2[Data.20220730.B3B4.P2$BLOCK == "1" , "BLOCK"] <- "3" ;

#### Check ###

Data.20220730.B3B4.P2[Data.20220730.B3B4.P2$BLOCK == "1" , "BLOCK"]

Data.20220730.B3B4.P2[Data.20220730.B3B4.P2$BLOCK == "3" , "BLOCK"]

### Correcting the Sample.Name  ###

Data.20220730.B3B4.P2[, "Sample.Name"] 

paste0( "B" ,Data.20220730.B3B4.P2$BLOCK , Data.20220730.B3B4.P2$CoverCrop , 
        
        Data.20220730.B3B4.P2$Treatment , "T" ,Data.20220730.B3B4.P2$Sampling.Time )

Data.20220730.B3B4.P2[, "Sample.Name"] <- paste0( "B" ,Data.20220730.B3B4.P2$BLOCK , Data.20220730.B3B4.P2$CoverCrop , 
                                                  
                                                  Data.20220730.B3B4.P2$Treatment , "T" ,Data.20220730.B3B4.P2$Sampling.Time );

Data.20220730.B3B4.P2



### Collecting the data from  "20220730B3B4peakareasP3.pdf"  ###

Data.20220730.B3B4.P3 <- GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                                               
                                               GC.Data.NoSTD.2022$File == "20220730B3B4peakareasP3.pdf", ] ;


Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "1" , "BLOCK"] 

Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "1" , "BLOCK"] <- "3" ;

#### Check ###

Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "3" , "BLOCK"] 

Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "2" , "BLOCK"] 

Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "2" , "BLOCK"] <- "4" ;

#### Check ###

Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "2" , "BLOCK"]

Data.20220730.B3B4.P3[Data.20220730.B3B4.P3$BLOCK == "4" , "BLOCK"]


### Correcting the Sample.Name  ###

Data.20220730.B3B4.P3[, "Sample.Name"] 

paste0( "B" ,Data.20220730.B3B4.P3$BLOCK , Data.20220730.B3B4.P3$CoverCrop , 
        
        Data.20220730.B3B4.P3$Treatment , "T" ,Data.20220730.B3B4.P3$Sampling.Time )

Data.20220730.B3B4.P3[, "Sample.Name"] <- paste0( "B" ,Data.20220730.B3B4.P3$BLOCK , Data.20220730.B3B4.P3$CoverCrop , 
                                                  
                                                  Data.20220730.B3B4.P3$Treatment , "T" ,Data.20220730.B3B4.P3$Sampling.Time ) ;

Data.20220730.B3B4.P3



### Adding the Corrected data from Data.20220730.B3B4.P2 to the data frame GC.Data.NoSTD.2022 ###

GC.Data.NoSTD.2022.4 <- GC.Data.NoSTD.2022 ;


GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                      
                      GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP2.pdf", ]



str(GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                            
                            GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP2.pdf", ])



GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                        
                        GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP2.pdf",  c("Sample.Name" , "BLOCK")]

str(Data.20220730.B3B4.P2)


GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                        
                        GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP2.pdf",  
                      
                      c("Sample.Name" , "BLOCK")] <- Data.20220730.B3B4.P2[,c("Sample.Name" , "BLOCK")] ;
  


### Adding the Corrected data from Data.20220730.B3B4.P3 to the data frame GC.Data.NoSTD.2022 ###

GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                        
                        GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP3.pdf", ]

str(GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                            
                            GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP3.pdf", ])

str(Data.20220730.B3B4.P3)


GC.Data.NoSTD.2022.4[ GC.Data.NoSTD.2022.4$Sampling.Date == paste0("2022","-07-30") & 
                        
                        GC.Data.NoSTD.2022.4$File == "20220730B3B4peakareasP3.pdf",  
                      
                      c("Sample.Name" , "BLOCK")] <- Data.20220730.B3B4.P3[,c("Sample.Name" , "BLOCK")] ;


GC.Data.NoSTD.2022 <-GC.Data.NoSTD.2022.4 ;

rm(GC.Data.NoSTD.2022.4)

### Check ###


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                       
                       GC.Data.NoSTD.2022$File == "20220730B3B4peakareasP2.pdf",  c("Sample.Name" , "BLOCK")]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30") & 
                      
                      GC.Data.NoSTD.2022$File == "20220730B3B4peakareasP3.pdf",  c("Sample.Name" , "BLOCK")]





### Re doing Sampling.Date == 2022-07-30  ; Sample.Name == B13SppAT45 ;

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ] 




GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-07-30")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T45 Syringe did not open" ;








### Sampling.Date == 2022-08-05  ; Sample.Name == B1ClovCT30  and  B1ClovCT15;

# Selecting set with the conditions


#### B1ClovCT30 ###

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "No T30 put T30 into T 15" ;

#### B1ClovCT15 ####



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "No T30 put T30 into T 15" ;





### Sampling.Date == 2022-08-05  ; Sample.Name == B1ClovDT15

# Selecting set with the conditions



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "No T 15" ;






### Sampling.Date == 2022-08-05  ; Sample.Name == B23SppA

# Selecting set with the conditions



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A",
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A",
                    
                    "Note" ] <- "Vials not well evacuated"  ;




### Sampling.Date == 2022-08-05  ; Sample.Name == B23SppBT30 and B23SppBT345

# Selecting set with the conditions

#####  B23SppBT30 #####

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 and T45 are interchanged" ;


#####  B23SppBT45 #####

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-05")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T30 and T45 are interchanged" ;






### Sampling.Date == 2022-08-12  ; Sample.Name == B3TritBT30 

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-12")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-12")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-12")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "No T 30, Syringe did not work" ;




### Sampling.Date == 2022-08-12  ; Sample.Name == B4TritCT15 

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-12")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-12")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-12")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T 15 no Sample, Syringe did not work" ;




### Sampling.Date == 2022-08-19  ; Sample.Name == B4ClovAT30 

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-08-19")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "Syringe 30 in did not take the full 30 ml" ;
  
  


### Sampling.Date == 2022-09-02  ; Sample.Name == B1TritCT15  

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, 
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, 
                    
                    "Note" ] <- "T15 Syringe did not go" ;
  
  
  
  



### Sampling.Date == 2022-09-02  ; Sample.Name == B2ClovAT15  

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "A"  &   GC.Data.NoSTD.2022$Sampling.Time == 15,
                    
                    "Note" ] <- "T15 only took 15 ml"  ;
  
  
  
### Sampling.Date == 2022-09-02  ; Sample.Name == B2ClovCT45  

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T 45 Syringe did not go" ;  
  


### Sampling.Date == 2022-09-02  ; Sample.Name ==  B2ClovDT30  and B2ClovDT45  

# Selecting set with the conditions


###### B2ClovDT30 ######

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 30, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 30,
                    
                    "Note" ] <- "T30 and T45 are exchanged; T30 is T45 and T45 is T 30" ;
  
  
###### B2ClovDT45 ######

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-02")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "Clover" &
                      
                      GC.Data.NoSTD.2022$Treatment == "D"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T30 and T45 are exchanged; T30 is T45 and T45 is T 30" ;

  
  
  

### Sampling.Date == 2022-09-17  ; Sample.Name ==  B1TritCT45

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-17")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-17")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-17")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "Trit" &
                      
                      GC.Data.NoSTD.2022$Treatment == "C"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T 45 no Sample" ;
  
  



### Sampling.Date == 2022-09-17  ; Sample.Name ==  B13SppBT45

# Selecting set with the conditions


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-17")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45, ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-17")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Field.Notes" ] <- "YES"   ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-17")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" &
                      
                      GC.Data.NoSTD.2022$Treatment == "B"  &   GC.Data.NoSTD.2022$Sampling.Time == 45,
                    
                    "Note" ] <- "T 45 no Sample"  ;
   
  
  

###############################################################################################################
#                          
#            Calculation of concentration based on the Standard gas concentrations
#             
#            The calibration curves were calculated with the StandardCalibration.R code
#  
#
###############################################################################################################


# ###############################################################################################################
# 
#                      Best calibration equation for CO2
# 
# ###############################################################################################################
# 
# 
# Call: rq(formula = CO2 ~ CO2.ppm, tau = 0.5, data = C.GC.Standards.All)
# 
# tau: [1] 0.5
# 
# Coefficients:
#   coefficients lower bd  upper bd 
# (Intercept) 797.91013    740.63252 845.75477
# CO2.ppm       3.30048      3.22693   3.47683
# 
# 
#  Calibration equation
# 
# CO2 = 798 + (3.3 * CO2.ppm)
# 
# CO2.ppm =(CO2-798)/3.3
# 
# 
# ###############################################################################################################



str(GC.Data.NoSTD.2022)

anyNA(GC.Data.NoSTD.2022$CO2)

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$CO2)), ]


##### CO2 ######

GC.Data.NoSTD.2022$CO2.ppm <- (as.numeric(GC.Data.NoSTD.2022$CO2) - 798) / 3.3  ;

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$CO2.ppm)), ]


plot(GC.Data.NoSTD.2022$CO2.ppm )


# ###############################################################################################################
# 
#                      Best calibration equation for N2O
# 
# ###############################################################################################################
# 
# 
# Call: rq(formula = N2O ~ N2O.ppm, tau = 0.5, data = C.GC.Standards.no.NA)
# 
# tau: [1] 0.5
# 
# Coefficients:
#   coefficients lower bd upper bd
# (Intercept) 134.9825     127.2636 149.5451
# N2O.ppm     624.6675     608.9579 634.0715 
# 
# Calibration equation
# 
# N2O = 135 + ( 625 * N2O.ppm )
# 
# N2O.ppm = (N20 - 135) / 625
# 
# 
# 
# ###############################################################################################################

str(GC.Data.NoSTD.2022)

anyNA(GC.Data.NoSTD.2022$N2O)

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$N2O)), ]



##### N2O ######


GC.Data.NoSTD.2022$N2O.ppm <- (as.numeric(GC.Data.NoSTD.2022$N2O) - 135) / 625  ;

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$CO2.ppm)), ]



plot(GC.Data.NoSTD.2022$N2O.ppm )


# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ; 
# 
# write.csv(x = GC.Data.NoSTD.2022, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\GC_data_Curated_2022",
# 
# Working.Date , ".csv"), append = F , row.names = F )


#read.csv(file = paste0("FluxDataAnalysisResults\\GC_data_Curated_20222026_10_22_11_34_59.csv" ))


###############################################################################################################
#                           
#                               Corrections based on Field measurements notes 
#
###############################################################################################################

#Block 1, Clover, D,










GC.Data.NoSTD.2022$Series <- paste( GC.Data.NoSTD.2022$Sampling.Day , GC.Data.NoSTD.2022$BLOCK.F , 
                               
                                    GC.Data.NoSTD.2022$CoverCrop.F , GC.Data.NoSTD.2022$Treatment.F, sep = "_") ;

head(GC.Data.NoSTD.2022)



str(GC.Data.NoSTD.2022)


levels(GC.Data.NoSTD.2022$Treatment.F)

levels(GC.Data.NoSTD.2022$BLOCK.F)

levels(GC.Data.NoSTD.2022$CoverCrop.F)

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$CoverCrop.F == "Clover" ,]


xyplot(CO2.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD.2022 , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD.2022$CO2.ppm)) ,   
       
       type="o", auto.key = T, main = "CO2");


xyplot(N2O.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD.2022 , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD.2022$N2O.ppm)) ,   
       
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


 
  