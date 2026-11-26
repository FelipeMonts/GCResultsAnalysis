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

# source( file = "DataCurationGCAnalysis2022.R" , verbose = T)

#### Read data  #####


PeakArea.results.2022 <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults_2022_2026_11_25_18_51_50.csv" ) , header = T) ;

tail(PeakArea.results.2022)

PeakArea.results.2022[PeakArea.results.2022$Sampling.Date == "2022-07-07" & 
                        
                        PeakArea.results.2022$File == "SummaryGCN2O_20220707_B2_20261022.xlsx" , ]

tail(PeakArea.results.2022[PeakArea.results.2022$Sampling.Date == "2022-07-07" , ])

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

###############################################################################################################
#                           
#                               Writing out curated Standards data
#
###############################################################################################################

##### Selecting standards records  #####

Standards.Dataset.2022 <- PeakArea.results.2022[grep( pattern = "B" , x = PeakArea.results.2022$Sample.Name, invert = T) ,]

str(Standards.Dataset.2022)


# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
# 
# write.csv(x = Standards.Dataset.2022, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\StandardsDataset2022_",
# 
# Working.Date , ".csv"), row.names = F ) ;
# 




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

GC.Data.NoSTD.2022$Treatment <- c("NONE");

GC.Data.NoSTD.2022[grep("AT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T), c("Treatment")] <- c("A");

GC.Data.NoSTD.2022[grep("BT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T), c("Treatment")] <- c("B");

GC.Data.NoSTD.2022[grep("CT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T) , c("Treatment")] <- c("C");

GC.Data.NoSTD.2022[grep("DT",GC.Data.NoSTD.2022$Sample.Name , ignore.case = T) , c("Treatment")] <- c("D");

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

GC.Data.NoSTD.2022$BLOCK <- c(9999);

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

GC.Data.NoSTD.2022[which(GC.Data.NoSTD.2022$Sampling.Time == 9999),];

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

GC.Data.NoSTD.2022[GC.Data.NoSTD.2022$Sampling.Date == "2022-07-07" & GC.Data.NoSTD.2022$BLOCK == 2 ,]


browser()

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


browser()

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


### Sampling.Date == 2022-09-27  ; Sample.Name == B3TritCT30

# Selecting set with the conditions 


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & GC.Data.NoSTD.2022$Sampling.Time == 30 , ]



GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & GC.Data.NoSTD.2022$Sampling.Time == 30 , 
                    
                    "Field.Notes" ] <- "YES";


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" & GC.Data.NoSTD.2022$Sampling.Time == 30 , 
                    
                    "Note" ] <- " T2 Solenoid is Failing" ;




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
                    

##############################################################################################################
#                           
#                              Adding GC processing notes to the data
#                              
#                              Making adjustments when appropriate according to the notes
#
##############################################################################################################

str(GC.Data.NoSTD.2022)

names(GC.Data.NoSTD.2022)[19] <- "Note.Field" ; 


names(GC.Data.NoSTD.2022)

GC.Data.NoSTD.2022$GC.Notes <- "NO" ;

GC.Data.NoSTD.2022$Note.GC <- NA ;

   
# ######  GC - Notes20220629_B1B2 #########
# 
# The vials on Block 2, 3-SppN treatment B were switched with the vials of Block 2, 3SppN treatment C
# 
# 2-3SppN-B <-> 2-3SppN-C
# 
##################################  


# Selecting set with the conditions   3SppN treatment B

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , "GC.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , "Note.GC" ] <- paste0("The vials on Block 2, 3-SppN treatment B",
                      
                      " were switched with the vials of Block 2, 3SppN treatment C" , "2-3SppN-B <-> 2-3SppN-C") ;




# Selecting set with the conditions   3SppN treatment c

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "GC.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" & GC.Data.NoSTD.2022$CoverCrop == "3Spp" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "C" , "Note.GC" ] <- paste0("The vials on Block 2, 3-SppN treatment B",
                                                                                  
                                                                                  " were switched with the vials of Block 2, 3SppN treatment C" , "2-3SppN-B <-> 2-3SppN-C") ;



# ######  GC - Notes20220615_B2B4 #########
# 
# 20220615 B4 was already processed on 08 15 22
# 
# 20220615 B2 was not processed# 
################################## 


# Selecting set with the conditions   20220615 B4

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", "GC.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "4", "Note.GC" ] <- "20220615 B4 was aleady processed on 08 15 22"  ;


# Selecting set with the conditions   20220615 B2


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", "GC.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2", "Note.GC" ] <- "20220615 B2 was not proceesed"



# ######  GC - Notes20220629_B3B4 #########
# 
# The sequence was interrupted because of a potential power out.
# It was interrupted at vial 12. It should be restarted at vial 13.
# Sample B3TritBT45 was lost when the GC was aborted.
# The complete sequence was restarted and therefore the second set of results samples through 12 B3TritBT45 should be discarded.
################################## 

# Selecting set with the conditions   20220629  B3TritB


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , "GC.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-29")  & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" & GC.Data.NoSTD.2022$CoverCrop == "Trit" & 
                      
                      GC.Data.NoSTD.2022$Treatment == "B" , "Note.GC" ] <- paste0("The sequence was interrupted",
                      
                      "because of a potential power out. " , "It was interrupted at vial 12.", 
                      
                      "Sample B3TritBT45 was lost when the GC was aborted. " , "The complete sequence was restarted ",
                      
                      "and therefore the second set of results samples through 12 B3TritBT45 should be discarded.") ;


# ######  GC - Notes20220927_B1B2 #########
# 
# Air cylinder was at 0 PSI!!! Changed the air cylinder. 
################################## 

# Selecting set with the conditions   20220927B1 B2

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27") & 
                      
                      (GC.Data.NoSTD.2022$BLOCK == "1" | GC.Data.NoSTD.2022$BLOCK == "2"), ]


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27") & 
                      
                      (GC.Data.NoSTD.2022$BLOCK == "1" | GC.Data.NoSTD.2022$BLOCK == "2"), "GC.Notes" ] <- "YES" ;


GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-09-27") & 
                      
                      (GC.Data.NoSTD.2022$BLOCK == "1" | GC.Data.NoSTD.2022$BLOCK == "2"), "Note.GC" ] <- 
  
  "Air cylinder was at 0 PSI!!! Changed the air cylinder. " ;

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15") & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "3" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15") & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "2" , ]

GC.Data.NoSTD.2022[ GC.Data.NoSTD.2022$Sampling.Date == paste0("2022","-06-15") & 
                      
                      GC.Data.NoSTD.2022$BLOCK == "1" , ]


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
#                      Best calibration equation Equation Coefficients
# 
# ###############################################################################################################


 read.csv(file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",

"Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,

"DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\",

"Calibration_coeff2026_11_16_13_31_43", ".csv"), header = T )

# ###############################################################################################################



###############################################################################################################
#                          
#            Calculation of concentration based on the Standard gas concentrations
#             
#            The calibration curves were calculated with the StandardCalibration.R code
#  
#
###############################################################################################################


GC.Calibration.coef <- read.csv(file = "FluxDataAnalysisResults\\Calibration_coeff2026_11_16_13_31_43.csv", header = T) ;

str(GC.Calibration.coef)

names(GC.Calibration.coef)[1] <- "Parameter" ;

GC.Calibration.coef


# ###############################################################################################################
# 
#                      Calibration equation for CO2
# 
# ###############################################################################################################
# 
# Parameter coefficients   lower.bd   upper.bd
# 1 CO2.Intercept   780.525526 728.423482 820.062038
# 2     CO2.Slope     3.391092   3.264132   3.499893
# 3 N2O.Intercept   132.557000 121.877913 140.096478
# 4     N2O.Slope   621.841160 608.128356 634.292285
# 
#
# Calibration equation
# 
# 
# CO2 = 780.525526 + (3.391092 * CO2.ppm)
#
# CO2 = 780.526 + (3.391 * CO2.ppm)
# 
# CO2.ppm =( CO2 - 780.526 ) / 3.391
# 
# 
# ###############################################################################################################



str(GC.Data.NoSTD.2022)

anyNA(GC.Data.NoSTD.2022$CO2)

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$CO2)), c( "Sample.Name" , "Field.Notes" , "Note.Field")]

str(GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$CO2)), c( "Sample.Name" , "Field.Notes" , "Note.Field")])  


##### CO2 ######


GC.Data.NoSTD.2022$CO2.ppm <- (GC.Data.NoSTD.2022$CO2 - 
                                 
                                 GC.Calibration.coef[GC.Calibration.coef$Parameter == "CO2.Intercept", "coefficients"]) / 
  
                                 GC.Calibration.coef[GC.Calibration.coef$Parameter == "CO2.Slope", "coefficients"] ;


GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$CO2.ppm)), ]

which(is.na(GC.Data.NoSTD.2022$CO2.ppm))

plot(GC.Data.NoSTD.2022$CO2.ppm )


# ###############################################################################################################
# 
#                      Best calibration equation for N2O
# 
# ###############################################################################################################
# 
# 
# Parameter coefficients   lower.bd   upper.bd
# 1 CO2.Intercept   780.525526 728.423482 820.062038
# 2     CO2.Slope     3.391092   3.264132   3.499893
# 3 N2O.Intercept   132.557000 121.877913 140.096478
# 4     N2O.Slope   621.841160 608.128356 634.292285
# 
# Calibration equation
# 
# N2O = 132.557000 + ( 621.841160 * N2O.ppm )
# 
# N2O = 132.56 + ( 621.84 * N2O.ppm )
#
# N2O.ppm = ( N20 - 132.56 ) / 621.84
# 
# 
# 
# ###############################################################################################################

str(GC.Data.NoSTD.2022)

anyNA(GC.Data.NoSTD.2022$N2O)

##### N2O ######

GC.Data.NoSTD.2022$N2O.ppm <- (GC.Data.NoSTD.2022$N2O -  
                                 
                                 GC.Calibration.coef[GC.Calibration.coef$Parameter == "N2O.Intercept", "coefficients"]) /
  
                                GC.Calibration.coef[GC.Calibration.coef$Parameter == "N2O.Slope", "coefficients"] ;

GC.Data.NoSTD.2022[which(is.na(GC.Data.NoSTD.2022$N2O.ppm)), ]

which(is.na(GC.Data.NoSTD.2022$N2O.ppm))

plot(GC.Data.NoSTD.2022$N2O.ppm )


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
# Pullens, Johannes Wilhelmus Maria, Diego Abalos, Søren O. Petersen, and Asger R. Pedersen.
# 
# “Identifying Criteria for Greenhouse Gas Flux Estimation with Automatic and Manual Chambers:
#   
# A Case Study for N2O.” European Journal of Soil Science 74, no. 1 (2023): e13340. https://doi.org/10.1111/ejss.13340.
# 
# and the r package : 
#
# Pedersen, Asger Roer. HMR: Flux Estimation with Static Chamber Data. V. 1.0.5. 
# 
# Released December 17, 2025. https://cran.r-project.org/web/packages/HMR/index.html.
# 
# The chosen units do, however, determine the unit of the estimated flux, which has the physical unit of 
# (VC)/(At), where  t and C denote, respectively, time and concentration. 
# 
#  V[L] , A[m2] ,t[h] , C[micro g/L] ==> fo [micro g/m2 /h ] 
#  
#  V[L] , A[m2] ,t[min] , C[micro L/L] ==> fo [micro L/m2 /min ] 
#  
#  V[L] , A[Km2] ,t[s] , C[Kg/m3] ==> fo [Kg/Km2/s ] 
#  
#
#   The results in Alli's Thesis are in N2O flux in g N2O-N ha-1 d-1, and cumulative emissions in kg N2O-N ha-1'.
# 
#   Therefore the units of concentration to be used in the HMR analysis needs to be
# 
#    V[m3] , A[m2] , t[h], C[g/m3] ==> fo [g/m2/h]
# 
# 
#   To convert from ppm(1/10^6) to g/m3;
# 
#   PV = nRT ; PV/RT = n ;
# 
#   P = 1 atm ; T = 273 K + 20°C = 293 K ; V = 1 m3 ;  R = m3-atm/K-Mol  with 1 Mol = MW in g
#   
#   (1 [m3] X * 1 [atm] )/ (293 [K] * 8.205736e-5 [m3-atm/K-Mol ]) = n [Mol] * g / [Mol] = n g
# 
#   n [g] / 10^6 [m3] = n [g] / [m3]
# 
#   n [g] / [m3] * 1000 [mg] / 1 [g] = n [mg] / [m3]
# 
# ###############################################################################################################


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
#
#               Reference data taken from Allison Kohele's Calculations Excel Files
#
###############################################################################################################



Molar.Mass <- data.frame(GAS=c("CH4" , "CO2" , "N2O"), UNITS=c("g/mol"), VALUE = c(16.04, 44.01, 44.013));

Gas.Law <- data.frame(UNITS=c("L-atm/Mol-K", "J/K-Mol", "m3-Pa/K-Mol", "Kg-m2-s2/K-Mol", "m3-atm/K-Mol"), VALUE = c(0.08205736, 8.314462,8.314462, 8.314462, 8.205736e-5 ))  ;



################################# Test conversion #############################################################
# 
# 100 ppm N2O == > g/m3
# 
# 
# 100 * 1 / (293 * 8.205736e-5) = 4159.248 Mol  ; 4159.248 * 44.013 / 1e6 = 0.183 g N2O /m3 ; 
# 
# 0.183 * 1000 = 183 mg N2O / m3
# 
#   (100  * Molar.Mass[Molar.Mass$GAS == "CO2" , "VALUE"] * 1000) /
#   
#   (Gas.Law[Gas.Law$UNITS == "m3-atm/K-Mol" , "VALUE"] * 293 * 1e6)
# 
# 
###############################################################################################################


GC.Data.NoSTD.2022$CO2.mg.m3 <- (GC.Data.NoSTD.2022$CO2.ppm  * Molar.Mass[Molar.Mass$GAS == "CO2" , "VALUE"] * 1000) /
  
  (Gas.Law[Gas.Law$UNITS == "m3-atm/K-Mol" , "VALUE"] * 293 * 1e6) ;



GC.Data.NoSTD.2022$N2O.mg.m3 <- (GC.Data.NoSTD.2022$N2O.ppm  * Molar.Mass[Molar.Mass$GAS == "N2O" , "VALUE"] * 1000) /
  
  (Gas.Law[Gas.Law$UNITS == "m3-atm/K-Mol" , "VALUE"] * 293 * 1e6) ;


str(GC.Data.NoSTD.2022)


###############################################################################################################
#                           
#                               Writing curated Data 2022 
#
###############################################################################################################




# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
# 
# write.csv(x = GC.Data.NoSTD.2022, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\GC_data_Curated_2022",
# 
# Working.Date , ".csv"), row.names = F )
# 
# 
# read.csv(file = paste0("FluxDataAnalysisResults\\GC_data_Curated_20222026_11_25_19_07_54.csv" ))





# write.table(x = Test.data.HMR.CO2.1, sep = ";", dec = "." ,file = "TEST_DATA.csv", row.names = F)



# save.image(file = paste0("FluxDataAnalysisResults\\GCAnalysis" , Year , ".RData"))


 
  