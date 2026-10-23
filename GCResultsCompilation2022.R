##############################################################################################################
# 
# 
# Program to read and collect 2022 GC Analysis results data from Professor Lauren McPhillips
# 
#                Agilent 8890 Gas Chromatograph summary results reports
# 
#     
# 
# 
#  Felipe Montes 2022/08/23
# 
#  Updated 2026/06/03
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################

#.libPaths("C:\\Users\\frm10\\AppData\\Local\\R\\win-library\\4.2")  ;


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

# setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")




###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################


# Read the Directories where the GC data are stored.

# #### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!########

# 
# Data stored in one drive is not immediately accessible and therefore create problems when trying to read it directly
# 
# A good option is to download the data to the computer and read it from there

# #### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!######## 




# File.List.directory.path <- paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects",
# 
# "\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data",
# 
# "\\GasChromatograph\\Felipe2022\\Results") ;


###############################################################################################################
#
#                              Notes about GC Data 2026/10/22
# 
# Felipe_GCN2O_20220609_B1B2 had the summary results labeled as Results20220630. The date of the GC analysis is 06/30/2022. The labels was marked with the date of the GC analysis and not of the Sampling Date.
# That has been corrected in the directory "Results_2022_Curated_FM_20261022"
# 
# Felipe_GCN2O_20220802_B3B4 has results for Summary GCN20_20220910. The file with the data for 20220802 was lost. That data is no longer available
# 
# Felipe_GCN2O_202200615_B3B4 and Felipe_GCN2O_202200615_B2B4. Block 1 was not sampled and block 4 was run two times trough the GC. Select the first run.
# 
# Felipe_GCN2O_202200707_B2 , Block B1 was not sampled that day. The results of the Felipe_GCN2O_202200707_B2 are identical to the results of FelipeGCN2O_20220719_B3B4 
#  
# A mistake in reading the file could cause the problem. The data from was recovered by hand from the GC sequence and it is now in the 
# 
# file SummaryGCN2O_20220707_B2_20261022.xlsx.
# 
# File SummaryGCN2O_20220707_B2.pdf was removed to prevent duplication
# 
# Directories  "Trouble" and "FelipeGCN2O20220808" do not have data.
# 
# "FelipeGCN2O-20220609_B1B2" is a duplicate of FelipeGCN2O_20220629_B1B2 
# 
# The directory with the corrected files is Results_2022_Curated_FM_20261022
#
#
##############################################################################################################



File.List.directory.path <- "C:\\Users\\frm10\\Results\\Results_2022_Curated_FM_20261022"  ;


list.files(File.List.directory.path)

##### Remove "FelipeGCN2O_20220707_B2"  from the list. See paragraph above #########

list.files(File.List.directory.path)[File.List.directory.path == "FelipeGCN2O_20220707_B2" ]
 

File.List.directory <- list.files(File.List.directory.path);

File.List.directory <- File.List.directory[!File.List.directory == "FelipeGCN2O_20220707_B2" ] ;




File.List.Sub.directory <- list.files(paste0(File.List.directory.path , "\\" , File.List.directory [1] )); length(File.List.Sub.directory) ;

Summary.directory <- grep("Sum", File.List.Sub.directory, value = T) ;

Summary.directory


###############################################################################################################
#                           Read all the GC result reports in the File.List
###############################################################################################################



## initialize the dataframe to collect all the data in the directory files in the Excel.Results.Files

PeakArea.results.0<-data.frame(Sample.Name = character(), Position = integer() , Vial.number = integer(), 
                               
                               CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), File = character(),
                               
                               Sampling.Day = character(),  DateOfAnalysis = character(), AnalysisName = character() );





###############################################################################################################
# 
# Inputs required by the function ReadGCReportPDF
# 
#  1-> GCPDF.File.path = path to the file containing the Gas Chromatograph analysis report in pdf format
#  
#  in the code below GCPDF.File = paste0(File.List.directory,"\\",PDF.Results.Files[1])
# 
#    GCPDF.File.path = "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results"
#  
#  2-> GCPDF.File.name= Name of the Gas Cromatograph analysis report in pdf format
# 
# 
#    GCPDF.File.name="2021027B3B4peakareas.pdf" 
# 
# 
# 
###############################################################################################################




##### load the ReadGCReportPDF function ###

source(paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\",

                "CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\" ,
                
                "RCode\\GCResultsAnalysis\\ReadGCReportPDF2022.R"))

# i = 8

#for (i in seq(1,length(File.List.directory)))

for (i in seq(1,length(File.List.directory))){
  
  File.List.Sub.directory <- list.files(paste0(File.List.directory.path , "\\" , File.List.directory [[i]] ));
  
  Summary.directory <- grep("Sum", File.List.Sub.directory, value = T) ;
  
  
  File.List <- list.files(paste0(File.List.directory.path , "\\" , File.List.directory [[i]] , "\\" , 
                                 
                                 Summary.directory )); length(File.List) ;
  
  
  
  PDF.Results.Files <- File.List[grep(".pdf", x = File.List)] ;
  
  print(PDF.Results.Files)
  
  print(File.List.directory[i])
  
  print(i)
  
  
  # j = 1
  
  for (j in seq(1, length(PDF.Results.Files))) {
    
    PeakArea.results.1 <- ReadGCReportPDF2022(GCPDF.File.path = paste0(File.List.directory.path , "\\" , File.List.directory [[i]] , "\\" , 
                                                                     
                                                                     Summary.directory ), GCPDF.File.name = PDF.Results.Files[j]) ;
    
    #names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
    
    
    PeakArea.results.1$AnalysisName<-PDF.Results.Files[[j]] ;
    
    PeakArea.results <- rbind(PeakArea.results.0,PeakArea.results.1 );
    
    
    PeakArea.results.0 <- PeakArea.results ;
    
    # Delete objects and files that are not longer needed
    
    rm(PeakArea.results.1, PeakArea.results)
    
    
    
  }
  
}

PeakArea.Results.All <- PeakArea.results.0 ;



###############################################################################################################

# Add The data from Felipe_GCN2O_202200707_B2 was recovered by hand from the GC sequence and it is now 
# 
# in the file SummaryGCN2O_20220707_B2_20261022
  
###############################################################################################################

read.xlsx( xlsxFile = paste0("C:\\Users\\frm10\\Results\\Results_2022_Curated_FM_20261022\\",

                            "FelipeGCN2O_20220707_B2\\Summary\\SummaryGCN2O_20220707_B2_20261022.xlsx"))

PeakArea.results.GCN2O_20220707_B2 <- read.xlsx( xlsxFile = paste0("C:\\Users\\frm10\\Results\\Results_2022_Curated_FM_20261022\\",
                                                                   
                                               "FelipeGCN2O_20220707_B2\\Summary\\SummaryGCN2O_20220707_B2_20261022.xlsx")) ; 

str(PeakArea.results.GCN2O_20220707_B2)

str(PeakArea.Results.All)

PeakArea.results.GCN2O_20220707_B2$File <- "SummaryGCN2O_20220707_B2_20261022.xlsx" ;

PeakArea.results.GCN2O_20220707_B2$Sampling.Day <- "20220707" ;

PeakArea.results.GCN2O_20220707_B2$Sampling.Date <- as.Date("2022-07-07") ;

PeakArea.results.GCN2O_20220707_B2$GC.Date <- as.Date("2022-10-14") ;

PeakArea.results.GCN2O_20220707_B2$AnalysisName <- "SummaryGCN2O_20220707_B2_20261022.xlsx" ;

names(PeakArea.results.GCN2O_20220707_B2)[4:6]

names(PeakArea.Results.All)

names(PeakArea.results.GCN2O_20220707_B2)[4:6] <- c( "CH4" , "CO2" , "N2O") ;


#### Add the data to the Dataframe #####

rbind(PeakArea.Results.All , PeakArea.results.GCN2O_20220707_B2) 

PeakArea.Results.All.2 <- rbind(PeakArea.Results.All , PeakArea.results.GCN2O_20220707_B2) ;

str(PeakArea.Results.All.2)

PeakArea.Results.All <- PeakArea.Results.All.2 ;


Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;

write.csv(x = PeakArea.Results.All, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",

"Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,

"DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\GCcompiledResults_2022_",

Working.Date , ".csv"), row.names = F )





