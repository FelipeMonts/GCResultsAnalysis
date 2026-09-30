##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused agregating the curated data set of GC measurements from 2021
# 
# 
#  Felipe Montes 2022/08/23
# 
# Updated  2026/06/27
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

#  install.packages("gtools",  dependencies = T)

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)

library(gtools)




###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

# setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")


###############################################################################################################
#                            Load the function  ReadGCReportPDF2021.R
###############################################################################################################

#source(file = "D:\\Felipe\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\ReadGCReportPDF2021.R", verbose =T)

source(file = paste0("C:/Users/frm10/OneDrive - The Pennsylvania State University/Current_Projects/" , 

                      "CCC Based Experiments/StrategicTillage_NitrogenLosses_OrganicCoverCrops/" , 
                      
                      "DataAnalysis/RCode/GCResultsAnalysis/ReadGCReportPDF2021.R"), verbose =T)


###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored


# File.List.directory <- "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results" ;


File.List.directory <- paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects",
                              
                             "\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data" , 
                             
                             "\\GasChromatograph\\Alli_Felipe2021\\Results") ;

File.List <- list.files(File.List.directory); length(File.List) ; 

# Only select the pdf files

PDF.Results.Files.1 <- File.List[grep(".pdf", File.List)] ;

PDF.Results.Files.1


str(PDF.Results.Files.1)


# PDF.Results.Files.1[6]

# PDF.Results.Files.1[45]

######## "20210929B1B2peakareas1.pdf" which had a GC error and therefore is incomplete ######

######## chamber tests also need to be removed

######## Files : 20210601B1B2sample1-22summaryreport.pdf , 20210601B1B2sample24-84summaryreport.pdf,

######## 20210601B3B4SummaryReport.pdf, "20210528B1B4peakareasMERGED.pdf" and "20210528peakareas.pdf" 

####### do not have valid data. GC samples are not named, just numbered without any numbering reference.

####### These data needs to be added manually later on ######



#### Files to be removed #######



PDF.Files.to.Remove <- c( "ChamberTest.pdf", "ChamberTest2.pdf", "ChamberTest3.pdf" ,  
                          
                          "ChamberTest4.pdf" , "ChamberTest6.pdf" , "ChamberTestAuto.pdf" , "COMPARE.pdf" , 
                          
                          "FelipeStandardsTest20211031.pdf", "TestStandardspeakareas.pdf", 
                          
                          "20210601B1B2sample1-22summaryreport.pdf , 20210601B1B2sample24-84summaryreport.pdf",
                          
                          "20210601B3B4SummaryReport.pdf", "20210929B1B2peakareas1.pdf"  ) ;

PDF.Results.Files <- PDF.Results.Files.1[! PDF.Results.Files.1 %in% PDF.Files.to.Remove] ;


str(PDF.Results.Files)

# Excel.Results.Files <-File.List[grep(".xlsx", File.List)] ;


###############################################################################################################
#                           Read all the GC result reports in the File.List
###############################################################################################################



## initialize the dataframe to collect all the data in the directory files in the Excel.Results.Files

PeakArea.results.0 <- data.frame(Sample.Name = character(), Position = integer() , Vial.number = integer(), 
                               
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


# which(PDF.Results.Files == "20210929B1B2peakareas1.pdf") 

# i = PDF.Results.Files[44]


for (i in PDF.Results.Files) {
  

  PeakArea.results.1<-ReadGCReportPDF2021(GCPDF.File.path = paste0("C:\\Users\\frm10\\",
                                                                   
  "OneDrive - The Pennsylvania State University\\","Current_Projects\\CCC Based Experiments\\",
  
  "StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph\\Alli_Felipe2021\\Results")
                                      
                                      , GCPDF.File.name = i);
  
  #

  #names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );


 # PeakArea.results.1$AnalysisName <- i ;

  PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );


  PeakArea.results.0<-PeakArea.results ;
  
  # Delete objects and files that are not longer needed

  rm(PeakArea.results.1)

}

str(PeakArea.results.0)



Working.Date <- format(Sys.time() , "%Y%M%d%h%m%s") ; 


# Manually adding data for "20210601B1B2sample1-22summaryreport.pdf" , "20210601B1B2sample24-84summaryreport.pdf" ,
# 
# "20210601B3B4SummaryReport.pdf" ,"20210929B1B2peakareas1.pdf"
# 
# The original data does not have samples names. The sample names were taken 
# 
# from the sampling data in 05/28 and 06/04 which have the same ordering in the GC analysis


PeakArea.results.20210929B1B2peakareas1 <- ReadGCReportPDF2021(GCPDF.File.path = paste0("C:\\Users\\frm10\\",
                                                                 
                                                                 "OneDrive - The Pennsylvania State University\\","Current_Projects\\CCC Based Experiments\\",
                                                                 
                                                                 "StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph\\Alli_Felipe2021\\Results")
                                        
                                        , GCPDF.File.name = "20210929B1B2peakareas1.pdf");



# Data from 20210601 does not have names for the samples. The data is collected from the data curate curation that
# 
# that Alli did to perform the calculations for her thesis.  The file location is:
# 
# https://pennstateoffice365.sharepoint.com/sites/StrategicTillageAndN2O/Shared%20Documents/
# 
# Data/GCResults/GCResults2021/SummaryReport/20210601/20210601calculations.xlsx
# 
# The file was copied to the folder that contains all the rest of the data: 
#   
# C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\CCC Based Experiments\\
# StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph\\Alli_Felipe2021\\Results\\20210601calculations.xlsx
# 
# and therefore is now included in the "File.List" 



which(File.List == "20210601calculations.xlsx" )

File.List[9]

Data.20210601 <- read.xlsx( xlsxFile = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\" ,
                             
                             "CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\",
                             
                             "GasChromatograph\\Alli_Felipe2021\\Results\\20210601calculations.xlsx" ),
           
           sheet = "Peak and Gas Concentration", startRow = 11, colNames = F, cols = c(1, 5,6:9)) ;


names(Data.20210601) <- c("Sample.Name" , "Position" , "Vial", "CH4" , "CO2" , "N2O") ;

str(Data.20210601)

head(Data.20210601, 10)

str(PeakArea.results.0)

head(PeakArea.results.0)

Data.20210601$File <- "20210601calculations.xlsx" ;

Data.20210601$Sampling.Day <- "20210601"  ;

Data.20210601$Sampling.Date <- as.Date("2021-06-01") ;

Data.20210601$GC.Date <- as.Date("2021-06-15") ;

str(Data.20210601)


##### Grouping all the results together

PeakArea.Results.All <-rbind(PeakArea.results.0 , PeakArea.results.20210929B1B2peakareas1, Data.20210601 )  ;





str(PeakArea.Results.All)


names(PeakArea.Results.All)



Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ; 

# write.csv(x = PeakArea.Results.All, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\GCcompiledResults_2021",
# 
# Working.Date , ".csv"), row.names = F )

          
