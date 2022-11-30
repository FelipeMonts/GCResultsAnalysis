##############################################################################################################
# 
# Program to bound together into a coherent dataframethe the results of gas chromatogrpah obtained from the PDF reports saved 
# after analyzing the results. 
# 
# It uses the ReadGCReportPDF function created in another program and called here using the source function
# 
# 
# 
#      Felipe Montes 2022/10/28
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################
# 
# 
# 
# #  Tell the program where the package libraries are  #####################
# 
# .libPaths("D:/Felipe/SotwareANDCoding/R_Library/library")  ;
###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() ;    


#setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("openxlsx",  dependencies = T, lib="D:/Felipe/SotwareANDCoding/R_Library/library")

# install.packages("Rtools",  dependencies = T, lib="D:/Felipe/SotwareANDCoding/R_Library/library")

# install.packages("pdftools",  dependencies = T, lib="D:/Felipe/SotwareANDCoding/R_Library/library")

# install.packages("askpass",  dependencies = T, lib="D:/Felipe/SotwareANDCoding/R_Library/library")

# install.packages("cli",  dependencies = T, lib="D:/Felipe/SotwareANDCoding/R_Library/library")

# install.packages("utf8",  dependencies = T, lib="D:/Felipe/SotwareANDCoding/R_Library/library")

  
  ###############################################################################################################
  #                           load the libraries that are needed   
  ###############################################################################################################
  
  library(openxlsx)
  
  library(lattice)
  
  library(pdftools)
  
  library(stringr)
  
###############################################################################################################
# 
#       Load the function ReadGCReportPDF(GCPDF.File.path, GCPDF.File.name ) to read and organize the GC sample analysis PDF reports
# 
# 
# Inputs required by the function
# 
#  1-> GCPDF.File.path = path to the file containing the Gas Cromatograph analysis report in pdf format
#  
#  in the code below GCPDF.File=paste0(File.List.directory,"\\",PDF.Results.Files[1])
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

source("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\ReadGCReportPDF.R")



### Organize the file paths and file names to input into the function sequentially 

### Path for the results of 2021: "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\GCResults2021\\SummaryReport"

### readClipboard() ;  


Directory.List.2021.Path<-("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\GCResults2021\\SummaryReport");


Directory.List.2021<-list.files(Directory.List.2021.Path) ;

# i= Directory.List.2021[[1]]

 for (i in Directory.List.2021) {
   
 File.List<-list.files(paste0(Directory.List.2021.Path,"\\",i))
 
 PDF.Files<-File.List[grep(".pdf", File.List)]
 
 File.DataF<-data.frame("directory=",i,"file=",PDF.Files)

 print(File.DataF)
   
 }

### initialize the data frame that will contain all the data from all the GC analysis reports

v<-data.frame( Sample.Name = character(), Position = integer(), Vial = integer() , CH4 = double() ,
                           CO2 = double() , N2O = double(), File = character(), Sampling.Day = character(), 
                           Sampling.Date = as.Date(character()), GC.Date = as.Date(character())) ;
   




### Read the pdf report with the function ReadGCReportPDF(GCPDF.File.path, GCPDF.File.name ) and stored in a data frame


assign(paste0("GCAnalysis",PDF.Files[1]), ReadGCReportPDF(GCPDF.File.path=Directory.List.2021.Path, 
                                                          GCPDF.File.name=paste0(Directory.List.2021[[1]],"\\",PDF.Files[1] ) ) )






