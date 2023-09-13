##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused on analyzing standards for calibration
# 
# 
#  Felipe Montes 2022/08/23
# 
# 
# 
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

# setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")




###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored

File.List.directory.path <- "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Felipe2022\\Results" ;

File.List.directory <- list.files(File.List.directory.path); length(File.List.directory) ; 

File.List.Sub.directory <- list.files(paste0(File.List.directory.path , "\\" ,File.List.directory [[1]] )); length(File.List.Sub.directory) ;

Summary.directory <- grep("Sum", File.List.Sub.directory, value = T) ;


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



# i = 1

#for (i in seq(1,length(File.List.directory)))

for (i in seq(1,length(File.List.directory)-2)){
  
  File.List.Sub.directory <- list.files(paste0(File.List.directory.path , "\\" ,File.List.directory [[i]] ));
  
  Summary.directory <- grep("Sum", File.List.Sub.directory, value = T) ;
  
  
  File.List <- list.files(paste0(File.List.directory.path , "\\" , File.List.directory [[i]] , "\\" , 
                                 
                                 Summary.directory )); length(File.List) ;
  
  
  
  PDF.Results.Files<-File.List[grep(".pdf", x = File.List)] ;
  
  # j = 1
  
  for (j in seq(1, length(PDF.Results.Files))) {
    
    PeakArea.results.1<-ReadGCReportPDF2022(GCPDF.File.path = paste0(File.List.directory.path , "\\" , File.List.directory [[i]] , "\\" , 
                                                                     
                                                                     Summary.directory ), GCPDF.File.name = PDF.Results.Files[j]) ;
    
    #names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
    
    
    PeakArea.results.1$AnalysisName<-PDF.Results.Files[[j]] ;
    
    PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );
    
    
    PeakArea.results.0<-PeakArea.results ;
    
    # Delete objects and files that are not longer needed
    
    rm(PeakArea.results.1)
    
    
    
  }
  
}


write.csv(x = PeakArea.results, file = "D:\\Felipe\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\GCcompiledResults2022.csv", row.names = F )

