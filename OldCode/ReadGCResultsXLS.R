##############################################################################################################
# 
# 
# Function to read the results of the gas chromatograph from xlxs sumaries saved after analyzing the results. 
# 
#    Felipe Montes 2022/08/23
# 
#    Rev 2023/07/18
#  
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
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() # 


# setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("openxlsx",  dependencies = T)

# install.packages("Rtools",  dependencies = T)

# install.packages("pdftools",  dependencies = T)

# install.packages("askpass",  dependencies = T)

# install.packages("cli",  dependencies = T)

# install.packages("utf8",  dependencies = T)



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)





###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored

File.List.directory<-"C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results" ;

File.List<-list.files("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results"); length(File.List) ; 

# Only select the excel files

Excel.Results.Files<-File.List[grep(".xlsx", File.List)] ;


###############################################################################################################
#                           Read all the  excel files in the File.List
###############################################################################################################



## initialize the dataframe to collect all the data in the directory files in the Excel.Results.Files



PeakArea.results.0<-data.frame(Sample.Name = character(), Vial.number = integer(), CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), DateOfAnalysis = character(), AnalysisName = character() );

#i=2

for (i in seq(1,length(Excel.Results.Files))) {
  
   # Initialize the dataframe to collect all the data in the directory files in the Excel.Results.Files
  
  PeakArea.results.0<-data.frame(Sample.Name = character(), Vial.number = integer(), CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), DateOfAnalysis = character(), AnalysisName = character() );
  
  # Read the file using the openxlsx package

  PeakArea.results.1<-read.xlsx(xlsxFile = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results\\", 
                                                  
                                                  Excel.Results.Files[i]), sheet = 1, startRow=3, colNames=F, cols = c(1,2,4:6) );

  names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );


  PeakArea.results.1$AnalysisName<-Excel.Results.Files[[i]] ;

  PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );


  PeakArea.results.0<-PeakArea.results ;

  rm(PeakArea.results.1)

}

# Delete objects and files that are not longer needed

rm(PeakArea.results.1, i)




