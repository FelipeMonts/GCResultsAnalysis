##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph  
# 
# 
#  Felipe Montes 2021/07/01
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################


#  Tell the program where the package libraries are  #####################

.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard()

#setwd("C:\\Felipe\\Willow_Project\\Willow_Experiments\\Willow Rock Spring\\SkyCap_SelectionTrial\\DataCollection") ;   # 

"https://pennstateoffice365.sharepoint.com/:f:/s/StrategicTillageAndN2O/Ehl9Lh_gza5FiOtKIyDD7MQBOKFdFk6h_k4EEYEktWJUYw?e=uYLqL0"

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################




###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)





###############################################################################################################
#                           load the files that will be needed  
###############################################################################################################


## Read which files are available in the directory

Files.List<-list.files("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\RawData20210528") ;

## Select files that are ..xlsx only

Files.csv.1<-Files.List[grep(".csv",Files.List)] ;


Files.csv<-Files.csv.1[which(Files.csv.1 != "Standby_1.csv")]


i=1

###############################################################################################################
#                           Read all the  excel files 
###############################################################################################################


## initialize the dataframe to collect all the data

PeakArea.results<-data.frame(Sample.Name = "SampleName", DateOfAnalysis="2021-06-25 08:26:16-04:00", CH4.Area =9999, CO2.Area =9999, N2O.Area =9999 );

for (i in (seq(1, length(Files.csv)))) {
  
  
  
  
  
  ## Read the files and get the variable names and the correct structure
  
  Analysis.date<-read.csv(paste0("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\RawData20210528\\",Files.csv[i]), header=F, skip=2, nrows=1);
  
  CH4.Data<-read.csv(paste0("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\RawData20210528\\", Files.csv[i]), header=T, skip=28, nrows=1);
  
  CO2.Data<-read.csv(paste0("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\RawData20210528\\",Files.csv[i]), header=T, skip=34, nrows=1);
  
  N2O.Data<-read.csv(paste0("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\RawData20210528\\",Files.csv[i]), header=T, skip=40, nrows=1);
  
  PeakArea.results[i,c('Sample.Name')]<-sub(Files.csv[i], pattern="*.csv",replacement="") ;
  
  
  PeakArea.results[i,c('DateOfAnalysis')]<-Analysis.date[2] ;
  
  PeakArea.results[i, c('CH4.Area')]<-CH4.Data$Area ;
  
  PeakArea.results[i, c('CO2.Area')]<-CO2.Data$Area ;
  
  PeakArea.results[i, c('N2O.Area')]<-N2O.Data$Area ;
  
  
}



