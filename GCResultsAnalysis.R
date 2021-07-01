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

library(rgdal)

library(raster)

library(sp)



###############################################################################################################
#                           load the files that will be needed  
###############################################################################################################


## Read which files are available in the directory

Files<-list.files("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\O2SensorTesting")

## Select files that are ..xlsx only

Files[grep(".xlsx",Files)]


###############################################################################################################
#                           Read all the  excel files 
###############################################################################################################
