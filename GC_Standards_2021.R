##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused on analyzing standards for calibration from the measurements on year 2021
# 
# 
#  Felipe Montes 2022/08/23
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

# install.packages("Viridis",  dependencies = T)

# install.packages("paletteer", dependencies = T)

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)

library(quantreg)

library(HMR)

library(viridis)

library(paletteer)

###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd(paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\",

"CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis"));


#### N2O Sampling year  #####

Year = 2021

# Year = 2022

###############################################################################################################
#                           
#                              Reading standards from the 2021 curated data
#
###############################################################################################################

GC.standards <- read.csv(file = "FluxDataAnalysisResults\\StandardsDataset2021_2026_11_04_09_13_48.csv" )  ;


###############################################################################################################
#
#                              Checking for duplicated records
#
###############################################################################################################

str(GC.standards)

head(GC.standards)

anyDuplicated(GC.standards, MARGIN = c(1,2))


###############################################################################################################
#
#                              Checking for repeated measurements
#
###############################################################################################################


anyDuplicated(GC.standards[,c(4:6)], MARGIN = c(1,2))



###############################################################################################################
#                           
#                            Checking Standard Values manually because 
#                The Standards Names PerSTDA and PerSTD are not consistent through out 2021, 
#                                therefore need to be selected manually!!!!!!
#
###############################################################################################################

###### Converting Sampling.Date from a character type to a date type 

GC.standards$Sampling.Date <- as.Date(GC.standards$Sampling.Date) ;


##############################################################################################################
#                           
#                           Working with Standards obtained in 2021
#
###############################################################################################################

# 
# GC.standards[GC.standards$Sample.Name == "100PerSTDA",c('CH4.ppm')] <- 5 ;
# GC.standards[GC.standards$Sample.Name == "100PerSTDA",c('CO2.ppm')] <- 500 ;
# GC.standards[GC.standards$Sample.Name == "100PerSTDA",c('N2O.ppm')] <- 1 ;

head(GC.standards);

GC.standards$CH4.ppm <- NaN

GC.standards$CO2.ppm <- NaN

GC.standards$N2O.ppm <- NaN



#################################    2021-05-28     ############################################

GC.standards[GC.standards$Sampling.Date == "2021-05-28",]


GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 ,  125 ,  0.25  )  # c(1 , 2 , 3 ) ;

GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5 , 250 , 0.5 ) ;

  
GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 , 375 ,  0.75  ) ;


GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5 , 500 , 1  ) ;

GC.standards[GC.standards$Sampling.Date == "2021-05-28",]


#################################    2021-06-01   ############################################
 
 str(GC.standards)
 
 GC.standards[GC.standards$Sampling.Date == "2021-06-01",]
 

 GC.standards[GC.standards$Sampling.Date == "2021-06-01" & GC.standards$Sample.Name ==  "25perSTD", 
              
              c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 ,  125 ,  0.25  )  # c(1 , 2 , 3 ) ;
 
 GC.standards[GC.standards$Sampling.Date == "2021-06-01" & GC.standards$Sample.Name ==  "50perSTD", 
              
              c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5 , 250 , 0.5 ) ;
 
 
 GC.standards[GC.standards$Sampling.Date == "2021-06-01" & GC.standards$Sample.Name ==  "75perSTD", 
              
              c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 , 375 ,  0.75  ) ;
 
 
 GC.standards[GC.standards$Sampling.Date == "2021-06-01" & GC.standards$Sample.Name ==  "100perSTD", 
              
              c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5 , 500 , 1  ) ;
 
 GC.standards[GC.standards$Sampling.Date == "2021-06-01",]
 
 
 

#################################    2021-06-04     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-04",]

GC.standards[GC.standards$Sampling.Date == "2021-06-04", "Sample.Name"]


GC.standards[GC.standards$Sampling.Date == "2021-06-04" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-04" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-04" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-04" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;

GC.standards[GC.standards$Sampling.Date == "2021-06-04",]



#################################    2021-06-14     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-14",]

#### rows with file 20210614B1B2peakareasMERGED and 20210614B1B2summaryreport1 are duplicated


#### removing rows 20210614B1B2summaryreport1 

GC.standards[!GC.standards$File == "20210614B1B2summaryreport1.pdf" ,]

GC.standards <- GC.standards[!GC.standards$File == "20210614B1B2summaryreport1.pdf" ,]

GC.standards[GC.standards$File == "20210614B1B2summaryreport1.pdf" ,]


GC.standards[GC.standards$File == "20210614B1B2summaryreport1.pdf" ,]




GC.standards[GC.standards$Sampling.Date == "2021-06-14",]

GC.standards[GC.standards$Sampling.Date == "2021-06-14", "Sample.Name"]


GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;

GC.standards[GC.standards$Sampling.Date == "2021-06-14",]






#################################    2021-06-16     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-16",]

GC.standards[GC.standards$Sampling.Date == "2021-06-16" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-16" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-16" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-16" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-16",]



#################################    2021-06-21     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-21",]

GC.standards[GC.standards$Sampling.Date == "2021-06-21" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-21" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-21" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-21" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-21",]





#################################    2021-06-23     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-23",]


GC.standards[GC.standards$Sampling.Date == "2021-06-23" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-23" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-23" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-23" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;

GC.standards[GC.standards$Sampling.Date == "2021-06-23",]



#################################    2021-06-29     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-29",]


GC.standards[GC.standards$Sampling.Date == "2021-06-29" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-29" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-29" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-29" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-29",]




#################################    2021-07-02     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-07-02",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "25PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "25PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 125 , 0.25 )  ; 

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5 , 25 , 25 , 250 , 2500, 2500,  0.5, 25, 25) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "75PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "75PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 , 375 ,  0.75) ;

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5, 50, 50, 500 , 5000, 5000, 1, 50 , 50);


GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) 
             

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 2 ) ;

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-02" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-07-02",]




#################################    2021-07-07     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-07-07",]

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 125 , 0.25 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5,	250,	0.5) ;

             
GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "75PerSTDA", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 ,	375,	0.75 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5,	500 ,	1 ) ;
             
             
GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "50PerSTD",] 

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(25,	2500,	25) ;

             
GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "100PerSTD",] 

GC.standards[GC.standards$Sampling.Date == "2021-07-07" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(50,	5000,	50 );


GC.standards[GC.standards$Sampling.Date == "2021-07-07",]


GC.standards[,c("Sampling.Date" , "GC.Date")]




#################################    2021-07-15     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-07-15",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 ,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-15" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;

GC.standards[GC.standards$Sampling.Date == "2021-07-15",]





#################################    2021-07-20     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-07-20",]


GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-20" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-20",]

str(GC.standards[GC.standards$Sampling.Date == "2021-07-20",])




#################################    2021-07-30     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-07-30",]


GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-07-30" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-07-30",]



#################################    2021-08-05     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-08-05",]


GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-08-05" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-05",]



#################################    2021-08-12     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-08-12",]


GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-08-12" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-12",]




#################################    2021-08-19     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-08-19",]


GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-08-19" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-08-19",]




#################################    2021-09-02     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-09-02",]



GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-02" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-02",]



#################################    2021-09-17     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-09-17",]



GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-17" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-17",]





#################################    2021-09-29     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-09-29",]


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "0PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "0PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(0 , 0 , 0 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 125 , 0.25 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5,	250,	0.5 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75,	375,	0.75) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5,	500,	1) ;



GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(25,	2500,	25 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(50,	5000,	50 ) ;



GC.standards[GC.standards$Sampling.Date == "2021-09-29",]



#################################    2021-10-27     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-10-27",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(0 , 0 , 0 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 125 , 0.25 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5,	250,	0.5) ;



GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "75PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75,	375,	0.75) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5,	500,	1) ;



GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(25,	2500,	25) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(50,	5000,	50 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27",]






##############################################################################################################
#                           
#                           checking if no standard was missing 
#
###############################################################################################################

any(is.na(GC.standards$CO2.ppm))

which(is.na(GC.standards$CO2.ppm))

which(is.na(GC.standards$N2O.ppm))



GC.standards[which(is.na(GC.standards$CO2.ppm)), ]

#### Add the missing standard values #####


GC.standards[which(is.na(GC.standards$CO2.ppm)), c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" )] <- c(0 , 0 , 0 ) ;


#### check ####


any(is.na(GC.standards$CO2.ppm))

any(is.na(GC.standards$N2O.ppm))


###############################################################################################################
#                           
#                          Exploring the standards by date of analysis 
#
###############################################################################################################



###############################################################################################################
#                           
#                                            CO2   Standards
#
###############################################################################################################  

GC.standards$ANAL.DATE<-as.factor(GC.standards$GC.Date)  ;

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data= GC.standards, 
       
       pch = 16 , 
       
       main="CO2", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       key = simpleKey(
         
         text = levels(GC.standards$ANAL.DATE),
          
         points = TRUE,
         
         pch = 16,
         
         col = paletteer_d("ggsci::default_ucscgb") ,
         
         space = "right"
         
         ),
       
       panel  = function(x, y,...) {
         
         panel.xyplot(x, y,...)
         
         panel.text(x,y,
                    
                    labels = rownames(GC.standards))
         
         
       })
       
 


###############################################################################################################
#                           
#                                            N2O   Standards
#
###############################################################################################################  
  
GC.standards$ANAL.DATE<-as.factor(GC.standards$GC.Date)  ;

xyplot(N2O ~ N2O.ppm, groups = GC.Date, data= GC.standards, 
       
       pch = 16 , 
       
       main="N2O", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       key = simpleKey(
         
         text = levels(GC.standards$ANAL.DATE),
         
         points = TRUE,
         
         pch = 16,
         
         col = paletteer_d("ggsci::default_ucscgb") ,
         
         space = "right"
         
       ),
       
       panel  = function(x, y,...) {
         
         panel.xyplot(x, y,...)
         
         panel.text(x,y,
                    
                    labels = rownames(GC.standards))
         
         
       })




###############################################################################################################
#                           
#                                 Write curated Standards Calibration Data Set
#
###############################################################################################################  

# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
# 
# write.csv(x = GC.standards, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\Standards2021_",
# 
# Working.Date , ".csv"), row.names = T )

