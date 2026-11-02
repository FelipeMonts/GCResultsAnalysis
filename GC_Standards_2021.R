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

GC.standards <- read.csv(file = "FluxDataAnalysisResults\\StandardsDataset20212026_11_02_11_00_59.csv" )  ;


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
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 3)  ; 

GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5 , 250 , 0.5), each = 3 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75 , 375 , 0.75 ), each = 3) ;


GC.standards[GC.standards$Sampling.Date == "2021-06-14" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5 , 500 , 1 ) , each = 3) ;

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

###############    2021-07-07  Missing 20210702b3b407b2peakareas.pdf ????   ####################
###############    2021-07-07  Missing 20210702b3b407b2peakareas.pdf ????   ####################
###############    2021-07-07  Missing 20210702b3b407b2peakareas.pdf ????   ####################
###############    2021-07-07  Missing 20210702b3b407b2peakareas.pdf ????   ####################
###############    2021-07-07  Missing 20210702b3b407b2peakareas.pdf ????   ####################

# there is a problem with  50PerSTD and 100PerSTD ?? #


#         Sample.Name Position Vial     CH4      CO2      N2O                      File Sampling.Day Sampling.Date    GC.Date
# 1424    50PerSTD        5    1 101.687 4309.923 6559.847 20210707B3B4peakareas.pdf     20210707    2021-07-07 2021-08-01
# 1435   100PerSTD        6    1 126.926 6050.251 8951.515 20210707B3B4peakareas.pdf     20210707    2021-07-07 2021-08-01
#                    AnalysisName CH4.ppm CO2.ppm N2O.ppm
# 1424 20210707B3B4peakareas.pdf   25.00    2500   25.00
# 1435 20210707B3B4peakareas.pdf   50.00    5000   50.00






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


##### 20210929B1B2peakareas1 is missing!!!!!!!!  
##### 20210929B1B2peakareas2 is missing!!!!!!!!  


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


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTD",
             
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

any(is.na(GC.standards$CH4.ppm))



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
       
 
GC.standards[rownames(GC.standards) == "2605", ]  # 2021-09-02

GC.standards[rownames(GC.standards) == "2530", ]  # 2021-09-02

GC.standards[rownames(GC.standards) == "2616", ]  # 2021-09-20

GC.standards[rownames(GC.standards) == "2541", ]  # 2021-09-20



GC.standards[rownames(GC.standards) == "1435", ]  # 2021-07-02 GC.Date 2021-08-01

GC.standards[rownames(GC.standards) == "1310", ]  # 2021-07-02 GC.Date 2021-08-01

GC.standards[rownames(GC.standards) == "2810", ] # 2021-09-17  GC.Date 2021-09-22

GC.standards[rownames(GC.standards) == "2799", ] # 2021-09-17 GC.Date 2021-09-22

GC.standards[rownames(GC.standards) == "2724", ] # 2021-09-17 GC.Date 2021-09-22

GC.standards[rownames(GC.standards) == "2713", ] # 2021-09-17 GC.Date 2021-09-22
 
 
###### with out  2021-09-02   ########

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data= GC.standards[!GC.standards$GC.Date == "2021-09-02",], 
       
       pch = 16 , 
       
       main="CO2", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       key = simpleKey(
         
         text = levels(GC.standards[GC.standards$GC.Date != "2021-09-02", "ANAL.DATE"]),
         
         points = TRUE,
         
         pch = 16,
         
         col = paletteer_d("ggsci::default_ucscgb") ,
         
         space = "right"
         
       ),
       
       panel  = function(x, y,...) {
         
         panel.xyplot(x, y,...)
         
         panel.text(x,y,
                    
                    labels = rownames(GC.standards[GC.standards$GC.Date != "2021-09-02",]))
         
         
       })



###### with out  2021-08-01   ########

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data= GC.standards[!GC.standards$GC.Date == "2021-08-01",], 
       
       pch = 16 , 
       
       main="CO2", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       key = simpleKey(
         
         text = levels(GC.standards[GC.standards$GC.Date != "2021-08-01", "ANAL.DATE"]),
         
         points = TRUE,
         
         pch = 16,
         
         col = paletteer_d("ggsci::default_ucscgb") ,
         
         space = "right"
         
       ),
       
       panel  = function(x, y,...) {
         
         panel.xyplot(x, y,...)
         
         panel.text(x,y,
                    
                    labels = rownames(GC.standards[GC.standards$GC.Date != "2021-08-01",]))
         
         
       })



###### without  2021-09-22   ########

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data= GC.standards[!GC.standards$GC.Date == "2021-09-22",], 
       
       pch = 16 , 
       
       main="CO2", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       key = simpleKey(
         
         text = levels(GC.standards[GC.standards$GC.Date != "2021-09-22", "ANAL.DATE"]),
         
         points = TRUE,
         
         pch = 16,
         
         col = paletteer_d("ggsci::default_ucscgb") ,
         
         space = "right"
         
       ),
       
       panel  = function(x, y,...) {
         
         panel.xyplot(x, y,...)
         
         panel.text(x,y,
                    
                    labels = rownames(GC.standards[GC.standards$GC.Date != "2021-09-22",]))
         
         
       })

########### Remove standards with outliers  ######

# 2021-09-22 #
# 2021-08-01 #
# 2021-09-02 #

GC.standards_No_2021_09_22 <- GC.standards[!GC.standards$GC.Date == "2021-09-22" , ] ;

GC.standards_No_2021_09_22[GC.standards_No_2021_09_22$GC.Date == "2021-09-22" , ]

GC.standards_No_2021_08_01 <- GC.standards_No_2021_09_22[!GC.standards_No_2021_09_22$GC.Date == "2021-08-01" , ]

GC.standards_No_2021_08_01[GC.standards_No_2021_08_01$GC.Date == "2021-08-01" , ]

GC.standards_No_2021_09_02 <- GC.standards_No_2021_08_01[! GC.standards_No_2021_08_01$GC.Date == "2021-09-02" , ]

GC.standards_No_2021_09_02[GC.standards_No_2021_09_02$GC.Date == "2021-09-02" , ]


GC.standards <- GC.standards_No_2021_09_02 ;


rm(GC.standards_No_2021_09_22 , GC.standards_No_2021_08_01 ,  GC.standards_No_2021_09_02 )




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
#                                           Final set for CO2 Standards Calibration
#
###############################################################################################################  




####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards,
     
     main = "CO2 Calibration Set 2021",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

# text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2"],
#       
#       y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2.ppm"],
#       
#       labels = GC.standards[GC.standards$Sampling.Date == "2021-10-27" , "Sample.Name"],
#       
#       pos = 3);
# 
# 
# text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2"],
#       
#       y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2.ppm"],
#       
#       labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-10-27" ,]),
#       
#       pos = 1);
# 



###### Calculate calibration line CO2 #######

CAL.CO2.FinalSet <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards)



summary(CAL.CO2.FinalSet) 

abline(a = CAL.CO2.FinalSet$coefficients[1] ,
       
       b = CAL.CO2.FinalSet$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards$CO2),
      
      y = mean(GC.standards$CO2.ppm),
      
      labels = paste(round(CAL.CO2.FinalSet$coefficients[1],3), round(CAL.CO2.FinalSet$coefficients[2],3), sep = "+"))




###############################################################################################################
#                           
#                                           Final set for N2O Standards Calibration
#
###############################################################################################################  

  
####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards,
     
     main = "N2O Calibration Set 2021",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

# text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2"],
#       
#       y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2.ppm"],
#       
#       labels = GC.standards[GC.standards$Sampling.Date == "2021-10-27" , "Sample.Name"],
#       
#       pos = 3);
# 
# 
# text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2"],
#       
#       y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2.ppm"],
#       
#       labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-10-27" ,]),
#       
#       pos = 1);
# 



###### Calculate calibration line CO2 #######

CAL.N2O.FinalSet <- lm(N2O.ppm ~ N2O,
                       
                       data = GC.standards)



summary(CAL.N2O.FinalSet) 

abline(a = CAL.N2O.FinalSet$coefficients[1] ,
       
       b = CAL.N2O.FinalSet$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards$N2O),
      
      y = mean(GC.standards$N2O.ppm),
      
      labels = paste(round(CAL.N2O.FinalSet$coefficients[1],3), round(CAL.N2O.FinalSet$coefficients[2],3), sep = "+"))


###############################################################################################################
# 
#  Removing seriously deviated outliers in the figure above
# 
# 3350 , 3224 , 3213 , 3349 , 2021 , 1628 , 1935 , 2891 , 1946 
# 
###############################################################################################################







###############################################################################################################
#                           
#                                 Write curated Standards Calibration Data Set
#
###############################################################################################################  

# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
# 
# write.csv(x = GC.standards, file, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\StandardsDataset2021",
# 
# Working.Date , ".csv"), append = F , row.names = T )

