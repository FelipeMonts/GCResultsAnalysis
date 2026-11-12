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


#source(file =  "GC_Standards_2021.R" , echo = T)



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

GC.standards <- read.csv(file = "FluxDataAnalysisResults\\Standards2021_2026_11_04_09_19_14.csv" )  ;


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

GC.standards[GC.standards$Sampling.Date == "2021-09-29", c( "X" , "Sample.Name" , "Position" ,
                                                            
                                                            "Sampling.Date" , "CH4.ppm" , "CO2.ppm" , "N2O.ppm")]


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "0PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "0PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(0 , 0 , 0 ) ;

##### The are two "25PerSTDA" points in this set therefore the filling pattern needs to be changed

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 1.25 , 125 , 125 , 0.25 , 0.25 ) ;

##### The are two "50PerSTDA" points in this set therefore the filling pattern needs to be changed

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5, 2.5 , 250 ,	250,	0.5 , 0.5 ) ;

##### The are two "75PerSTDA" points in this set therefore the filling pattern needs to be changed

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "75PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 , 3.75 , 375 ,	375 ,	0.75 , 0.75) ;

##### The are two "100PerSTDA" points in this set therefore the filling pattern needs to be changed

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5 , 5, 500 ,	500 , 1 ,	1) ;

##### The are two "50PerSTD" points in this set therefore the filling pattern needs to be changed

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(25 , 25 , 2500, 2500 , 25 , 25 ) ;


##### The are two "100PerSTD" points in this set therefore the filling pattern needs to be changed


GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-09-29" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(50 , 50 , 5000 , 5000 ,	50 , 50 ) ;



GC.standards[GC.standards$Sampling.Date == "2021-09-29",]



#################################    2021-10-27     ############################################





GC.standards[GC.standards$Sampling.Date == "2021-10-27",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27", c( "X" , "Sample.Name" , "Position" ,
                                                            
                                                            "Sampling.Date" , "CH4.ppm" , "CO2.ppm" , "N2O.ppm")]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(0 , 0 , 0 ) ;

##### The are two "25PerSTDA" points in this set therefore the filling pattern needs to be changed

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 1.25 , 125 , 125 , 0.25 ,0.25 ) ;

##### The are two "50PerSTDA" points in this set therefore the filling pattern needs to be changed


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5, 2.5 , 250 , 250 ,	0.5 , 0.5) ;


##### The are two "75PerSTDA" points in this set therefore the filling pattern needs to be changed


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "75PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 , 3.75 , 375 ,	375 ,	0.75 , 0.75) ;


##### The are two "100PerSTDA" points in this set therefore the filling pattern needs to be changed


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5 , 5 , 500 ,	500 ,	1 , 1) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(0 , 0 , 0) ;


##### The are two "50PerSTD" points in this set therefore the filling pattern needs to be changed


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(25 , 25 , 2500 , 2500 , 25 ,	25) ;


##### The are two "100PerSTD" points in this set therefore the filling pattern needs to be changed


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(50 , 50 ,	5000 , 5000 ,	50 , 50 ) ;











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
       
       # xlim = c(-10 , 500),
       
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


##############################################################################################################
#                           
#      Plot all the standard calibration points with the quantile regression median and and 95% quantiles as background 
# 
#     and 95% quantiles as background  to detect outliers
#
###############################################################################################################

###############################################################################################################
#                           
#                                            CO2   Standards
#
###############################################################################################################  

str(GC.standards)



GC.standards$ANAL.DATE <- as.factor(GC.standards$GC.Date)  ;

GC.standards$Sampling.Day <- as.factor(GC.standards$Sampling.Day)  ;


xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data= GC.standards, 
       
       pch = 16 , 
       
       main="CO2", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       auto.key =  list( space = "right" , points = T , lines = F ,title = "Sampling Day" ,  type = "l", pch = 16, fill = T) );

# panel  = function(x, y,...) {
#   
#   panel.xyplot(x, y,...)
#   
#   panel.text(x,y,
#              
#              labels = rownames(GC.standards))
#   
#   
# })

simpleKey(text = levels(GC.standards$Sampling.Day),
          
          points = T,
          
          rectangles = T ,
          
          lines = F ,
          
          pch = 16)



xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data= GC.standards, 
       
       pch = 16 , 
       
       main="CO2", 
       
       color.palette = paletteer_d("ggsci::default_ucscgb"),
       
       key = simpleKey(
         
         text = levels(GC.standards$Sampling.Day),
         
         points = TRUE,
         
         col = paletteer_d("ggsci::default_ucscgb") ,
         
         space = "right" )) ;

# panel  = function(x, y,...) {
#   
#   panel.xyplot(x, y,...)
#   
#   panel.text(x,y,
#              
#              labels = rownames(GC.standards))
#   
#   
# })

##### Do Quantile Regression  ####


Quantile.Reg.CO2 <- rq( CO2 ~ CO2.ppm, data = GC.standards, tau = c(0.05 , 0.25, 0.50 , 0.75 , 0.95)) ;

summary(Quantile.Reg.CO2)

plot(NULL, xlim = c( min(GC.standards$CO2.ppm), max(GC.standards$CO2.ppm)),
     
     ylim = c(min(GC.standards$CO2), max(GC.standards$CO2)),
     
     main = "CO2", xlab = "CO2" , ylab = "CO2.ppm" ) ;

####### calculations to plot quantile bands  #######

range(GC.standards$CO2.ppm)[1]
min(GC.standards$CO2.ppm)


##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards$CO2.ppm) , max(GC.standards$CO2.ppm) ,max(GC.standards$CO2.ppm) , min(GC.standards$CO2.ppm)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "gray")

Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1]

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2],
       
       col = "red1")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2],
       
       col = "red1")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.25"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.25"][2],
       
       col = "red4")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.75"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.75"][2],
       
       col = "red4")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.50"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.50"][2],
       
       col = "blue")
text(x = GC.standards$CO2.ppm , y = GC.standards$CO2, labels = row.names(GC.standards) )


###############################################################################################################
#                           
#                                 Identify points outside the 95 percentile
#
###############################################################################################################

range(GC.standards$CO2) 

range(GC.standards$CO2.ppm)

plot(NULL, xlim = range(GC.standards$CO2.ppm),
     
     ylim = range(GC.standards$CO2) ,
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;


# plot(NULL, xlim = c( -10, 600),
# 
#      ylim = c(0, 5000),
# 
#      main = "CO2", xlab = "CO2" , ylab = "CO2.ppm" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)

points(CO2 ~ CO2.ppm , data = GC.standards,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "gray")

Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1]

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2],
       
       col = "red1")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2],
       
       col = "red1")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.25"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.25"][2],
       
       col = "red4")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.75"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.75"][2],
       
       col = "red4")

abline(a = Quantile.Reg.CO2$coefficients[,"tau= 0.50"][1] , b = Quantile.Reg.CO2$coefficients[,"tau= 0.50"][2],
       
       col = "blue")
text(x = GC.standards$CO2.ppm , y = GC.standards$CO2, labels = row.names(GC.standards) )


####### give each point a unique id based on the row.name  ######

GC.standards$P.ID <- row.names.data.frame(GC.standards) ;


#### points that are outside of the 95 percentile ###

OutRange.Points.CO2 <- as.character( c(123 , 104 ,  178 , 192 , 44 , 57 , 51 , 15 ) );

anyDuplicated(OutRange.Points.CO2)

anyDuplicated(OutRange.Points.CO2)

GC.standards[GC.standards$P.ID %in% OutRange.Points.CO2 ,]

OutRange.Points.CO2.1 <- GC.standards[GC.standards$P.ID %in% OutRange.Points.CO2 ,];


OutRange.Points.CO2.1 <- OutRange.Points.CO2.1 [order(OutRange.Points.CO2.1$Sampling.Date) ,]


str(GC.standards)

str(OutRange.Points.CO2.1 )

points(CO2 ~ CO2.ppm , data = OutRange.Points.CO2.1 ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")






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





str(GC.standards)

xyplot(CO2 ~ CO2.ppm,
       
       data = GC.standards, 
       
       main = "CO2",
       
       groups = ANAL.DATE,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue")
                       
         )
       ))


xyplot(N2O ~ N2O.ppm,
       
       data = GC.standards, 
       
       main = "N2O",
       
       groups = ANAL.DATE,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue")
                       
         )
       ))


######################################################################################################



##### Do Quantile Regression  ####


Quantile.Reg.N2O <- rq( N2O ~ N2O.ppm, data = GC.standards, tau = c(0.05 , 0.25, 0.50 , 0.75 , 0.95)) ;

Quantile.Reg.N2O

summary(Quantile.Reg.N2O)

plot(NULL, xlim = range(GC.standards$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;


# plot(NULL, xlim =c(-10,10),
# 
#      ylim = c(0,5000),
# 
#       main = "N2O", xlab = "N2O" , ylab = "N2O.ppm" ) ;

####### calculations to plot quantile bands  #######

range(GC.standards$N2O.ppm)
min(GC.standards$N2O.ppm)

range(GC.standards$N2O)

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards$N2O.ppm, na.rm = T) , max(GC.standards$N2O.ppm , na.rm = T) , 
               
               max(GC.standards$N2O.ppm , na.rm = T) , min(GC.standards$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1], max(GC.standards$N2O),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

range(polygon.y)

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "gray")

Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1]

abline(a = Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] , b = Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2],
       
       col = "red1")

abline(a = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] , b = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2],
       
       col = "red1")

abline(a = Quantile.Reg.N2O$coefficients[,"tau= 0.25"][1] , b = Quantile.Reg.N2O$coefficients[,"tau= 0.25"][2],
       
       col = "red4")

abline(a = Quantile.Reg.N2O$coefficients[,"tau= 0.75"][1] , b = Quantile.Reg.N2O$coefficients[,"tau= 0.75"][2],
       
       col = "red4")

abline(a = Quantile.Reg.N2O$coefficients[,"tau= 0.50"][1] , b = Quantile.Reg.N2O$coefficients[,"tau= 0.50"][2],
       
       col = "blue")
text(x = GC.standards$N2O.ppm , y = GC.standards$N2O, labels = GC.standards$P.ID )



######################################################################################################
# Some the standards , 179 , 180 , 181 , 182  Very strange #############
# 
# 
# The problem is with the standards in 20210929B3B4peakareas.pdf 
# 
# The pdf reports are correct  
# 
# The are two " points in this set therefore the filling pattern needs to be changed
# 
# This was corrected on 11/12/2026 
######################################################################################################

GC.standards[which( GC.standards$P.ID == "179" |  GC.standards$P.ID == "180" | 
                      
                      GC.standards$P.ID == "181" |   GC.standards$P.ID == "182") ,  ]







##### add outrange CO2 points ####

points(N2O ~ N2O.ppm , data = OutRange.Points.CO2.1,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

##### There are points outside range in N2O that have not selected from  points outrange in CO2  ####

OutRange.Points.N2O <- as.character( c(59 , 104 ,  157 , 103 ) );

anyDuplicated(OutRange.Points.N2O)

anyDuplicated(OutRange.Points.N2O)

GC.standards[GC.standards$P.ID %in% OutRange.Points.N2O ,]

OutRange.Points.N2O.1 <- GC.standards[GC.standards$P.ID %in% OutRange.Points.N2O ,];


OutRange.Points.N2O.1 <- OutRange.Points.N2O.1 [order(OutRange.Points.N2O.1$Sampling.Date) ,]


str(GC.standards)

str(OutRange.Points.N2O.1 )

points(N2O ~ N2O.ppm , data = OutRange.Points.N2O.1 ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "blue")




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

