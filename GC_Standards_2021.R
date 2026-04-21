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

setwd("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis");


#### N2O Sampling year  #####

Year = 2021

# Year = 2022


PeakArea.results <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults" , Year ,".csv" ) , header = T) ;

###############################################################################################################
#                           
#                              Checking for duplicated records
#
###############################################################################################################

str(PeakArea.results)

head(PeakArea.results)

anyDuplicated(PeakArea.results, MARGIN = c(1,2))



##################################  Remove duplicates  ######################################################

PeakArea.results.1 <- PeakArea.results[!duplicated(PeakArea.results, MARGIN = c(1,2)),] ;

str(PeakArea.results.1) 

anyDuplicated(PeakArea.results.1, MARGIN = c(1,2))

PeakArea.results <- PeakArea.results.1  ;

str(PeakArea.results) 
###############################################################################################################
#                           
#                               Working with standards
#
###############################################################################################################


# getting the GC samples with standards together

GC.standards <- PeakArea.results[grep( pattern = "B" , x = PeakArea.results$Sample.Name, invert = T) ,] ;
   

# GC.Data <-  PeakArea.results[grep( pattern = "B" , x = PeakArea.results$Sample.Name, invert = F) ,] ;

which(is.na(GC.standards))

###############################################################################################################
#                           
#                            Checking Standard Values manually because 
#                The Standards Names PerSTDA and PerSTD are not consistent through out 2021, 
#                                therefore need to be selected manually!!!!!!
#
###############################################################################################################


str(GC.standards)


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
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(1.25 , 125 , 0.25 ) ;

GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "50PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(2.5 , 250 , 0.5 ) ;

  
GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "75PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(3.75 , 375 , 0.75 ) ;


GC.standards[GC.standards$Sampling.Date == "2021-05-28" & GC.standards$Sample.Name ==  "100PerSTD", 
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(5 , 500 , 1 ) ;

GC.standards[GC.standards$Sampling.Date == "2021-05-28",]

####### Plot Calibration line  for  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-05-28",],
     
     main = "2021-05-28",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-05-28", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-05-28", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-05-28" , "Sample.Name"],
      
      pos = 3);



###### Calculate calibration line CO2 #######

CAL.CO2.2021_05_28 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-05-28",],
                         
                         )


     
 summary(CAL.CO2.2021_05_28) 
 
 abline(a = CAL.CO2.2021_05_28$coefficients[1] ,
        
        b = CAL.CO2.2021_05_28$coefficients[2] , 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-05-28", c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == "2021-05-28", c("CO2.ppm")]),
       
       labels = paste(round(CAL.CO2.2021_05_28$coefficients[1],3), round(CAL.CO2.2021_05_28$coefficients[2],3), sep = "+")
       
 )

 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == "2021-05-28",],
      
      main = "2021-05-28",
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == "2021-05-28", "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == "2021-05-28", "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == "2021-05-28" , "Sample.Name"],
       
       pos = 3);
 
 
 
 ###### Calculate calibration line N2O #######
 
 CAL.N2O.2021_05_28 <- lm(N2O.ppm ~ N2O,
                          
                          data = GC.standards[GC.standards$Sampling.Date == "2021-05-28",],
                          
 )
 
 
 
 summary(CAL.N2O.2021_05_28) 
 
 abline(a = CAL.N2O.2021_05_28$coefficients[1] ,
        
        b = CAL.N2O.2021_05_28$coefficients[2] , 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-05-28", c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == "2021-05-28", c("N2O.ppm")]),
       
       labels = paste(round(CAL.N2O.2021_05_28$coefficients[1],3), round(CAL.N2O.2021_05_28$coefficients[2],3), sep = "+")
       
 )
 


#################################    2021-06-01  Missing   ############################################

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


####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-04",],
     
     main = "2021-06-04",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-04" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-04" ,]),
      
      pos = 1);


GC.standards[rownames(GC.standards) == "118", c("CH4.ppm" , "CO2.ppm" , "N2O.ppm")] <- c(1.25,     125,    0.25) ;

###### Calculate calibration line CO2 #######

CAL.CO2.2021_06_04 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-04",])



summary(CAL.CO2.2021_06_04) 

abline(a = CAL.CO2.2021_06_04$coefficients[1] ,
       
       b = CAL.CO2.2021_06_04$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-04", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-04", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_06_04$coefficients[1],3), round(CAL.CO2.2021_06_04$coefficients[2],3), sep = "+"))
      


####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-04",],
     
     main = "2021-06-04",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-04" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-04", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-04" ,]),
      
      pos = 1);



GC.standards <- GC.standards[!rownames(GC.standards) == "118", ] ;

###### Calculate calibration line #######

CAL.N2O.2021_06_04 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-04",])



summary(CAL.N2O.2021_06_04) 

abline(a = CAL.N2O.2021_06_04$coefficients[1] ,
       
       b = CAL.N2O.2021_06_04$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-04", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-04", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_06_04$coefficients[1],3), round(CAL.N2O.2021_06_04$coefficients[2],3), sep = "+"))

##### Remove outliers #######

GC.standards[rownames(GC.standards) == "118", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "118", ] ;

GC.standards[rownames(GC.standards) == "87", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "87", ] ;

#################################    2021-06-14     ############################################


GC.standards[GC.standards$Sampling.Date == "2021-06-14",]

#### rows with file 20210614B1B2peakareasMERGED and 20210614B1B2summaryreport1 are duplicated
#### removing rows 20210614B1B2summaryreport1 

GC.standards[!GC.standards$File == "20210614B1B2summaryreport1.pdf" ,]

GC.standards <- GC.standards[!GC.standards$File == "20210614B1B2summaryreport1.pdf" ,]

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

GC.standards[GC.standards$Sampling.Date == "2021-06-04",]


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


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTD",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "0PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- c(0 , 0 , 0 ) ;



GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "25PerSTDA",]

GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "25PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(1.25 , 125 , 0.25 ), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(2.5,	250,	0.5), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "75PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "75PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(3.75,	375,	0.75), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTDA",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTDA",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(5,	500,	1), each = 2) ;



GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "50PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(25,	2500,	25), each = 2) ;


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTD",]


GC.standards[GC.standards$Sampling.Date == "2021-10-27" & GC.standards$Sample.Name ==  "100PerSTD",
             
             c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm" ) ] <- rep(c(50,	5000,	50 ), each = 2) ;


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



GC.standards$ANAL.DATE<-as.factor(GC.standards$GC.Date)  ;

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data=GC.standards, type="b", main="CO2", auto.key = T)

xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Position , data=GC.standards, type="p",main="CO2", auto.key = T)

simpleKey(text = GC.standards$Position )


str(GC.standards)

###############################################################################################################
#                           
#                                            CO2   Standards
#
###############################################################################################################  
  
str(GC.standards)

GC.standards$Factor.Name <-as.factor(GC.standards$Sample.Name) ;

levels(GC.standards$Factor.Name)

  ####### 0 STD   #######

  
  plot(CO2 ~ CO2.ppm, 
       
       data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
  
  
  boxplot(CO2 ~ CO2.ppm, 
          
          data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
  
  
  plot(CO2 ~ ANAL.DATE, 
        
        data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
   
   
  boxplot(CO2 ~ ANAL.DATE , 
          
          data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ]  )
  
  
  plot(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "CO2" ],
       
       main = "0% STANDARD", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  ###### Add Median Line  #####
  
  median(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "CO2" ])
  
  abline(h = median(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "CO2" ]),
         
         col = "red",lwd = 3 )
  
 
  hist(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA", "CO2" ])
  
  range(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  c("CO2")])
  
  
  
  
  
    ####### 25 STD   #######
  
  plot(CO2 ~ CO2.ppm, 
       
       data = GC.standards[GC.standards$Factor.Name == "25PerSTDA" | GC.standards$Factor.Name == "25PerSTD" ,  ]) 
  
  
  boxplot(CO2 ~ CO2.ppm, 
          
          data = GC.standards[GC.standards$Factor.Name == "25PerSTDA" ,  ] )
  
  
  plot(CO2 ~ ANAL.DATE, 
       
       data = GC.standards[GC.standards$Factor.Name == "25PerSTDA" | GC.standards$Factor.Name == "25PerSTD" ,  ], 
       
       main = ("25% STANDARD"));
       
  plot(GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "CO2" ],
       
       main = "25% STANDARD", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;   
  
  text(GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "Sample.Name" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ; 
  
  grid(nx = 5, ny = 10 , col = "red")
  
  abline(h = median(GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "CO2" ]),
         
         col = "red",lwd = 3 ) 
  
  
  hist(GC.standards[GC.standards$Factor.Name == "25PerSTDA" , "CO2" ])
  
  
  
  ####### 50 STD   #######
  
  plot(CO2 ~ CO2.ppm,
       
       data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ])
  
  
  boxplot(CO2 ~ CO2.ppm,
          
          data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ]  )
  
  
  plot(CO2 ~ ANAL.DATE,
       
       data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ])
  
  
  hist(GC.standards[GC.standards$Factor.Name == "50PerSTD" , "CO2" ])
  
  
  plot(CO2 ~ CO2.ppm, 
       
       data = GC.standards[GC.standards$Factor.Name == "50PerSTDA" ,  ]) 
  
  
  boxplot(CO2 ~ CO2.ppm, 
          
          data = GC.standards[GC.standards$Factor.Name == "50PerSTDA" ,  ]  )
  
  
  hist(GC.standards[GC.standards$Factor.Name == "50PerSTDA" , "CO2" ])
  
  
  #### Combined "50PerSTDA" and "50PerSTD"  #
  
  
  plot(CO2 ~ ANAL.DATE,
       
       data = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" ,  ],
       
       main = "50% STANDARD")
  
  
  
  plot(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       main = "50% STANDARD", ylab = NA);
  
  
  text(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  text(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "Sample.Name" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
 
  
  ############  50% STANDARD < 6000 ####################
  
  plot(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 <= 6000,"CO2"],
       
       main = "50% STANDARD < 6000", ylab = NA);  
  
  text(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 <= 6000,"CO2"],
       
       labels = GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$CO2 <= 6000 , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 <= 6000,"CO2"]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 <= 6000,"CO2"]),
                         
                         labels = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 <= 6000,"CO2"] ),
       
       pos = 4, offset = 2);
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  
  
  
  ############  50% STANDARD > 6000 ####################
  
  plot(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 >= 6000,"CO2"],
       
       main = "50% STANDARD > 6000", ylab = NA);  
  
  text(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 >= 6000,"CO2"],
       
       labels = GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$CO2 >= 6000 , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 >= 6000,"CO2"]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 >= 6000,"CO2"]),
       
       labels = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA")  & GC.standards$CO2 >= 6000,"CO2"] ),
       
       pos = 4, offset = 2);
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
 
  
  plot(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       main = "50% STANDARD", ylab = NA, ylim = c(1000, 3000));
  
  text(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  
  plot(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       main = "50% STANDARD", ylab = NA, ylim = c(3000, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  ####### 75 STD   #######
  
  plot(CO2 ~ ANAL.DATE,
       
       data = GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" ,  ],
       
       main = "75% STANDARD")
  
  plot(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "CO2"  ],
       
       main = "75% STANDARD" , ylab = NA) # ylim = c(3000, 15000));) 
  
  text(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "CO2"  ],
       
       labels = GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "ANAL.DATE"  ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  abline(h = median(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "CO2"  ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "CO2"  ]),
       
       labels =  median(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "CO2"  ]),
       
       pos = 4, offset = 2);
  
  
  
  ####### 100 STD   #######
  
  plot(CO2 ~ CO2.ppm, 
       
       data = GC.standards[GC.standards$Factor.Name == "100PerSTD" ,  ])
  
  
  boxplot(CO2 ~ CO2.ppm, 
          
          data = GC.standards[GC.standards$Factor.Name == "100PerSTD" ,  ]  )
  
  hist(GC.standards[GC.standards$Factor.Name == "100PerSTD" , "CO2" ])
  
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  ])
  
  boxplot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  ]  )
  
  hist(GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  "CO2"   ])
  
  #### Combined "100PerSTDA" and "100PerSTD"  #
  
  plot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD",  ],
       
       main= "100% STANDARD");
  
  plot(GC.standards[GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD", "CO2" ],
       
       main = "100% STANDARD", ylab = NA );
  
  text(GC.standards[GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD", "CO2" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red") ;
  
  
  ##### 100% Standards < 10000
  
  plot(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 <= 10000, "CO2" ],
       
       main = "100% STANDARD < 10000", ylab = NA );
  
  text(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 <= 10000, "CO2" ],
       
       labels = GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 <= 10000, "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red") ;
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 <= 10000, "CO2" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 <= 10000, "CO2" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 <= 10000, "CO2" ]),
       
       pos = 4, offset = 2);
  
  
  
  ##### 100% Standards > 10000
  
  plot(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 >= 10000, "CO2" ],
       
       main = "100% STANDARD > 10000", ylab = NA );
  
  text(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 >= 10000, "CO2" ],
       
       labels = GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 >= 10000, "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red") ;
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 >= 10000, "CO2" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 >= 10000, "CO2" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTDA" | GC.standards$Factor.Name == "100PerSTD") & GC.standards$CO2 >= 10000, "CO2" ]),
       
       pos = 4, offset = 2);
  
  
  
  ###############################################################################################################
  #                           
  #                                            N2O   Standards
  #
  ###############################################################################################################  
  
  

  ####### 0 STD   #######
  
  levels(GC.standards$Factor.Name)
  
  GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA","N2O"  ]
  
  plot(N2O ~ N2O.ppm, 
       
       data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
  
  
  boxplot(N2O ~ N2O.ppm, 
          
          data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
  
  
  plot(N2O ~ ANAL.DATE, 
       
       data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
  
  
  boxplot(CO2 ~ ANAL.DATE , 
          
          data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ]  )
  
 
  plot(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "N2O" ],
       
       main = "0% STANDARD N20", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "N2O" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA"), "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA"), "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA"), "N2O" ]),
       
       pos = 4, offset = 2);
  
   
  
  ####### 25%  STD   #######
  
  levels(GC.standards$Factor.Name)
  
  plot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA",  ]) 
  
  boxplot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA", ] )
  
  
  plot(GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "N2O" ],
       
       main = "25% STANDARD N20", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "N2O" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA"), "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA"), "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "25PerSTD" | GC.standards$Factor.Name == "25PerSTDA"), "N2O" ]),
       
       pos = 4, offset = 2);
  
  
  
  ####### 50%  STD   #######
  
  levels(GC.standards$Factor.Name)
  
  plot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA",  ]) 
  
  boxplot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA", ] )
  
  
  plot(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "N2O" ],
       
       main = "50% STANDARD N20", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "N2O" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA"), "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA"), "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA"), "N2O" ]),
       
       pos = 4, offset = 2);
  
  ####### 50%  STD >=  11000####### 
  
  
  plot(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O >= 11000 , "N2O" ],
       
       main = "50% STANDARD N20 >= 11000", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O >= 11000 , "N2O" ],
       
       labels = GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O >= 11000  , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O >= 11000 , "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O >= 11000 , "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O >= 11000 , "N2O" ]),
       
       pos = 4, offset = 2);
  
  
  ####### 50%  STD <=  11000####### 
  
  
  plot(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O <= 11000 , "N2O" ],
       
       main = "50% STANDARD N20 <= 11000", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O <= 11000 , "N2O" ],
       
       labels = GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O <= 11000  , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O <= 11000 , "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O <= 11000 , "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "50PerSTD" | GC.standards$Factor.Name == "50PerSTDA") & GC.standards$N2O <= 11000 , "N2O" ]),
       
       pos = 4, offset = 2);
  
  
  ####### 75%  STD   #######
  
  levels(GC.standards$Factor.Name)
  
  plot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA",  ]) 
  
  boxplot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA", ] )
  
  
  plot(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "N2O" ],
       
       main = "75% STANDARD N20", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "N2O" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA"), "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA"), "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "75PerSTD" | GC.standards$Factor.Name == "75PerSTDA"), "N2O" ]),
       
       pos = 4, offset = 2);
  
  
  
  ####### 100%  STD   #######
  
  levels(GC.standards$Factor.Name)
  
  plot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA",  ]) 
  
  boxplot(N2O ~ N2O.ppm, data = GC.standards[GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA", ] )
  
  
  plot(GC.standards[GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA" , "N2O" ],
       
       main = "100% STANDARD N20", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA" , "N2O" ],
       
       labels = GC.standards[GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA" , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA"), "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA"), "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA"), "N2O" ]),
       
       pos = 4, offset = 2);
  
  ####### 100%  STD >= 20000   #######
  
  
  plot(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O >= 20000 , "N2O" ],
       
       main = "100% STANDARD N20 >= 20000", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O >= 20000 , "N2O" ],
       
       labels = GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O >= 20000   , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O >= 20000   , "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O >= 20000   , "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O >= 20000 , "N2O" ]),
       
       pos = 4, offset = 2);
  
  
  ####### 100%  STD < 20000   #######
  
  
  plot(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O <= 20000 , "N2O" ],
       
       main = "100% STANDARD N20 < 20000", ylab = NA) #, ylim = c(0, 15000));
  
  text(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O <= 20000 , "N2O" ],
       
       labels = GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O <= 20000   , "ANAL.DATE" ],
       
       srt= 90, cex = 0.5, pos = 3, offset = 2) ;
  
  grid(nx = 5, ny = 10 , col = "red")
  
  
  abline(h = median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O <= 20000   , "N2O" ]),
         
         col = "red",lwd = 3 ) 
  
  text(x = 0, y =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O <= 20000   , "N2O" ]),
       
       labels =  median(GC.standards[(GC.standards$Factor.Name == "100PerSTD" | GC.standards$Factor.Name == "100PerSTDA") & GC.standards$N2O <= 20000 , "N2O" ]),
       
       pos = 4, offset = 2);
  
  
  
  
###############################################################################################################  
# 
# The Standards Names PerSTDA and PerSTD are not consistent through out 2021, therefore need to be selected manually!!!!!!
# 
###############################################################################################################   
  
  
  