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

##### Remove outliers row names 118, 85 ,87 #######

GC.standards[rownames(GC.standards) == "118", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "118", ] ;

GC.standards[rownames(GC.standards) == "87", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "87", ] ;

GC.standards[rownames(GC.standards) == "85", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "85", ] ;

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




####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-14",],
     
     main = "2021-06-14",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-14" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-14" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_06_14 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-14",])



summary(CAL.CO2.2021_06_14) 

abline(a = CAL.CO2.2021_06_14$coefficients[1] ,
       
       b = CAL.CO2.2021_06_14$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-14", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-14", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_06_14$coefficients[1],3), round(CAL.CO2.2021_06_14$coefficients[2],3), sep = "+"))



####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-14",],
     
     main = "2021-06-14",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-14" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-14", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-14" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_06_14 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-14",])



summary(CAL.N2O.2021_06_14) 

abline(a = CAL.N2O.2021_06_14$coefficients[1] ,
       
       b = CAL.N2O.2021_06_14$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-14", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-14", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_06_14$coefficients[1],3), round(CAL.N2O.2021_06_14$coefficients[2],3), sep = "+"))




##### Remove outliers row names 472 , 433 #######

GC.standards[rownames(GC.standards) == "472", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "472", ] ;

GC.standards[rownames(GC.standards) == "433", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "433", ] ;


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


####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-16",],
     
     main = "2021-06-16",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-16" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-16" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_06_16 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-16",])



summary(CAL.CO2.2021_06_16) 

abline(a = CAL.CO2.2021_06_16$coefficients[1] ,
       
       b = CAL.CO2.2021_06_16$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-16", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-16", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_06_16$coefficients[1],3), round(CAL.CO2.2021_06_16$coefficients[2],3), sep = "+"))






####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-16",],
     
     main = "2021-06-16",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-16" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-16", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-16" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_06_16 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-16",])



summary(CAL.N2O.2021_06_16) 

abline(a = CAL.N2O.2021_06_16$coefficients[1] ,
       
       b = CAL.N2O.2021_06_16$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-16", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-16", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_06_16$coefficients[1],3), round(CAL.N2O.2021_06_16$coefficients[2],3), sep = "+"))




##### Remove outliers row names 500 #######

GC.standards[rownames(GC.standards) == "500", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "500", ] ;



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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-21",],
     
     main = "2021-06-21",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-21" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-21" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_06_21 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-21",])



summary(CAL.CO2.2021_06_21) 

abline(a = CAL.CO2.2021_06_21$coefficients[1] ,
       
       b = CAL.CO2.2021_06_21$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-21", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-21", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_06_21$coefficients[1],3), round(CAL.CO2.2021_06_21$coefficients[2],3), sep = "+"))




####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-21",],
     
     main = "2021-06-21",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-21" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-21", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-21" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_06_21 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-21",])



summary(CAL.N2O.2021_06_21) 

abline(a = CAL.N2O.2021_06_21$coefficients[1] ,
       
       b = CAL.N2O.2021_06_21$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-21", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-21", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_06_21$coefficients[1],3), round(CAL.N2O.2021_06_21$coefficients[2],3), sep = "+"))





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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-23",],
     
     main = "2021-06-23",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-23" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-23" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_06_23 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-23",])



summary(CAL.CO2.2021_06_23) 

abline(a = CAL.CO2.2021_06_23$coefficients[1] ,
       
       b = CAL.CO2.2021_06_23$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-23", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-23", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_06_23$coefficients[1],3), round(CAL.CO2.2021_06_23$coefficients[2],3), sep = "+"))



####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-23",],
     
     main = "2021-06-23",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-23" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-23", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-23" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_06_23 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-23",])



summary(CAL.N2O.2021_06_23) 

abline(a = CAL.N2O.2021_06_23$coefficients[1] ,
       
       b = CAL.N2O.2021_06_23$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-23", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-23", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_06_23$coefficients[1],3), round(CAL.N2O.2021_06_23$coefficients[2],3), sep = "+"))



##### Remove outliers row names 945 #######

GC.standards[rownames(GC.standards) == "945", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "945", ] ;




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


####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-29",],
     
     main = "2021-06-29",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-29" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-29" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_06_29 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-29",])



summary(CAL.CO2.2021_06_29) 

abline(a = CAL.CO2.2021_06_29$coefficients[1] ,
       
       b = CAL.CO2.2021_06_29$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-29", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-29", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_06_29$coefficients[1],3), round(CAL.CO2.2021_06_29$coefficients[2],3), sep = "+"))



####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-06-29",],
     
     main = "2021-06-29",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-06-29" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-06-29", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-06-29" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_06_29 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-06-29",])



summary(CAL.N2O.2021_06_29) 

abline(a = CAL.N2O.2021_06_29$coefficients[1] ,
       
       b = CAL.N2O.2021_06_29$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-29", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-06-29", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_06_29$coefficients[1],3), round(CAL.N2O.2021_06_29$coefficients[2],3), sep = "+"))




##### Remove outliers row names 1091 , 1113 #######

GC.standards[rownames(GC.standards) == "1091", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "1091", ] ;

GC.standards[rownames(GC.standards) == "1113", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "1113", ] ;


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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-02",],
     
     main = "2021-07-02",
     
     col = "blue",
     
     #ylim = c(0,600),
     
     #xlim = c(0,2500),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-02" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-02" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_07_02 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-02",])



summary(CAL.CO2.2021_07_02) 

abline(a = CAL.CO2.2021_07_02$coefficients[1] ,
       
       b = CAL.CO2.2021_07_02$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-02", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-02", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_07_02$coefficients[1],3), round(CAL.CO2.2021_07_02$coefficients[2],3), sep = "+"))





####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-02",],
     
     main = "2021-07-02",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-02" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-02", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-02" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_07_02 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-02",])



summary(CAL.N2O.2021_07_02) 

abline(a = CAL.N2O.2021_07_02$coefficients[1] ,
       
       b = CAL.N2O.2021_07_02$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-02", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-02", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_07_02$coefficients[1],3), round(CAL.N2O.2021_07_02$coefficients[2],3), sep = "+"))




##### Remove outliers row names 1376  #######

GC.standards[rownames(GC.standards) == "1376", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "1376", ] ;



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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-07",],
     
     main = "2021-07-07",
     
     col = "blue",
     
     #ylim = c(0,1000),
     
     #xlim = c(0,2000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-07" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-07" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_07_07 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-07",])



summary(CAL.CO2.2021_07_07) 

abline(a = CAL.CO2.2021_07_07$coefficients[1] ,
       
       b = CAL.CO2.2021_07_07$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-07", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-07", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_07_07$coefficients[1],3), round(CAL.CO2.2021_07_07$coefficients[2],3), sep = "+"))



####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-07",],
     
     main = "2021-07-07",
     
     col = "blue",
     
     xlim = c(0,600) ,
     
     ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-07" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-07", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-07" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_07_07 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-07",])



summary(CAL.N2O.2021_07_07) 

abline(a = CAL.N2O.2021_07_07$coefficients[1] ,
       
       b = CAL.N2O.2021_07_07$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-07", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-07", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_07_07$coefficients[1],3), round(CAL.N2O.2021_07_07$coefficients[2],3), sep = "+"))




##### Remove outliers row names 1424 , 1435 #######

GC.standards[rownames(GC.standards) == "1424", ]

# GC.standards <- GC.standards[!rownames(GC.standards) == "1424", ] ;

GC.standards[rownames(GC.standards) == "1435", ]

# GC.standards <- GC.standards[!rownames(GC.standards) == "1435", ] ;






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






####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-15",],
     
     main = "2021-07-15",
     
     col = "blue",
     
    # ylim = c(0,600),
     
    # xlim = c(1000,2000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-15" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-15" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_07_15 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-15",])



summary(CAL.CO2.2021_07_15) 

abline(a = CAL.CO2.2021_07_15$coefficients[1] ,
       
       b = CAL.CO2.2021_07_15$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-15", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-15", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_07_15$coefficients[1],3), round(CAL.CO2.2021_07_15$coefficients[2],3), sep = "+"))





####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-15",],
     
     main = "2021-07-15",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-15" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-15", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-15" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_07_15 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-15",])



summary(CAL.N2O.2021_07_15) 

abline(a = CAL.N2O.2021_07_15$coefficients[1] ,
       
       b = CAL.N2O.2021_07_15$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-15", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-15", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_07_15$coefficients[1],3), round(CAL.N2O.2021_07_15$coefficients[2],3), sep = "+"))



# there is a problem with  1477#

##### Remove outliers row names 1477 #######

GC.standards[rownames(GC.standards) == "1477", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "1477", ] ;






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


####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-20",],
     
     main = "2021-07-20",
     
     col = "blue",
     
    # ylim = c(0,600),
     
    # xlim = c(1000,2000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-20" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-20" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_07_20 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-20",])



summary(CAL.CO2.2021_07_20) 

abline(a = CAL.CO2.2021_07_20$coefficients[1] ,
       
       b = CAL.CO2.2021_07_20$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-20", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-20", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_07_20$coefficients[1],3), round(CAL.CO2.2021_07_20$coefficients[2],3), sep = "+"))




####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-20",],
     
     main = "2021-07-20",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-20" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-20", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-20" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_07_20 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-20",])



summary(CAL.N2O.2021_07_20) 

abline(a = CAL.N2O.2021_07_20$coefficients[1] ,
       
       b = CAL.N2O.2021_07_20$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-20", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-20", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_07_20$coefficients[1],3), round(CAL.N2O.2021_07_20$coefficients[2],3), sep = "+"))



# there is a problem with  50PerSTD and 100PerSTD ?? #

#       Sample.Name Position Vial     CH4      CO2      N2O                      File Sampling.Day Sampling.Date    GC.Date
# 1682    50PerSTD        5    1 112.476  4034.716  4870.992 20210720B1B2peakareas.pdf     20210720    2021-07-20 2021-08-06
# 1693   100PerSTD        6    1  15.614  2075.896   373.643 20210720B1B2peakareas.pdf     20210720    2021-07-20 2021-08-06
# 1768    50PerSTD        5    1 583.846 25807.623 44608.263 20210720B3B4peakareas.pdf     20210720    2021-07-20 2021-08-06
# 1779   100PerSTD        6    1 329.127 14744.544 24922.562 20210720B3B4peakareas.pdf     20210720    2021-07-20 2021-08-06
# 
#       AnalysisName CH4.ppm CO2.ppm N2O.ppm
# 
# 1682 20210720B1B2peakareas.pdf   25.00    2500   25.00
# 1693 20210720B1B2peakareas.pdf   50.00    5000   50.00
# 1768 20210720B3B4peakareas.pdf   25.00    2500   25.00
# 1779 20210720B3B4peakareas.pdf   50.00    5000   50.00

# 1682 and 1693 ;  1768 and 1779 seemed  switched; 50 is 100 and 100 is 50.
# 


# Switch records  1682, 1693,  1768 and 1779

GC.standards[rownames(GC.standards) == "1682", ]

GC.standards[rownames(GC.standards) == "1682", "Sample.Name"] <- "100PerSTD"

GC.standards[rownames(GC.standards) == "1682", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c( 50,  5000, 50 )


GC.standards[rownames(GC.standards) == "1693", ]

GC.standards[rownames(GC.standards) == "1693", "Sample.Name"] <- "50PerSTD"

GC.standards[rownames(GC.standards) == "1682", c("CH4.ppm","CO2.ppm" ,"N2O.ppm") ] <- c( 25,  2500, 25 )


GC.standards[rownames(GC.standards) == "1768", ]

GC.standards[rownames(GC.standards) == "1768", c( "Sample.Name" )] <- "100PerSTD" 

GC.standards[rownames(GC.standards) == "1768", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c(50,  5000, 50 )


GC.standards[rownames(GC.standards) == "1779", ]

GC.standards[rownames(GC.standards) == "1779", c( "Sample.Name")] <- "50PerSTD"

GC.standards[rownames(GC.standards) == "1779", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c(25,  2500, 25)


##### Remove 1693 and 1682 #####

##### Remove outliers row names 1682 ,1693 #######

GC.standards[rownames(GC.standards) == "1682", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "1682", ] ;

GC.standards[rownames(GC.standards) == "1693", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "1693", ] ;











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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-30",],
     
     main = "2021-07-30",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-30" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-30" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_07_30 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-30",])



summary(CAL.CO2.2021_07_30) 

abline(a = CAL.CO2.2021_07_30$coefficients[1] ,
       
       b = CAL.CO2.2021_07_30$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-30", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-30", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_07_30$coefficients[1],3), round(CAL.CO2.2021_07_30$coefficients[2],3), sep = "+"))



####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-07-30",],
     
     main = "2021-07-30",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-07-30" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-07-30", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-07-30" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_07_30 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-07-30",])



summary(CAL.N2O.2021_07_30) 

abline(a = CAL.N2O.2021_07_30$coefficients[1] ,
       
       b = CAL.N2O.2021_07_30$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-30", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-07-30", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_07_30$coefficients[1],3), round(CAL.N2O.2021_07_30$coefficients[2],3), sep = "+"))







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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-08-05",],
     
     main = "2021-08-05",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-08-05" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-08-05" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_08_05 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-08-05",])



summary(CAL.CO2.2021_08_05) 

abline(a = CAL.CO2.2021_08_05$coefficients[1] ,
       
       b = CAL.CO2.2021_08_05$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-05", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-05", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_08_05$coefficients[1],3), round(CAL.CO2.2021_08_05$coefficients[2],3), sep = "+"))




####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-08-05",],
     
     main = "2021-08-05",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-08-05" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-05", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-08-05" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_08_05 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-08-05",])



summary(CAL.N2O.2021_08_05) 

abline(a = CAL.N2O.2021_08_05$coefficients[1] ,
       
       b = CAL.N2O.2021_08_05$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-05", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-05", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_08_05$coefficients[1],3), round(CAL.N2O.2021_08_05$coefficients[2],3), sep = "+"))





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




####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-08-12",],
     
     main = "2021-08-12",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-08-12" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-08-12" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_08_12 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-08-12",])



summary(CAL.CO2.2021_08_12) 

abline(a = CAL.CO2.2021_08_12$coefficients[1] ,
       
       b = CAL.CO2.2021_08_12$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-12", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-12", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_08_12$coefficients[1],3), round(CAL.CO2.2021_08_12$coefficients[2],3), sep = "+"))


####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-08-12",],
     
     main = "2021-08-12",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-08-12" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-12", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-08-12" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_08_12 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-08-12",])



summary(CAL.N2O.2021_08_12) 

abline(a = CAL.N2O.2021_08_12$coefficients[1] ,
       
       b = CAL.N2O.2021_08_12$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-12", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-12", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_08_12$coefficients[1],3), round(CAL.N2O.2021_08_12$coefficients[2],3), sep = "+"))









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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-08-19",],
     
     main = "2021-08-19",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-08-19" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-08-19" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_08_19 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-08-19",])



summary(CAL.CO2.2021_08_19) 

abline(a = CAL.CO2.2021_08_19$coefficients[1] ,
       
       b = CAL.CO2.2021_08_19$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-19", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-19", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_08_19$coefficients[1],3), round(CAL.CO2.2021_08_19$coefficients[2],3), sep = "+"))




####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-08-19",],
     
     main = "2021-08-19",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-08-19" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-08-19", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-08-19" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_08_19 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-08-19",])



summary(CAL.N2O.2021_08_19) 

abline(a = CAL.N2O.2021_08_19$coefficients[1] ,
       
       b = CAL.N2O.2021_08_19$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-19", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-08-19", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_08_19$coefficients[1],3), round(CAL.N2O.2021_08_19$coefficients[2],3), sep = "+"))






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




####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-09-02",],
     
     main = "2021-09-02",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-09-02" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-09-02",]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_09_02 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-09-02",])



summary(CAL.CO2.2021_09_02) 

abline(a = CAL.CO2.2021_09_02$coefficients[1] ,
       
       b = CAL.CO2.2021_09_02$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-02", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-02", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_09_02$coefficients[1],3), round(CAL.CO2.2021_09_02$coefficients[2],3), sep = "+"))





####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-09-02",],
     
     main = "2021-09-02",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-09-02" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-02", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-09-02" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_09_02 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-09-02",])



summary(CAL.N2O.2021_09_02) 

abline(a = CAL.N2O.2021_09_02$coefficients[1] ,
       
       b = CAL.N2O.2021_09_02$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-02", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-02", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_09_02$coefficients[1],3), round(CAL.N2O.2021_09_02$coefficients[2],3), sep = "+"))


###### Record 2638 is an outlier ######

##### Remove outlier row name 2638, 2552, 2541, 2530, 2605 #######

GC.standards[rownames(GC.standards) == "2638", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "2638", ] ;

GC.standards[rownames(GC.standards) == "2552", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "2552", ] ;

GC.standards[rownames(GC.standards) == "2541", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "2541", ] ;

GC.standards[rownames(GC.standards) == "2530", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "2530", ] ;


GC.standards[rownames(GC.standards) == "2605", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "2605", ] ;

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




####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-09-17",],
     
     main = "2021-09-17",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-09-17" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-09-17" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_09_17 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-09-17",])



summary(CAL.CO2.2021_09_17) 

abline(a = CAL.CO2.2021_09_17$coefficients[1] ,
       
       b = CAL.CO2.2021_09_17$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-17", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-17", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_09_17$coefficients[1],3), round(CAL.CO2.2021_09_17$coefficients[2],3), sep = "+"))








####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-09-17",],
     
     main = "2021-09-17",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-09-17" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-17", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-09-17" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_09_17 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-09-17",])



summary(CAL.N2O.2021_09_17) 

abline(a = CAL.N2O.2021_09_17$coefficients[1] ,
       
       b = CAL.N2O.2021_09_17$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-17", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-17", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_09_17$coefficients[1],3), round(CAL.N2O.2021_09_17$coefficients[2],3), sep = "+"))


# Switch records  2810, 2724,  2799 and 2713

GC.standards[rownames(GC.standards) == "2810", ]

GC.standards[rownames(GC.standards) == "2810", "Sample.Name"] <- "50PerSTD"

GC.standards[rownames(GC.standards) == "2810", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c( 25,  2500, 25 ) ;


GC.standards[rownames(GC.standards) == "2724", ]

GC.standards[rownames(GC.standards) == "2724", "Sample.Name"] <- "50PerSTD"

GC.standards[rownames(GC.standards) == "2724", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c( 25,  2500, 25 ) ;


GC.standards[rownames(GC.standards) == "2799", ]

GC.standards[rownames(GC.standards) == "2799", "Sample.Name"] <- "100PerSTD"

GC.standards[rownames(GC.standards) == "2799", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c( 50,  5000, 50 ) ;


GC.standards[rownames(GC.standards) == "2713", ]

GC.standards[rownames(GC.standards) == "2713", "Sample.Name"] <- "100PerSTD"

GC.standards[rownames(GC.standards) == "2713", c("CH4.ppm" , "CO2.ppm" ,"N2O.ppm") ] <- c( 50,  5000, 50 ) ;






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



####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-09-29",],
     
     main = "2021-09-29",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-09-29" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-09-29" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_09_29 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-09-29",])



summary(CAL.CO2.2021_09_29) 

abline(a = CAL.CO2.2021_09_29$coefficients[1] ,
       
       b = CAL.CO2.2021_09_29$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-29", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-29", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_09_29$coefficients[1],3), round(CAL.CO2.2021_09_29$coefficients[2],3), sep = "+"))




####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-09-29",],
     
     main = "2021-09-29",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-09-29" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-09-29", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-09-29" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_09_29 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-09-29",])



summary(CAL.N2O.2021_09_29) 

abline(a = CAL.N2O.2021_09_29$coefficients[1] ,
       
       b = CAL.N2O.2021_09_29$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-29", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-09-29", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_09_29$coefficients[1],3), round(CAL.N2O.2021_09_29$coefficients[2],3), sep = "+"))










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




####### Plot Calibration line  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-10-27",],
     
     main = "2021-10-27",
     
     col = "blue",
     
     # ylim = c(0,600),
     
     # xlim = c(8000,11000),
     
     type = "p")  ; 

text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-10-27" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-10-27" ,]),
      
      pos = 1);




###### Calculate calibration line CO2 #######

CAL.CO2.2021_10_27 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-10-27",])



summary(CAL.CO2.2021_10_27) 

abline(a = CAL.CO2.2021_10_27$coefficients[1] ,
       
       b = CAL.CO2.2021_10_27$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-10-27", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-10-27", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2021_10_27$coefficients[1],3), round(CAL.CO2.2021_10_27$coefficients[2],3), sep = "+"))







####### Plot Calibration line  N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2021-10-27",],
     
     main = "2021-10-27",
     
     col = "blue",
     
     #xlim = c(0,600),
     
     #ylim = c(0,2),
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2021-10-27" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2021-10-27", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2021-10-27" ,]),
      
      pos = 1);




###### Calculate calibration line  N2O #######

CAL.N2O.2021_10_27 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2021-10-27",])



summary(CAL.N2O.2021_10_27) 

abline(a = CAL.N2O.2021_10_27$coefficients[1] ,
       
       b = CAL.N2O.2021_10_27$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2021-10-27", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2021-10-27", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2021_10_27$coefficients[1],3), round(CAL.N2O.2021_10_27$coefficients[2],3), sep = "+"))





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



###### with out  2021-09-22   ########

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

  
  
  
  
  