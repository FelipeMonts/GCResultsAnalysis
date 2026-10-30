##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused on analyzing standards for calibration obtained during 2022
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

# Year = 2021

 Year = 2022


PeakArea.results <- read.csv(file = "FluxDataAnalysisResults\\GCcompiledResults_2022_2026_10_23_11_06_11.csv"  , header = T) ;



# ###############################################################################################################
#              Notes collected during the GC analysis that affect standards results
# 
# 20220629_B1B2
# 
# The vials on Block 2, 3-SppN treatment B were switched with the vials of Block 2, 3SppN treatment C
# 
# 2-3SppN-B <-> 2-3SppN-C
# 
# The first half of the standards were pressure equilibrated, the second half were not.
# 
# 
# 20220719_B1B2
# 
# Low standards were not available and this run is without low standards.
# 
# Low standards vials ar evacuated empty.
# 
# 
# 202200910_B3B4
# 
# There was no enough 50% low standard to fill all the vials.
# The last 2 sets of standards do not have any gas in the low 50% standard vials L-50%; Those are empty
# 
# 
# 202200927_B1B2
# 
# There were no low standards (L-100%, L-50%) standards left.
# Those vials do not have any gas except what was left from the preious GC analysis.
# Air cylinder was at 0 PSI!!! Changed the air cylinder. Falem stabilized at 23 Pa

##############################################################################################################




###############################################################################################################

###############################################################################################################
#                           
#                              Checking for duplicated records
#
###############################################################################################################

str(PeakArea.results)

head(PeakArea.results)

anyDuplicated(PeakArea.results, MARGIN = c(1,2))

PeakArea.results[c(316:320) ,]

PeakArea.results[PeakArea.results$CH4 == 4.048,]

PeakArea.results[PeakArea.results$CH4 == 9.056,]

PeakArea.results[PeakArea.results$CH4 == 10.521,]

duplicated(PeakArea.results, MARGIN = c(1,2))

################################## It seems that 2022 has some duplicates   ##################################

which(duplicated(PeakArea.results, MARGIN = c(1,2)))

PeakArea.results[c(316:320) ,]

PeakArea.results[PeakArea.results$CH4 == 4.048,]

PeakArea.results[PeakArea.results$CH4 == 9.056,]

PeakArea.results[PeakArea.results$CH4 == 10.521,]

duplicated(PeakArea.results, MARGIN = c(1,2))

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



# PeakArea.results$CH4<-as.double(PeakArea.results$CH4) ;
# 
# PeakArea.results$CO2<-as.double(PeakArea.results$CO2) ;
# 
# PeakArea.results$N2O<-as.double(PeakArea.results$N2O) ;


# getting the GC samples with standards together

GC.standards <- PeakArea.results[grep( pattern = "B" , x = PeakArea.results$Sample.Name, invert = T) ,] ;
   

# GC.Data <-  PeakArea.results[grep( pattern = "B" , x = PeakArea.results$Sample.Name, invert = F) ,] ;

which(is.na(GC.standards))

###############################################################################################################
#                           
#                            Standards Data dispersion and variability 
#
###############################################################################################################


str(GC.standards)

### group the data by the different standards

GC.standards$Factor.Name <- as.factor(GC.standards$Sample.Name) ;

str(GC.standards$Sample.Name)

str(GC.standards$Factor.Name)


GC.standards$CH4.ppm <- NaN

GC.standards$CO2.ppm <- NaN

GC.standards$N2O.ppm <- NaN

levels(GC.standards$Factor.Name)




head(GC.standards);

    
    # Levels Names for 2022
    
    # [1] "0"    "H100" "H50"  "L100" "L25"  "L50"
    
    # Values of the standards 
    # CH4	CO2	N2O
    # 0 perSTD	ppm 	uL/Lgas	0	0	0
    # L25 perSTDA	ppm 	uL/Lgas	1.25	125	0.25
    # L50 perSTDA	ppm 	uL/Lgas	2.5	250	0.5
    # L75 perSTDA	ppm 	uL/Lgas	3.75	375	0.75
    # L100 perSTDA	ppm 	uL/Lgas	5	500	1
    # H50 PerSTD	ppm 	uL/Lgas	25	2500	25
    # H100 PerSTD	ppm 	uL/Lgas	50	5000	50
    
GC.standards[GC.standards$Sample.Name == 'L100',c('CH4.ppm')] <- 5 ;
GC.standards[GC.standards$Sample.Name == 'L100',c('CO2.ppm')] <- 500 ;
GC.standards[GC.standards$Sample.Name == 'L100',c('N2O.ppm')] <- 1 ;
    
    
GC.standards[GC.standards$Sample.Name == 'H100',c('CH4.ppm')] <- 50 ;
GC.standards[GC.standards$Sample.Name == 'H100',c('CO2.ppm')] <- 5000 ;
GC.standards[GC.standards$Sample.Name == 'H100',c('N2O.ppm')] <- 50 ;    
    
    
GC.standards[GC.standards$Sample.Name == 'L50',c('CH4.ppm')] <- 2.5  ;
GC.standards[GC.standards$Sample.Name == 'L50',c('CO2.ppm')] <- 250  ;
GC.standards[GC.standards$Sample.Name == 'L50',c('N2O.ppm')] <- 0.5  ;
    
    
GC.standards[GC.standards$Sample.Name == 'H50',c('CH4.ppm')] <- 25 ;
GC.standards[GC.standards$Sample.Name == 'H50',c('CO2.ppm')] <- 2500 ;
GC.standards[GC.standards$Sample.Name == 'H50',c('N2O.ppm')] <- 25;
    
GC.standards[GC.standards$Sample.Name == 'L25',c('CH4.ppm')] <- 1.25 ;
GC.standards[GC.standards$Sample.Name == 'L25',c('CO2.ppm')] <- 125 ;
GC.standards[GC.standards$Sample.Name == 'L25',c('N2O.ppm')] <- 0.25;



    
GC.standards[GC.standards$Sample.Name == '0',c('CH4.ppm', 'CO2.ppm', 'N2O.ppm')] <- 0 ;
    
    


str(GC.standards)

GC.standards


head(GC.standards)  

tail(GC.standards)
  



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

  

##############################################################################################################
#                           
#                           Working with Standards obtained in 2022
#
###############################################################################################################


str(GC.standards$Sample.Name)

unique(GC.standards$Sample.Name)



##############################################################################################################
#                           
#                           GC Date [2022-07-13]  Sampling.Date [2022-06-29]
#
###############################################################################################################


GC.standards[GC.standards$GC.Date == "2022-07-13",]


######## GC NOTE  20220629_B1B2
# 
# The vials on Block 2, 3-SppN treatment B were switched with the vials of Block 2, 3SppN treatment C
# 
# 2-3SppN-B <-> 2-3SppN-C
# 
# The first half of the standards were pressure equilibrated, the second half were not.
##############################


##### Select conditions  


GC.standards[GC.standards$Sampling.Date == "2022-06-29" & GC.standards$AnalysisName == "Summary20220629_B1B2.pdf", ]

GC.standards.20220629B1B2 <- GC.standards[GC.standards$Sampling.Date == "2022-06-29" & 
                                        
                                        GC.standards$AnalysisName == "Summary20220629_B1B2.pdf", ] ;

GC.standards.20220629B1B2 <- GC.standards.20220629B1B2[order(GC.standards.20220629B1B2$Position),] ;

dim.data.frame(GC.standards.20220629B1B2)

plot(CO2 ~ CO2.ppm , data = GC.standards.20220629B1B2[1:15,], col= "red", xlim = c(0, 5000))
points(CO2 ~ CO2.ppm, data = GC.standards.20220629B1B2[16:25,], col= "blue")

plot(N2O ~ N2O.ppm , data = GC.standards.20220629B1B2[1:15,], col= "red", xlim = c(0, 50))
points(N2O ~ N2O.ppm, data = GC.standards.20220629B1B2[16:25,], col= "blue")


plot(CH4 ~ CH4.ppm , data = GC.standards.20220629B1B2[1:15,], col= "red", xlim = c(0, 50))
points(CH4 ~ CH4.ppm, data = GC.standards.20220629B1B2[16:25,], col= "blue")

###### no practical difference between the sets ,  keep both ###########

rm(GC.standards.20220629B1B2)


####### Plot Calibration line  for  CO2 #######

GC.standards[GC.standards$Sampling.Date == "2022-06-29",]

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2022-06-29",],
     
     main = "2022-06-30",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "Sample.Name"],
      
      pos = 3);

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,]),
      
      pos = 1);


###### Calculate calibration line CO2 #######

CAL.CO2.2022_06_29 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2022-06-29",])



summary(CAL.CO2.2022_06_29) 

abline(a = CAL.CO2.2022_06_29$coefficients[1] ,
       
       b = CAL.CO2.2022_06_29$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-29", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-29", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2022_06_29$coefficients[1],3), round(CAL.CO2.2022_06_29$coefficients[2],3), sep = "+"))



####### Plot Calibration line  for N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2022-06-29",],
     
     main = "2022-06-29",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "Sample.Name"],
      
      pos = 3);

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-29", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,]),
      
      pos = 1);

###### Calculate calibration line N2O #######

CAL.N2O.2022_06_29 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2022-06-29",] )
                         




summary(CAL.N2O.2022_06_29) 

abline(a = CAL.N2O.2022_06_29$coefficients[1] ,
       
       b = CAL.N2O.2022_06_29$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-29", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-29", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2022_06_29$coefficients[1],3), round(CAL.N2O.2022_06_29$coefficients[2],3), sep = "+")
      
)



##############################################################################################################
#                           
#                           GC Date [2022-08-09]  Sampling.Date [2022-06-09]
#
###############################################################################################################


GC.standards[GC.standards$GC.Date == "2022-08-09",]


####### Plot Calibration line  for  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2022-06-09",],
     
     main = "2022-06-09",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2022-06-09" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2022-06-09" ,]),
      
      pos = 1);


###### Calculate calibration line CO2 #######

CAL.CO2.2022_06_09 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2022-06-09",])



summary(CAL.CO2.2022_06_09) 

abline(a = CAL.CO2.2022_06_09$coefficients[1] ,
       
       b = CAL.CO2.2022_06_09$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-09", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-09", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2022_06_09$coefficients[1],3), round(CAL.CO2.2022_06_09$coefficients[2],3), sep = "+"))



####### Plot Calibration line  for N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2022-06-09",],
     
     main = "2022-06-09",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2022-06-09" , "Sample.Name"],
      
      pos = 3);

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-09", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2022-06-09" ,]),
      
      pos = 1);

###### Calculate calibration line N2O #######

CAL.N2O.2022_06_09 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2022-06-09",],
                         
)



summary(CAL.N2O.2022_06_09) 

abline(a = CAL.N2O.2022_06_09$coefficients[1] ,
       
       b = CAL.N2O.2022_06_09$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-09", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-09", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2022_06_09$coefficients[1],3), round(CAL.N2O.2022_06_09$coefficients[2],3), sep = "+")
      
)


points(N2O.ppm ~ N2O, 
       
       data = GC.standards[rownames(GC.standards) == "454", ],
       
       main = "2022-06-09",
       
       col = "red",
       
       cex = 2,
       
       pch = 19) ;

##### Remove outliers row names 454 ####### 

GC.standards[rownames(GC.standards) == "454", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "454", ] ;

##############################################################################################################
#                           
#                           Cumulative plot for 2022
#
###############################################################################################################

##### data to plot  CO2 #####

str(GC.standards$Sampling.Date)

Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09")


Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;

str(Cum.Plot.Data)

xyplot(CO2 ~ CO2.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main="CO2", 
       
       auto.key = T, 

      panel=function(x, y, ...){
        
        panel.xyplot(x, y, ...)
        
        panel.text(x=x, y=y, 
                   
                   labels= rownames(Cum.Plot.Data), 
                   
                   pos=1)
        
      })


text( x = Cum.Plot.Data$CO2.ppm,
      
      y = Cum.Plot.Data$CO2,
      
      labels = rownames(Cum.Plot.Data),
      
      pos = 1);



##### data to plot  N20 #####

str(GC.standards$Sampling.Date)

Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09")


Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;

str(Cum.Plot.Data)

xyplot(N2O ~ N2O.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main = "N2O", 
       
       auto.key = T, 
       
       panel=function(x, y, ...){
         
         panel.xyplot(x, y, ...)
         
         panel.text(x=x, y=y, 
                    
                    labels= rownames(Cum.Plot.Data), 
                    
                    pos=1)
         
       }
       
)
       


GC.standards[rownames(GC.standards) == "421", ]


##### Remove outliers row names 421 ####### 

GC.standards[rownames(GC.standards) == "421", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "421", ] ;


##### data to plot  CO2 #####

str(GC.standards$Sampling.Date)

Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09")


Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;

str(Cum.Plot.Data)

xyplot(CO2 ~ CO2.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main="CO2", 
       
       auto.key = T, 

      panel=function(x, y, ...){
        
        panel.xyplot(x, y, ...)
        
        panel.text(x=x, y=y, 
                   
                   labels= rownames(Cum.Plot.Data), 
                   
                   pos=1)
        
      })




##############################################################################################################
#                           
#                           GC Date [2022-08-10]  Sampling.Date [2022-06-23]
#
###############################################################################################################


GC.standards[GC.standards$GC.Date == "2022-08-10",]


####### Plot Calibration line  for  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2022-06-23",],
     
     main = "2022-06-23",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2022-06-23" ,]),
      
      pos = 1);


###### Calculate calibration line CO2 #######

CAL.CO2.2022_06_23 <- lm(CO2.ppm ~ CO2,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2022-06-23",])



summary(CAL.CO2.2022_06_23) 

abline(a = CAL.CO2.2022_06_23$coefficients[1] ,
       
       b = CAL.CO2.2022_06_23$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-23", c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-23", c("CO2.ppm")]),
      
      labels = paste(round(CAL.CO2.2022_06_23$coefficients[1],3), round(CAL.CO2.2022_06_23$coefficients[2],3), sep = "+"))



####### Plot Calibration line  for N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == "2022-06-23",],
     
     main = "2022-06-23",
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "Sample.Name"],
      
      pos = 3);

text( x = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == "2022-06-23", "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == "2022-06-23" ,]),
      
      pos = 1);

###### Calculate calibration line N2O #######

CAL.N2O.2022_06_23 <- lm(N2O.ppm ~ N2O,
                         
                         data = GC.standards[GC.standards$Sampling.Date == "2022-06-23",],
                         
)



summary(CAL.N2O.2022_06_23) 

abline(a = CAL.N2O.2022_06_23$coefficients[1] ,
       
       b = CAL.N2O.2022_06_23$coefficients[2] , 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-23", c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == "2022-06-23", c("N2O.ppm")]),
      
      labels = paste(round(CAL.N2O.2022_06_23$coefficients[1],3), round(CAL.N2O.2022_06_23$coefficients[2],3), sep = "+")
      
)





points(N2O.ppm ~ N2O, 
       
       data = GC.standards[rownames(GC.standards) == "716", ],
       
       col = "red",
       
       cex = 2,
       
       pch = 19) ;

##### Remove outliers row names 454 ####### 

GC.standards[rownames(GC.standards) == "454", ]

GC.standards <- GC.standards[!rownames(GC.standards) == "454", ] ;

##############################################################################################################
#                           
#                           Cumulative plot for 2022
#
###############################################################################################################

##### data to plot  CO2 #####

str(GC.standards$Sampling.Date)

Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09", "2022-06-23")


Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;

str(Cum.Plot.Data)

xyplot(CO2 ~ CO2.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main="CO2", 
       
       auto.key = T, 
       
       panel=function(x, y, ...){
         
         panel.xyplot(x, y, ...)
         
         panel.text(x=x, y=y, 
                    
                    labels= rownames(Cum.Plot.Data), 
                    
                    pos=1)
         
       })




##### data to plot  N20 #####

str(GC.standards$Sampling.Date)


str(Cum.Plot.Data)

xyplot(N2O ~ N2O.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main = "N2O", 
       
       auto.key = T, 
       
       panel=function(x, y, ...){
         
         panel.xyplot(x, y, ...)
         
         panel.text(x=x, y=y, 
                    
                    labels= rownames(Cum.Plot.Data), 
                    
                    pos=1)
         
       }
       
)






##### Remove outliers row names  ####### 

# GC.standards[rownames(GC.standards) == "421", ]

# GC.standards <- GC.standards[!rownames(GC.standards) == "421", ] ;






##############################################################################################################
#                           
#                           GC Date [2022-08-19 & 2022-08-30]  Sampling.Date [2022-06-29]
#
###############################################################################################################



GC.standards[GC.standards$GC.Date == "2022-08-19",]


i = "2022-06-29" 

j = "2022_06_29"

####### Plot Calibration line  for  CO2 #######

plot(CO2.ppm ~ CO2, 
     
     data = GC.standards[GC.standards$Sampling.Date == i,],
     
     main = i ,
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
      
      pos = 3);


text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
      
      y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
      
      pos = 1);



###### Calculate calibration line CO2 #######

assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                   
                                   data = GC.standards[GC.standards$Sampling.Date == i , ]))


summary(get(paste("CAL.CO2.", j, sep = "")))

get(paste("CAL.CO2.", j,sep = ""))[[1]][1]

abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
       
       b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
      
      labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                     
                     round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                     
                     sep = "+")
      )



####### Plot Calibration line  for N2O #######

plot(N2O.ppm ~ N2O, 
     
     data = GC.standards[GC.standards$Sampling.Date == i,],
     
     main = i,
     
     col = "blue",
     
     type = "p")  ;

text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
      
      labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
      
      pos = 3);

text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
      
      y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
      
      labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
      
      pos = 1);

###### Calculate calibration line N2O #######

assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                          
                                          data = GC.standards[GC.standards$Sampling.Date == i,]))



summary(get(paste("CAL.N2O.", j, sep = "")))

abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
       
       b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
       
       col = "red") ;

text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
      
      y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
      
      labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
      
)







points(N2O.ppm ~ N2O, 
       
       data = GC.standards[rownames(GC.standards) == "716", ],
       
       col = "red",
       
       cex = 2,
       
       pch = 19) ;

##### Remove outliers row names 454 ####### 

# GC.standards[rownames(GC.standards) == "454", ]

# GC.standards <- GC.standards[!rownames(GC.standards) == "454", ] ;

##############################################################################################################
#                           
#                           Cumulative plot for 2022
#
###############################################################################################################

##### data to plot  CO2 #####

i

str(GC.standards$Sampling.Date)

Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09", "2022-06-23" , "2022-06-29")


Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;

str(Cum.Plot.Data)

Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main="CO2", 
       
       auto.key = T, 
       
       panel=function(x, y, ...){
         
         panel.xyplot(x, y, ...)
         
         panel.text(x=x, y=y, 
                    
                    labels= rownames(Cum.Plot.Data), 
                    
                    pos=1)
         
       })

Cum.Plot.CO2

## insert additional points

update(Cum.Plot.CO2, panel = function(...) {
  
  panel.xyplot(...)
  
  panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "CO2.ppm" ],
               
               y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "CO2" ], 
               
               
               pch = 19,
               
               col = "red",
               
               cex = 1.5
               
               )
  
  panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "CO2.ppm" ],
             
             y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "CO2" ], 
             
             labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29",]), 
             
             pos=1)
})

##### data to plot  N20 #####

str(GC.standards$Sampling.Date)


str(Cum.Plot.Data)

Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
       
       groups = GC.Date, 
       
       data = Cum.Plot.Data, 
       
       pch =16, 
       
       main = "N2O", 
       
       auto.key = T, 
       
       panel=function(x, y, ...){
         
         panel.xyplot(x, y, ...)
         
         panel.text(x=x, y=y, 
                    
                    labels= rownames(Cum.Plot.Data), 
                    
                    pos=1)
         
       }
       
)

Cum.Plot.N2O

update(Cum.Plot.N2O, panel = function(...) {
  
  panel.xyplot(...)
  
  panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "N2O.ppm" ],
               
               y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "N2O" ], 
               
               
               pch = 19,
               
               col = "red",
               
               cex = 1.5
               
  )
  
  panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "N2O.ppm" ],
             
             y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29", "N2O" ], 
             
             labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == "2022-06-29",]), 
             
             pos=1)
})




##### Remove outliers row names  ####### 

 GC.standards[rownames(GC.standards) == "924", ]

 GC.standards <- GC.standards[!rownames(GC.standards) == "924", ] ;

 GC.standards[rownames(GC.standards) == "842", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "842", ] ;

 GC.standards[rownames(GC.standards) == "888", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "888", ] ;

 GC.standards[rownames(GC.standards) == "1319", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "1319", ] ;

 GC.standards[rownames(GC.standards) == "877", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "877", ] ;

 GC.standards[rownames(GC.standards) == "893", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "893", ] ;

 GC.standards[rownames(GC.standards) == "842", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "842", ] ;

 GC.standards[rownames(GC.standards) == "926", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "926", ] ;

 GC.standards[rownames(GC.standards) == "1308", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "1308", ] ;


 ##############################################################################################################
 #                           
 #                           GC Date [2022-08-31]  Sampling.Date [2022-06-15]
 #
 ###############################################################################################################
  
 
 
 
 GC.standards[GC.standards$GC.Date == "2022-08-31",]
 
 
 i = "2022-06-15" 
 
 j = "2022_06_15"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 
 
 
 points(N2O.ppm ~ N2O, 
        
        data = GC.standards[rownames(GC.standards) == "716", ],
        
        col = "red",
        
        cex = 2,
        
        pch = 19) ;
 
 
 ##### Remove outliers row names 454 ####### 
 
  GC.standards[rownames(GC.standards) == "576", ]
 
  GC.standards <- GC.standards[!rownames(GC.standards) == "576", ] ;
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09", "2022-06-23" , "2022-06-29" , "2022-06-15")
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 

 ##############################################################################################################
 #                           
 #                           GC Date [2022-09-02]  Sampling.Date [2022-07-07]
 #
 ###############################################################################################################
 
 
 
 
 GC.standards[GC.standards$GC.Date == "2022-09-02",]
 
 
 i = "2022-07-07" 
 
 j = "2022_07_07"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 

 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 454 ####### 
 
 # GC.standards[rownames(GC.standards) == "454", ]
 
 # GC.standards <- GC.standards[!rownames(GC.standards) == "454", ] ;
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09", "2022-06-23" , "2022-06-29" , "2022-06-15", "2022-07-07" )
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 
 
 ##############################################################################################################
 #                           
 #                           GC Date [2022-09-04]  Sampling.Date [2022-07-19]
 #
 ###############################################################################################################
 
 ############################################
 #  GC NOTE
 # 20220719_B1B2
 # 
 # Low standards were not available and this run is without low standards.
 # 
 # Low standards vials ar evacuated empty.
 ############################################
 
 
 # Select conditions
 
 
 GC.standards.20220719B1B2 <- GC.standards[GC.standards$AnalysisName == "Summary20220719_B1B2.pdf",]
 
 grep( pattern = "L" , x =  GC.standards.20220719B1B2$Sample.Name, invert = F)
   
 GC.standards.20220719B1B2L <- GC.standards.20220719B1B2[grep( pattern = "L" , x =  GC.standards.20220719B1B2$Sample.Name, invert = F),]
 

 plot(CO2 ~ CO2.ppm , data = GC.standards.20220719B1B2 , col = "red" )
 
 points(CO2 ~ CO2.ppm,  data = GC.standards.20220719B1B2L , col = "blue" )
 
 
 ##### Low Standards are defective according to the note (blue in the plot)
 

 
 row.names(GC.standards.20220719B1B2L)
 
 GC.standards[row.names(GC.standards.20220719B1B2L), ]
 
 GC.standards.Rev1 <- GC.standards[ which(!row.names(GC.standards) %in% row.names(GC.standards.20220719B1B2L)),]
 
 GC.standards.Rev1[GC.standards.Rev1$AnalysisName == "Summary20220719_B1B2.pdf", ] 
 
 GC.standards.Rev2 <- GC.standards[row.names(GC.standards) %in% row.names(GC.standards.Rev1),]
 
 GC.standards <- GC.standards.Rev2 
 
 rm(GC.standards.Rev1, GC.standards.Rev2  )
 
 ### check
 
 GC.standards[GC.standards$AnalysisName == "Summary20220719_B1B2.pdf", ]  
 
 
 
 
 i = "2022-07-19" 
 
 j = "2022_07_19"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 1381 ####### 
 
GC.standards[rownames(GC.standards) == "1381", ]
 
GC.standards <- GC.standards[!rownames(GC.standards) == "1381", ] ;
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30" , 
                         
                         "2022-06-09", 
                         
                         "2022-06-23" , 
                         
                         "2022-06-29",
                         
                         "2022-06-15",
                         
                         "2022-07-07",
                         
                         "2022-07-19" )
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })



 ##############################################################################################################
 #                           
 #                           GC Date ["2022-10-15"]  Sampling.Date [2022-06-23]
 #
 ###############################################################################################################
 
 
 
 
 levels(GC.standards$ANAL.DATE)
 
 GC.standards[GC.standards$GC.Date == "2022-10-15",]
 
 
 i = "2022-06-23" 
 
 j = "2022_06_23"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 1381 ####### 
 
 GC.standards[rownames(GC.standards) == "1381", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "1381", ] ;
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09", "2022-06-23" , "2022-06-29" , "2022-06-15", "2022-07-07" ,"2022-07-19" )
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 
 
 ##############################################################################################################
 #                           
 #                          GC Date ["2022-10-16"]  Sampling.Date [2022-07-19]
 #
 ###############################################################################################################
 
 

 
 str(GC.standards)
 
 levels(GC.standards$ANAL.DATE)
 
 unique(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates
 
 GC.standards[GC.standards$GC.Date == "2022-10-16",]
 
 GC.standards[GC.standards$Sampling.Date == "2022-07-19",]
 
 
 i = "2022-07-19" 
 
 j = "2022_07-19"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 1381 ####### 
 
 GC.standards[rownames(GC.standards) == "1381", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "1381", ] ;
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30" , "2022-06-09", "2022-06-23" , "2022-06-29" , "2022-06-15", "2022-07-07" ,"2022-07-19" )
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 
 ##############################################################################################################
 #                           
 #                         GC Date ["2022-10-23"]  Sampling.Date [2022-09-01] 
 #
 ###############################################################################################################
 
 
 
 str(GC.standards)
 
 levels(GC.standards$ANAL.DATE)
 
 unique(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates
 
 GC.standards[GC.standards$GC.Date == "2022-10-23",]
 
 GC.standards[GC.standards$Sampling.Date == "2022-09-01",]
 
 
 i = "2022-09-01" 
 
 j = "2022_09_01"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 1623 and 1745 ####### 
 
 GC.standards[rownames(GC.standards) == "1623", ]

 GC.standards <- GC.standards[!rownames(GC.standards) == "1623", ] ;
 
 GC.standards[rownames(GC.standards) == "1745", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "1745", ] ;

 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30",
                         
                         "2022-06-09", 
                         
                         "2022-06-23", 
                         
                         "2022-06-29", 
                         
                         "2022-06-15", 
                         
                         "2022-07-07",
                         
                         "2022-07-19",
                         
                         "2022-09-01")
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 
 
 
 ##############################################################################################################
 #                           
 #                         GC Date ["2022-11-24"]  Sampling.Date [2022-09-10]
 #
 ###############################################################################################################
 
 
 # #############################
 # GC Note
 # 202200910_B3B4
 # 
 # There was no enough 50% low standard to fill all the vials.
 # The last 2 sets of standards do not have any gas in the low 50% standard vials L-50%; Those are empty
 # 
 # #############################
 
 
 ### Get the conditions ######
 
 GC.standards[GC.standards$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" , ]
 
 row.names(GC.standards[GC.standards$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" & GC.standards$Factor.Name == "L50" , ])
 
 L50.Rev <- GC.standards[GC.standards$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" & GC.standards$Factor.Name == "L50" , ]
 
 row.names(L50.Rev)
 
 L50.Rev.Pos <- L50.Rev[order(L50.Rev$Position),]
 
 row.names(L50.Rev.Pos)

 plot(L50.Rev.Pos$CO2, col = "blue")
 
 plot(L50.Rev.Pos$N2O, col = "red")
 
 plot(L50.Rev.Pos$CH4, col = "green")
      
 ####### Two last L50 Standards are defective as the note describe. Remove #####
 
 dim(L50.Rev.Pos)
 
 row.names(L50.Rev.Pos[4:5,])
 
 GC.standards.L50.Rev <- GC.standards[which(!row.names(GC.standards) %in% row.names(L50.Rev.Pos[4:5,])), ] ;
 
 row.names(GC.standards[GC.standards$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" & GC.standards$Factor.Name == "L50" , ])
 
 
row.names(GC.standards.L50.Rev[GC.standards.L50.Rev$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" & GC.standards.L50.Rev$Factor.Name == "L50" , ])
 

 GC.standards.L50.Rev[GC.standards.L50.Rev$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" &
                        
                        GC.standards.L50.Rev$Factor.Name == "L50",  ]
 
 GC.standards[!row.names(GC.standards) %in% row.names(GC.standards.L50.Rev),] ;
 
 
 GC.standards <- GC.standards.L50.Rev ;
 
 
 rm(L50.Rev , GC.standards.L50.Rev, GC.standards.Rev.L50.202200910_B3B4)
 
 
###### check ###
 
 GC.standards[GC.standards$AnalysisName == "SummaryGCN2O20220910_B3B4.pdf" & GC.standards$Factor.Name == "L50" ,]
 
 
 
 levels(GC.standards$ANAL.DATE)
 
 unique(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates
 
 GC.standards[GC.standards$GC.Date == "2022-11-24",]
 
 GC.standards[GC.standards$Sampling.Date == "2022-09-10",]
 
 
 i = "2022-09-10" 
 
 j = "2022_09_10"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 194 ####### 
 
 GC.standards[rownames(GC.standards) == "194", ]
 
 GC.standards <- GC.standards[!rownames(GC.standards) == "194", ] ;
 
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30",
                         
                         "2022-06-09", 
                         
                         "2022-06-23", 
                         
                         "2022-06-29", 
                         
                         "2022-06-15", 
                         
                         "2022-07-07",
                         
                         "2022-07-19",
                         
                         "2022-09-01",
                         
                         "2022-09-10")
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 
 
 
 ##############################################################################################################
 #                           
 #                         GC Date ["2022-11-25"]  Sampling.Date [2022-09-10]
 #
 ###############################################################################################################
 
 
 
 ##############################################################################################################
 #                           
 #                         GC Date ["2022-11-27"]  Sampling.Date [2022-09-27]
 #
 ###############################################################################################################
 
 str(GC.standards)
 
 levels(GC.standards$ANAL.DATE)
 
 unique(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates
 
 GC.standards[GC.standards$GC.Date == "2022-11-27",]
 
 GC.standards[GC.standards$Sampling.Date == "2022-09-27",]
 
########### GC Note
 # 202200927_B1B2
 # 
 # There were no low standards (L-100%, L-50%) standards left.
 # Those vials do not have any gas except what was left from the previous GC analysis.
###########
 
 ## Select conditions 
 
 GC.standards[GC.standards$Sampling.Date == "2022-09-27" & GC.standards$AnalysisName == "SummaryGCN2O20220927_B1B2.pdf" &
                
                (GC.standards$Factor.Name == "L100" | GC.standards$Factor.Name == "L50"),]
 
 GC.standards.L <- GC.standards[GC.standards$Sampling.Date == "2022-09-27" & 
                                  
                                  GC.standards$AnalysisName == "SummaryGCN2O20220927_B1B2.pdf" &
                                  
                                  (GC.standards$Factor.Name == "L100" | GC.standards$Factor.Name == "L50"),]
 
 GC.standards.L.1 <- GC.standards[GC.standards$Sampling.Date == "2022-09-10" & 
                                    
                                    GC.standards$AnalysisName == "SummaryGCN2O20220910_B1B2.pdf" &
                                    
                                    (GC.standards$Factor.Name == "L100" | GC.standards$Factor.Name == "L50"),]
                                                              
                                                              
 
 plot(CO2 ~ CO2.ppm , data = GC.standards.L.1 , col = "blue", ylim = c(0,3000) )

 points(CO2 ~ CO2.ppm , data = GC.standards.L , col = "red")
 
 
 plot(N2O ~ N2O.ppm , data = GC.standards.L.1 , col = "blue", ylim = c(0,1000) )
 
 points(N2O ~ N2O.ppm , data = GC.standards.L , col = "red")
 
 
 plot(CH4 ~ CH4.ppm , data = GC.standards.L.1 , col = "blue", ylim = c(0,50) )
 
 points(CH4 ~ CH4.ppm , data = GC.standards.L , col = "red")
 
 
 #### low standards (L-100%, L-50%) standards are defective. Remove  ###
 
 row.names(GC.standards[GC.standards$Sampling.Date == "2022-09-27" & 
                          
                          GC.standards$AnalysisName == "SummaryGCN2O20220927_B1B2.pdf" &
                          
                          (GC.standards$Factor.Name == "L100" | GC.standards$Factor.Name == "L50"),])
 
 
 
 GC.standards.L <- GC.standards[GC.standards$Sampling.Date == "2022-09-27" & 
                                  
                                  GC.standards$AnalysisName == "SummaryGCN2O20220927_B1B2.pdf" &
                                  
                                  (GC.standards$Factor.Name == "L100" | GC.standards$Factor.Name == "L50"),]
 
row.names(GC.standards.L)
 
GC.standards.Rev.L <- GC.standards[which(!row.names(GC.standards) %in% row.names(GC.standards.L) ),] ;


GC.standards <- GC.standards.Rev.L ;

#### check 
 
GC.standards[GC.standards$Sampling.Date == "2022-09-27" & GC.standards$AnalysisName == "SummaryGCN2O20220927_B1B2.pdf",]




 
 i = "2022-09-27" 
 
 j = "2022_09_27"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 2060 & 2159  ####### 
 
  GC.standards[rownames(GC.standards) == "2060", ]
  
  GC.standards <- GC.standards[!rownames(GC.standards) == "2060", ] ;
  
  GC.standards[rownames(GC.standards) == "2159", ]
  
  GC.standards <- GC.standards[!rownames(GC.standards) == "2159", ] ;
  
 
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30",
                         
                         "2022-06-09", 
                         
                         "2022-06-23", 
                         
                         "2022-06-29", 
                         
                         "2022-06-15", 
                         
                         "2022-07-07",
                         
                         "2022-07-19",
                         
                         "2022-09-01",
                         
                         "2022-09-10",
                         
                         "2022-09-27")
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 

 
 ##############################################################################################################
 #                           
 #                         GC Date ["2022-11-28"]  Sampling.Date [2022-09-27]
 #
 ###############################################################################################################
 
 
 
 ##############################################################################################################
 #                           
 #                         GC Date ["2022-12-05"]  Sampling.Date ["2022-10-05"]
 #
 ###############################################################################################################
 
 str(GC.standards)
 
 levels(GC.standards$ANAL.DATE)
 
 unique(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates
 
 GC.standards[GC.standards$GC.Date == "2022-12-05",]
 
 GC.standards[GC.standards$Sampling.Date == "2022-10-05",]
 
 
 i = "2022-10-05" 
 
 j = "2022_10_05"
 
 ####### Plot Calibration line  for  CO2 #######
 
 plot(CO2.ppm ~ CO2, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i ,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "CO2"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "CO2.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 
 
 ###### Calculate calibration line CO2 #######
 
 assign(paste("CAL.CO2.", j, sep = ""), lm(CO2.ppm ~ CO2,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i , ]))
 
 
 summary(get(paste("CAL.CO2.", j, sep = "")))
 
 get(paste("CAL.CO2.", j,sep = ""))[[1]][1]
 
 abline(a = get(paste("CAL.CO2.", j,sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.CO2.", j,sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("CO2.ppm")]),
       
       labels = paste(round(get(paste("CAL.CO2.", j,sep = ""))[[1]][1],3), 
                      
                      round(get(paste("CAL.CO2.", j,sep = ""))[[1]][2],3), 
                      
                      sep = "+")
 )
 
 
 
 ####### Plot Calibration line  for N2O #######
 
 plot(N2O.ppm ~ N2O, 
      
      data = GC.standards[GC.standards$Sampling.Date == i,],
      
      main = i,
      
      col = "blue",
      
      type = "p")  ;
 
 text( x = GC.standards[GC.standards$Sampling.Date == i , "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i , "N2O.ppm"],
       
       labels = GC.standards[GC.standards$Sampling.Date == i , "Sample.Name"],
       
       pos = 3);
 
 text( x = GC.standards[GC.standards$Sampling.Date == i, "N2O"],
       
       y = GC.standards[GC.standards$Sampling.Date == i, "N2O.ppm"],
       
       labels = rownames(GC.standards[GC.standards$Sampling.Date == i ,]),
       
       pos = 1);
 
 ###### Calculate calibration line N2O #######
 
 assign(paste("CAL.N2O.", j, sep = ""), lm(N2O.ppm ~ N2O,
                                           
                                           data = GC.standards[GC.standards$Sampling.Date == i,]))
 
 
 
 summary(get(paste("CAL.N2O.", j, sep = "")))
 
 abline(a = get(paste("CAL.N2O.", j, sep = ""))[[1]][1] ,
        
        b = get(paste("CAL.N2O.", j, sep = ""))[[1]][2], 
        
        col = "red") ;
 
 text( x = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O")]),
       
       y = mean(GC.standards[GC.standards$Sampling.Date == i , c("N2O.ppm")]),
       
       labels = paste(round(get(paste("CAL.N2O.", j, sep = ""))[[1]][1],3), round(get(paste("CAL.N2O.", j, sep = ""))[[1]][2],3), sep = "+")
       
 )
 
 
 
 
 # points(N2O.ppm ~ N2O, 
 #        
 #        data = GC.standards[rownames(GC.standards) == "716", ],
 #        
 #        col = "red",
 #        
 #        cex = 2,
 #        
 #        pch = 19) ;
 # 
 
 ##### Remove outliers row names 2326 , 2512 , 2406 , 2430  ####### 
 
  GC.standards[rownames(GC.standards) == "2326", ]
 
  GC.standards <- GC.standards[!rownames(GC.standards) == "2326", ] ;
  
  GC.standards[rownames(GC.standards) == "2512", ]
  
  GC.standards <- GC.standards[!rownames(GC.standards) == "2512", ] ;
  
  GC.standards[rownames(GC.standards) == "2406", ]
  
  GC.standards <- GC.standards[!rownames(GC.standards) == "2406", ] ; 
  
  GC.standards[rownames(GC.standards) == "2430", ]
  
  GC.standards <- GC.standards[!rownames(GC.standards) == "2430", ] ; 
 
 ##############################################################################################################
 #                           
 #                           Cumulative plot for 2022
 #
 ###############################################################################################################
 
 ##### data to plot  CO2 #####
 
 i ; j ;
 
 
 str(GC.standards$Sampling.Date)
 
 Cum.Sampling.Dates <- c("2022-06-30",
                         
                         "2022-06-09", 
                         
                         "2022-06-23", 
                         
                         "2022-06-29", 
                         
                         "2022-06-15", 
                         
                         "2022-07-07",
                         
                         "2022-07-19",
                         
                         "2022-09-01",
                         
                         "2022-09-10",
                         
                         "2022-09-27",
                         
                         "2022-10-05")
 
 
 Cum.Plot.Data <-  GC.standards[which(GC.standards$Sampling.Date %in% Cum.Sampling.Dates),] ;
 
 str(Cum.Plot.Data)
 
 Cum.Plot.CO2 <- xyplot(CO2 ~ CO2.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main="CO2", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        })
 
 Cum.Plot.CO2
 
 ## insert additional points
 
 update(Cum.Plot.CO2, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i, "CO2" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "CO2" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 ##### data to plot  N20 #####
 
 str(GC.standards$Sampling.Date)
 
 
 str(Cum.Plot.Data)
 
 Cum.Plot.N2O <- xyplot(N2O ~ N2O.ppm, 
                        
                        groups = GC.Date, 
                        
                        data = Cum.Plot.Data, 
                        
                        pch =16, 
                        
                        main = "N2O", 
                        
                        auto.key = T, 
                        
                        panel=function(x, y, ...){
                          
                          panel.xyplot(x, y, ...)
                          
                          panel.text(x=x, y=y, 
                                     
                                     labels= rownames(Cum.Plot.Data), 
                                     
                                     pos=1)
                          
                        }
                        
 )
 
 Cum.Plot.N2O
 
 update(Cum.Plot.N2O, panel = function(...) {
   
   panel.xyplot(...)
   
   panel.xyplot(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
                
                y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
                
                
                pch = 19,
                
                col = "red",
                
                cex = 1.5
                
   )
   
   panel.text(x = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O.ppm" ],
              
              y = Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i , "N2O" ], 
              
              labels= rownames(Cum.Plot.Data[Cum.Plot.Data$Sampling.Date == i ,]), 
              
              pos=1)
 })
 
 
 
 
 
 ###############################################################################################################
 #                           
 #                                 Write curated Standards Calibration Data Set
 #
 ###############################################################################################################  
 
 write.csv(x = GC.standards, file = "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\StandardsDataset2022.csv")
 
 



 
 
 
 