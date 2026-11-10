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


# source( file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects",
# 
# "\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode" ,
# 
# "\\GCResultsAnalysis\\GC_Standards_2022.R"), echo = T )

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

library(paletteer)

###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

setwd("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis");


#### N2O Sampling year  #####

# Year = 2021

 Year = 2022


 GC.standards <- read.csv(file = "FluxDataAnalysisResults\\StandardsDataset2022_2026_11_04_09_17_13.csv"  , header = T) ;



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

str(GC.standards)

head(GC.standards)

anyDuplicated(GC.standards, MARGIN = c(1,2))

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


##### Check if all the standards were filled  ####

anyNA(GC.standards$N2O.ppm)

which(is.na(GC.standards$CO2.ppm))
    
which(is.na(GC.standards$N2O.ppm))

GC.standards[which(is.na(GC.standards$N2O.ppm)), ]

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


# ###############################################################################################################
# #                           
# #                                 Write curated Standards Calibration Data Set
# #
# ###############################################################################################################  
# 
# Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
# 
# write.csv(x =GC.standards, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
# 
# "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
# 
# "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\Standards2022_",
# 
# Working.Date , ".csv"), row.names = F )
# 
# 
#   

##############################################################################################################
#                           
#                           Working with Standards obtained in 2022
#
###############################################################################################################


str(GC.standards$Sample.Name)

unique(GC.standards$Sample.Name)


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


# plot(NULL, xlim = c( 4990 , 5010),
#      
#      ylim = c(27000, 29000),
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

 OutRange.Points <- c('150' , '170' , '525' , '365', '90' , '63' , '241' , '491' , '122' , '161' , '281' , '55' , '74' , 
                      
                      '226' , '167' , '162' , '192' , '551' , '471' , '282' , '332' ,'426' , '172') ;
 
 anyDuplicated(OutRange.Points)

 OutRange.Points[18]
 
 anyDuplicated(OutRange.Points)
 
 GC.standards[GC.standards$P.ID %in% OutRange.Points ,]
 
 OutRange.Points.1 <- GC.standards[GC.standards$P.ID %in% OutRange.Points ,];
 
 
 OutRange.Points.1 <- OutRange.Points.1[order(OutRange.Points.1$Sampling.Date) ,]
 

 str(GC.standards)
 
 str(OutRange.Points.1)

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

##### Some Standards have NA ####

GC.standards[which(is.na(GC.standards$N2O)),]

#         Sample.Name Position Vial     CH4     CO2 N2O                           File Sampling.Day
# 138           0       51    1   1.914 188.711  NA SummaryGCN2O_20220623_B3B4.pdf     20220623
# 268         L50       52    1   1.879 278.501  NA       Summary20220719_B1B2.pdf     20220719
# 303           0      101    1   1.225 387.675  NA  SummaryGCN2O20220819_B1B2.pdf     20220819
# 392           0       51    1 237.220   0.598  NA  SummaryGCN2O20220901_B3B4.pdf     20220901
# 458         L50        2    1 167.949  19.377  NA  SummaryGCN2O20220927_B1B2.pdf     20220927

GC.standards[which(is.na(GC.standards$N2O)), "File"]

# [1] "SummaryGCN2O_20220623_B3B4.pdf" "Summary20220719_B1B2.pdf"      
# [3] "SummaryGCN2O20220819_B1B2.pdf"  "SummaryGCN2O20220901_B3B4.pdf" 
# [5] "SummaryGCN2O20220927_B1B2.pdf" 

GC.standards[GC.standards$File == "SummaryGCN2O_20220623_B3B4.pdf" & GC.standards$Position == 51 ,]

# Sample.Name Position Vial   CH4     CO2 N2O                           File Sampling.Day
#       0       51    1 1.914 188.711  NA SummaryGCN2O_20220623_B3B4.pdf     20220623
# Sampling.Date    GC.Date                   AnalysisName Factor.Name CH4.ppm CO2.ppm N2O.ppm
# 2022-06-23 2022-10-15 SummaryGCN2O_20220623_B3B4.pdf           0       0       0       0
# ANAL.DATE
# 2022-10-15

#### OK No N20 measurement in the GC report #######

GC.standards[GC.standards$File == "Summary20220719_B1B2.pdf" & GC.standards$Position == 52 ,]

# Sample.Name Position Vial   CH4     CO2 N2O                     File Sampling.Day Sampling.Date
# L50       52    1 1.879 278.501  NA Summary20220719_B1B2.pdf     20220719    2022-07-19
# GC.Date             AnalysisName Factor.Name CH4.ppm CO2.ppm N2O.ppm  ANAL.DATE
# 2022-09-04 Summary20220719_B1B2.pdf         L50     2.5     250     0.5 2022-09-04

#### OK No N20 measurement in the GC report #######

GC.standards[GC.standards$File == "SummaryGCN2O20220819_B1B2.pdf" & GC.standards$Position == 101 ,]

# Sample.Name Position Vial   CH4     CO2 N2O                          File Sampling.Day
# 0      101    1 1.225 387.675  NA SummaryGCN2O20220819_B1B2.pdf     20220819
# Sampling.Date    GC.Date                  AnalysisName Factor.Name CH4.ppm CO2.ppm N2O.ppm
# 2022-08-19 2022-10-20 SummaryGCN2O20220819_B1B2.pdf           0       0       0       0
# ANAL.DATE
# 2022-10-20

#### OK No N20 measurement in the GC report #######

GC.standards[GC.standards$File == "SummaryGCN2O20220901_B3B4.pdf" & GC.standards$Position == 51 ,]

# Sample.Name Position Vial    CH4   CO2 N2O                          File Sampling.Day
#  0       51    1 237.22 0.598  NA SummaryGCN2O20220901_B3B4.pdf     20220901
# Sampling.Date    GC.Date                  AnalysisName Factor.Name CH4.ppm CO2.ppm N2O.ppm
# 2022-09-01 2022-11-23 SummaryGCN2O20220901_B3B4.pdf           0       0       0       0
# ANAL.DATE
# 2022-11-23


# In this case CH4 does not have a standard and the data was displaced to the left. #
# CH4 = NA, CO2 = 237.22 , N2O = 0.598 #

GC.standards[GC.standards$File == "SummaryGCN2O20220901_B3B4.pdf" & GC.standards$Position == 51 ,
             
             c("CH4" , "CO2" , "N2O")] <- c(NA , 237.22 , 0.598  ) ;

#######################


GC.standards[GC.standards$File == "SummaryGCN2O20220927_B1B2.pdf" & GC.standards$Position == 2 ,]

# Sample.Name Position Vial     CH4    CO2 N2O                          File Sampling.Day
# L50        2    1 167.949 19.377  NA SummaryGCN2O20220927_B1B2.pdf     20220927
# Sampling.Date    GC.Date                  AnalysisName Factor.Name CH4.ppm CO2.ppm N2O.ppm
# 2022-09-27 2022-11-27 SummaryGCN2O20220927_B1B2.pdf         L50     2.5     250     0.5
# ANAL.DATE
# 2022-11-27

# In this case CH4 does not have a standard and the data was displaced to the left. #
# CH4 = NA, CO2 = 167.949 , N2O = 19.377 #

GC.standards[GC.standards$File == "SummaryGCN2O20220927_B1B2.pdf" & GC.standards$Position == 2 ,
             
             c("CH4" , "CO2" , "N2O")] <- c(NA , 167.949 , 19.377  ) ;

######################################################################################################



##### Do Quantile Regression  ####


Quantile.Reg.N2O <- rq( N2O ~ N2O.ppm, data = GC.standards, tau = c(0.05 , 0.25, 0.50 , 0.75 , 0.95)) ;

Quantile.Reg.N2O

summary(Quantile.Reg.N2O)

plot(NULL, xlim =range(GC.standards$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;


# plot(NULL, xlim =c(40,60), 
#      
#      ylim = c(40000,50000), 
#      
#      main = "N2O", xlab = "N2O" , ylab = "N2O.ppm" ) ;

####### calculations to plot quantile bands  #######

range(GC.standards$N2O.ppm)[1]
min(GC.standards$N2O.ppm)


##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards$N2O.ppm, na.rm = T) , max(GC.standards$N2O.ppm , na.rm = T) , 
               
               max(GC.standards$N2O.ppm , na.rm = T) , min(GC.standards$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

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

##### add outrange CO2 points ####

points(N2O ~ N2O.ppm , data = OutRange.Points.1,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

##### out of range CO2 points  are also outrange in N2O####



###############################################################################################################
#                           
#                                 Identify points outside the 95 percentile after
#
#                                 removal of CO2 points out of range
#
###############################################################################################################


##### remove out of range points  ######

str(GC.standards)

str(GC.standards[!GC.standards$P.ID %in% OutRange.Points ,])

GC.standards.2 <- GC.standards[!GC.standards$P.ID %in% OutRange.Points ,];

str(GC.standards.2)

#####  check  ### 

GC.standards.2[GC.standards.2$P.ID %in% OutRange.Points ,]


##### Do Quantile Regression  ####


Quantile.Reg.N2O <- rq( N2O ~ N2O.ppm, data = GC.standards.2, tau = c(0.05 , 0.25, 0.50 , 0.75 , 0.95)) ;

Quantile.Reg.N2O

summary(Quantile.Reg.N2O)

plot(NULL, xlim =range(GC.standards.2$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.2$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;


# plot(NULL, xlim =c(45,55),
# 
#      ylim = c(40000,50000),
# 
#      main = "N2O", xlab = "N2O" , ylab = "N2O.ppm" ) ;

####### calculations to plot quantile bands  #######

range(GC.standards.2$N2O.ppm)[1]
min(GC.standards.2$N2O.ppm)

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.2$N2O.ppm, na.rm = T) , max(GC.standards.2$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.2$N2O.ppm , na.rm = T) , min(GC.standards.2$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.2$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.2$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.2,
       
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
text(x = GC.standards.2$N2O.ppm , y = GC.standards.2$N2O, labels = GC.standards.2$P.ID )


########## Points that are outside the 95% interval ############################

OutRange.Points.2 <- as.character(sort(c(221 , 44 , 80 , 166 , 89 , 531 , 245 , 231 , 250 , 95 , 100 , 45 , 197 , 20 , 246 , 257 , 507 ))) ;

GC.standards.2[GC.standards.2$P.ID %in% OutRange.Points.2 ,]

GC.standards.3 <- GC.standards.2[!GC.standards.2$P.ID %in% OutRange.Points.2 ,]

str(GC.standards.3)


points(N2O ~ N2O.ppm , data = GC.standards.3,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")



###### check out of range points #####

OutRange.Points.1

OutRange.Points.3 <- GC.standards.2[GC.standards.2$P.ID %in% OutRange.Points.2 ,] ;

OutRange.Points.3 <- OutRange.Points.3 [order(OutRange.Points.3$Sampling.Date) , ] ;

OutRange.Points.All <- rbind(OutRange.Points.1 , OutRange.Points.3)


##############################################################################################################
#                           
#                           Do Quantile Regresion with out out of range points
#
###############################################################################################################

##### Do Quantile Regression for N2O ####


Quantile.Reg.N2O <- rq( N2O ~ N2O.ppm, data = GC.standards.3, tau = c(0.05 , 0.25, 0.50 , 0.75 , 0.95)) ;

Quantile.Reg.N2O

summary(Quantile.Reg.N2O)


##### Do Quantile Regression for CO2 ####

Quantile.Reg.CO2 <- rq( CO2 ~ CO2.ppm, data = GC.standards.3, tau = c(0.05 , 0.25, 0.50 , 0.75 , 0.95)) ;

Quantile.Reg.CO2

summary(Quantile.Reg.CO2)


##############################################################################################################
#                           
#                           GC Date [2022-07-13]  Sampling.Date [2022-06-29]
#
###############################################################################################################

###############################################################################################################
#                           
#                                       Check     N2O   Standards
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




plot(NULL, xlim = range(GC.standards.3$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$N2O.ppm, na.rm = T) , max(GC.standards.3$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.3$N2O.ppm , na.rm = T) , min(GC.standards.3$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$N2O.ppm , y = GC.standards.3$N2O, labels = GC.standards.3$P.ID )



##### Select conditions 


GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "N2O.ppm"] ,
     
     y =GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "N2O"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "P.ID"])





GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")
       
text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "N2O.ppm"] ,
     
     y =GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "N2O"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "P.ID"])
       



###############################################################################################################
#                           
#                                       Check     CO2   Standards
#
###############################################################################################################


plot(NULL, xlim = range(GC.standards.3$CO2.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$CO2 , na.rm = T), 
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$CO2.ppm, na.rm = T) , max(GC.standards.3$CO2.ppm , na.rm = T) , 
               
               max(GC.standards.3$CO2.ppm , na.rm = T) , min(GC.standards.3$CO2.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$CO2.ppm , y = GC.standards.3$CO2, labels = GC.standards.3$P.ID )


##### Select conditions  



GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "CO2.ppm"] ,
     
     y = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "CO2"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-06-29"  , "P.ID"])




GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "CO2.ppm"] ,
     
     y = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "CO2"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29"  , "P.ID"])


####### all the remaining points are within range for N2O or for CO2. Points are OK  ###################


##############################################################################################################
#                           
#                           GC Date [2022-08-10]  Sampling.Date [2022-06-23]
#
###############################################################################################################


GC.standards[GC.standards$GC.Date == "2022-08-10",]

###############################################################################################################
#                           
#                                       Check     CO2   Standards
#
###############################################################################################################


plot(NULL, xlim = range(GC.standards.3$CO2.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$CO2 , na.rm = T), 
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$CO2.ppm, na.rm = T) , max(GC.standards.3$CO2.ppm , na.rm = T) , 
               
               max(GC.standards.3$CO2.ppm , na.rm = T) , min(GC.standards.3$CO2.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$CO2.ppm , y = GC.standards.3$CO2, labels = GC.standards.3$P.ID )


##### Select conditions  

GC.standards[GC.standards$Sampling.Date == "2022-06-23" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-06-23" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "CO2.ppm"] ,
     
     y = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "CO2"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "P.ID"])



GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" , "CO2.ppm"] ,
     
     y = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" , "CO2"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" , "P.ID"])



###############################################################################################################
#                           
#                                       Check     N2O   Standards
#
###############################################################################################################

OutRange.Points.All


GC.standards[GC.standards$GC.Date == "2022-07-13",]


plot(NULL, xlim = range(GC.standards.3$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$N2O.ppm, na.rm = T) , max(GC.standards.3$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.3$N2O.ppm , na.rm = T) , min(GC.standards.3$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$N2O.ppm , y = GC.standards.3$N2O, labels = GC.standards.3$P.ID )



##### Select conditions

GC.standards[GC.standards$Sampling.Date == "2022-06-23" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-06-23" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "N2O.ppm"] ,
     
     y =GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "N2O"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-06-23" , "P.ID"])




GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" , "N2O.ppm"] ,
     
     y =GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" , "N2O"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-23" , "P.ID"])

####### all the remaining points are within range for N2O or for CO2. Points are OK  ###################


##############################################################################################################
#                           
#                           GC Date [2022-08-19 & 2022-08-30]  Sampling.Date [2022-06-29]
#
###############################################################################################################

OutRange.Points.All

GC.standards[GC.standards$GC.Date == "2022-08-19",]


###############################################################################################################
#                           
#                                       Check     CO2   Standards
#
###############################################################################################################


plot(NULL, xlim = range(GC.standards.3$CO2.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$CO2 , na.rm = T), 
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$CO2.ppm, na.rm = T) , max(GC.standards.3$CO2.ppm , na.rm = T) , 
               
               max(GC.standards.3$CO2.ppm , na.rm = T) , min(GC.standards.3$CO2.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$CO2.ppm , y = GC.standards.3$CO2, labels = GC.standards.3$P.ID )


##### Select conditions  

GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "CO2.ppm"] ,
     
     y = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "CO2"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "P.ID"])



GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "CO2.ppm"] ,
     
     y = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "CO2"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "P.ID"])



###############################################################################################################
#                           
#                                       Check     N2O   Standards
#
###############################################################################################################

OutRange.Points.All


GC.standards[GC.standards$GC.Date == "2022-07-13",]


plot(NULL, xlim = range(GC.standards.3$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$N2O.ppm, na.rm = T) , max(GC.standards.3$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.3$N2O.ppm , na.rm = T) , min(GC.standards.3$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$N2O.ppm , y = GC.standards.3$N2O, labels = GC.standards.3$P.ID )



##### Select conditions

GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "N2O.ppm"] ,
     
     y =GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "N2O"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-06-29" , "P.ID"])




GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "N2O.ppm"] ,
     
     y =GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "N2O"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-06-29" , "P.ID"])


####### all the remaining points are within range for N2O or for CO2. Points are OK  ###################

 

 
 
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
 
 
OutRange.Points.All

GC.standards[GC.standards$GC.Date == "2022-08-19",]


###############################################################################################################
#                           
#                                       Check     CO2   Standards
#
###############################################################################################################


plot(NULL, xlim = range(GC.standards.3$CO2.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$CO2 , na.rm = T), 
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$CO2.ppm, na.rm = T) , max(GC.standards.3$CO2.ppm , na.rm = T) , 
               
               max(GC.standards.3$CO2.ppm , na.rm = T) , min(GC.standards.3$CO2.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$CO2.ppm , y = GC.standards.3$CO2, labels = GC.standards.3$P.ID )


##### Select conditions  

GC.standards[GC.standards$Sampling.Date == "2022-07-19" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-07-19" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-07-19" , "CO2.ppm"] ,
     
     y = GC.standards[GC.standards$Sampling.Date == "2022-07-19" , "CO2"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-07-19" , "P.ID"])




GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" , "CO2.ppm"] ,
     
     y = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" , "CO2"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" , "P.ID"])


######### All standards were below the median   for CO2 ########


###############################################################################################################
#                           
#                                       Check     N2O   Standards
#
###############################################################################################################

OutRange.Points.All


GC.standards[GC.standards$GC.Date == "2022-09-04",]


plot(NULL, xlim = range(GC.standards.3$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;



# plot(NULL, xlim = c(0,5), 
#      
#      ylim = c(0, 2000), 
#      
#      main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$N2O.ppm, na.rm = T) , max(GC.standards.3$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.3$N2O.ppm , na.rm = T) , min(GC.standards.3$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$N2O.ppm , y = GC.standards.3$N2O, labels = GC.standards.3$P.ID )



##### Select conditions

GC.standards[GC.standards$Sampling.Date == "2022-07-19" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-07-19" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-07-19" , "N2O.ppm"] ,
     
     y =GC.standards[GC.standards$Sampling.Date == "2022-07-19" , "N2O"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-07-19" , "P.ID"])

GC.standards[GC.standards$Sampling.Date == "2022-07-19" ,]



points(N2O ~ N2O.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" , "N2O.ppm"] ,
     
     y =GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" , "N2O"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-07-19" , "P.ID"])


######### All standards were below the median for CO2 and N2O ########  What to do?

OutRange.Points.All

 
 
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
 
OutRange.Points.All

GC.standards[GC.standards$GC.Date == "2022-09-10",]


###############################################################################################################
#                           
#                                       Check     CO2   Standards
#
###############################################################################################################


plot(NULL, xlim = range(GC.standards.3$CO2.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$CO2 , na.rm = T), 
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$CO2.ppm, na.rm = T) , max(GC.standards.3$CO2.ppm , na.rm = T) , 
               
               max(GC.standards.3$CO2.ppm , na.rm = T) , min(GC.standards.3$CO2.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$CO2.ppm , y = GC.standards.3$CO2, labels = GC.standards.3$P.ID )


##### Select conditions  


GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" , "CO2.ppm"] ,
     
     y = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" , "CO2"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" , "P.ID"])


######### All standards are within the 95 quartile ########


###############################################################################################################
#                           
#                                       Check     N2O   Standards
#
###############################################################################################################

OutRange.Points.All


GC.standards[GC.standards$GC.Date == "2022-09-10",]


plot(NULL, xlim = range(GC.standards.3$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;



# plot(NULL, xlim = c(0,5), 
#      
#      ylim = c(0, 2000), 
#      
#      main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$N2O.ppm, na.rm = T) , max(GC.standards.3$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.3$N2O.ppm , na.rm = T) , min(GC.standards.3$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$N2O.ppm , y = GC.standards.3$N2O, labels = GC.standards.3$P.ID )



##### Select conditions

GC.standards[GC.standards$Sampling.Date == "2022-09-10" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-09-10" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-09-10" , "N2O.ppm"] ,
     
     y =GC.standards[GC.standards$Sampling.Date == "2022-09-10" , "N2O"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-09-10" , "P.ID"])


GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" , "N2O.ppm"] ,
     
     y =GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" , "N2O"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-10" , "P.ID"])



######### All standards were within the 95 quartile ########  

OutRange.Points.All


 ##############################################################################################################
 #                           
 #                         GC Date ["2022-11-27"]  Sampling.Date [2022-09-27]
 #
 ###############################################################################################################
 
 
########### GC Note
 # 202200927_B1B2
 # 
 # There were no low standards (L-100%, L-50%) standards left.
 # Those vials do not have any gas except what was left from the previous GC analysis.
###########
 

###############################################################################################################
#                           
#                                       Check     CO2   Standards
#
###############################################################################################################


plot(NULL, xlim = range(GC.standards.3$CO2.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$CO2 , na.rm = T), 
     
     main = "CO2", xlab = "CO2.ppm" , ylab = "CO2" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$CO2.ppm, na.rm = T) , max(GC.standards.3$CO2.ppm , na.rm = T) , 
               
               max(GC.standards.3$CO2.ppm , na.rm = T) , min(GC.standards.3$CO2.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1],  Quantile.Reg.CO2$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.CO2$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.CO2$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$CO2.ppm)),
               
               Quantile.Reg.CO2$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(CO2 ~ CO2.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$CO2.ppm , y = GC.standards.3$CO2, labels = GC.standards.3$P.ID )


##### Select conditions  


GC.standards[GC.standards$Sampling.Date == "2022-09-27" ,]

points(CO2 ~ CO2.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-09-27" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-09-27" , "CO2.ppm"] ,
     
     y = GC.standards[GC.standards$Sampling.Date == "2022-09-27" , "CO2"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-09-27" , "P.ID"])



GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" ,]


points(CO2 ~ CO2.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" ,] ,
       
       pch = 16 , 
       
       main="CO2", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" , "CO2.ppm"] ,
     
     y = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" , "CO2"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" , "P.ID"])



######### All standards are within the 95 quartile ########


###############################################################################################################
#                           
#                                       Check     N2O   Standards
#
###############################################################################################################

OutRange.Points.All


GC.standards[GC.standards$GC.Date == "2022-09-27",]


plot(NULL, xlim = range(GC.standards.3$N2O.ppm, na.rm = T), 
     
     ylim = range(GC.standards.3$N2O , na.rm = T), 
     
     main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;



# plot(NULL, xlim = c(0,5), 
#      
#      ylim = c(0, 2000), 
#      
#      main = "N2O", xlab = "N2O.ppm" , ylab = "N2O" ) ;

##### 95% confidence quartile polygon envelope   #########

polygon.x <- c(min(GC.standards.3$N2O.ppm, na.rm = T) , max(GC.standards.3$N2O.ppm , na.rm = T) , 
               
               max(GC.standards.3$N2O.ppm , na.rm = T) , min(GC.standards.3$N2O.ppm, na.rm = T)) ;

polygon.y <- c(y = Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1],  Quantile.Reg.N2O$coefficients[,"tau= 0.95"][1] +
                 
                 (Quantile.Reg.N2O$coefficients[,"tau= 0.95"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] + (Quantile.Reg.N2O$coefficients[,"tau= 0.05"][2] * max(GC.standards.3$N2O.ppm)),
               
               Quantile.Reg.N2O$coefficients[,"tau= 0.05"][1] ) ;

points(polygon.x,polygon.y, pch = 16 , col = "magenta")

polygon(x = polygon.x ,  y = polygon.y, col = adjustcolor( col = "gray", alpha.f = 0.1), border = NA)


####### All points in gray  #######

points(N2O ~ N2O.ppm , data = GC.standards.3,
       
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
#text(x = GC.standards.3$N2O.ppm , y = GC.standards.3$N2O, labels = GC.standards.3$P.ID )



##### Select conditions

GC.standards[GC.standards$Sampling.Date == "2022-09-27" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards[GC.standards$Sampling.Date == "2022-09-27" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "red")

text(x = GC.standards[GC.standards$Sampling.Date == "2022-09-27" , "N2O.ppm"] ,
     
     y =GC.standards[GC.standards$Sampling.Date == "2022-09-27" , "N2O"] ,
     
     labels = GC.standards[GC.standards$Sampling.Date == "2022-09-27" , "P.ID"])


GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" ,]

points(N2O ~ N2O.ppm ,  data = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" ,] ,
       
       pch = 16 , 
       
       main="N2O", 
       
       col = "blue")

text(x = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" , "N2O.ppm"] ,
     
     y =GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" , "N2O"] ,
     
     labels = GC.standards.3[GC.standards.3$Sampling.Date == "2022-09-27" , "P.ID"])
 
 ###############################################################################################################
 #                           
 #                                 Write curated Standards Calibration Data Set
 #
 ###############################################################################################################  
 
 # Working.Date <- format(Sys.time() , "%Y_%m_%d_%H_%M_%S") ;
 # 
 # write.csv(x =GC.standards, file = paste0("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",
 # 
 # "Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\" ,
 # 
 # "DataAnalysis\\RCode\\GCResultsAnalysis\\FluxDataAnalysisResults\\Standards2022_",
 # 
 # Working.Date , ".csv"), row.names = F )
 # 
 