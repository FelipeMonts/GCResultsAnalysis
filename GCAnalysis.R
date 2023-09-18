##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#     This program is focused on analyzing standards for calibration
# 
# 
#  Felipe Montes 2022/08/23
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

setwd("D:\\Felipe\\Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\RCode\\GCResultsAnalysis")



#### N2O Sampling year  #####

Year = 2021

# Year = 2022


PeakArea.results <- read.csv(file = paste0("FluxDataAnalysisResults\\GCcompiledResults" , Year ,".csv" ) , header = T)


###############################################################################################################
#                           
#                               Working with standards
#
###############################################################################################################

str(PeakArea.results)

unique(PeakArea.results$Sampling.Day)

tail(unique(PeakArea.results$Sampling.Date))



head(PeakArea.results)

which(is.na(PeakArea.results))
   
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






###############################################################################################################
#                           
#                           Separating the standards by year because their names are different
#
###############################################################################################################

head(GC.standards);

if(Year == 2021) {
  
  # Levels Names for 2021
  
  # [1] "0PerSTD"    "0PerSTDA"   "100PerSTD"  "100PerSTDA" "25PerSTD"   "25PerSTDA"  "50PerSTD"   "50PerSTDA"  "75PerSTD"  
  # [10] "75PerSTDA"
  
  # CH4	CO2	N2O
  # 0perSTD	ppm 	uL/Lgas	0	0	0
  # 25perSTDA	ppm 	uL/Lgas	1.25	125	0.25
  # 50perSTDA	ppm 	uL/Lgas	2.5	250	0.5
  # 75perSTDA	ppm 	uL/Lgas	3.75	375	0.75
  # 100perSTDA	ppm 	uL/Lgas	5	500	1
  # 25perSTD	ppm  uL/Lgas 12.5 1250 1.25
  # 50PerSTD	ppm 	uL/Lgas	25	2500	25
  # 75perSTD	ppm 	uL/Lgas	37.5	3750	37.5
  # 100PerSTD	ppm 	uL/Lgas	50	5000	50
  
  GC.standards[GC.standards$Sample.Name == "100PerSTDA",c('CH4.ppm')] <- 5 ;
  GC.standards[GC.standards$Sample.Name == "100PerSTDA",c('CO2.ppm')] <- 500 ;
  GC.standards[GC.standards$Sample.Name == "100PerSTDA",c('N2O.ppm')] <- 1 ;
  
  
  GC.standards[GC.standards$Sample.Name == '100PerSTD',c('CH4.ppm')] <- 50 ;
  GC.standards[GC.standards$Sample.Name == '100PerSTD',c('CO2.ppm')] <- 5000 ;
  GC.standards[GC.standards$Sample.Name == '100PerSTD',c('N2O.ppm')] <- 50 ;    
  
  
  GC.standards[GC.standards$Sample.Name == "50PerSTDA",c('CH4.ppm')] <- 2.5  ;
  GC.standards[GC.standards$Sample.Name == "50PerSTDA",c('CO2.ppm')] <- 250  ;
  GC.standards[GC.standards$Sample.Name == "50PerSTDA",c('N2O.ppm')] <- 0.5  ;
  
  
  GC.standards[GC.standards$Sample.Name == '50PerSTD',c('CH4.ppm')] <- 25 ;
  GC.standards[GC.standards$Sample.Name == '50PerSTD',c('CO2.ppm')] <- 2500 ;
  GC.standards[GC.standards$Sample.Name == '50PerSTD',c('N2O.ppm')] <- 25;
 
  GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CH4.ppm')] <- 3.75 ;
  GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CO2.ppm')] <- 375  ;
  GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('N2O.ppm')] <- 0.75 ; 
  
   GC.standards[GC.standards$Sample.Name == '75PerSTD',c('CH4.ppm')] <- 37.5 ;
   GC.standards[GC.standards$Sample.Name == '75PerSTD',c('CO2.ppm')] <- 3750  ;
   GC.standards[GC.standards$Sample.Name == '75PerSTD',c('N2O.ppm')] <- 37.5 ; 
   
  GC.standards[GC.standards$Sample.Name == "25PerSTDA",c('CH4.ppm')] <- 1.25 ;
  GC.standards[GC.standards$Sample.Name == "25PerSTDA",c('CO2.ppm')] <- 125  ;
  GC.standards[GC.standards$Sample.Name == "25PerSTDA",c('N2O.ppm')] <- 0.25  ;
  
  GC.standards[GC.standards$Sample.Name == "25PerSTD",c('CH4.ppm')] <- 12.5 ;
  GC.standards[GC.standards$Sample.Name == "25PerSTD",c('CO2.ppm')] <- 1250  ;
  GC.standards[GC.standards$Sample.Name == "25PerSTD",c('N2O.ppm')] <- 0.25  ;
  
  GC.standards[GC.standards$Sample.Name == "0PerSTD" |  GC.standards$Sample.Name == "0PerSTDA" ,
               
               c('CH4.ppm', 'CO2.ppm', 'N2O.ppm')] <- 1e-5 
  
  GC.standards[which(is.na(GC.standards[,c('CH4.ppm')])), ]
                           
  GC.standards[which(is.na(GC.standards[,c('CO2.ppm')])), ]  
  
  GC.standards[which(is.na(GC.standards[,c('N2O.ppm')])), c('Sample.Name')]  
                          
  
} else {
    
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
    # H100 PerSTD	ppm 	uL/Lgas	100	5000	50
    
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
    
    
    GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CH4.ppm')] <-3.75 ;
    GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CO2.ppm')] <- 375  ;
    GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('N2O.ppm')] <- 0.75 ;
    
    
    GC.standards[GC.standards$Sample.Name == '0',c('CH4.ppm', 'CO2.ppm', 'N2O.ppm')] <- 1e-5 ;
    
    
  }
    
    
  





##### Exploring the standards by date of analysis Date Of Analysis

GC.standards$ANAL.DATE<-as.factor(GC.standards$GC.Date)  ;

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data=GC.standards, type="b", main="CO2", auto.key = T)

xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Position , data=GC.standards, type="p",main="CO2", auto.key = T)

simpleKey(text = GC.standards$Position )


str(GC.standards)

if (Year == 2021) {
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ])
  
  boxplot(CO2 ~ ANAL.DATE , data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ]  )
  
  boxplot(CH4 ~ ANAL.DATE , data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ]  )
  
  boxplot(N2O ~ ANAL.DATE , data = GC.standards[GC.standards$Factor.Name == "0PerSTD" | GC.standards$Factor.Name == "0PerSTDA",  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "25PerSTDA" ,  ]) 
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "25PerSTDA" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "50PerSTD" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "100PerSTD" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "100PerSTD" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "50PerSTDA" ,  ]) 
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "50PerSTDA" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  ]  )
  
  boxplot(CH4 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  ]  )
  
  boxplot(N2O ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "100PerSTDA" ,  ]  )
  
  
  xyplot(N2O~N2O.ppm, data=GC.standards, type="p",main="N2O")
  
  xyplot(CH4~CH4.ppm, data=GC.standards, type="b",main="CH4")
  
}  else {
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "0" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE , data = GC.standards[GC.standards$Factor.Name == "0" ,  ]  )
  
  boxplot(CH4 ~ ANAL.DATE , data = GC.standards[GC.standards$Factor.Name == "0" ,  ]  )
  
  boxplot(N2O ~ ANAL.DATE , data = GC.standards[GC.standards$Factor.Name == "0" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "L25" ,  ]) 
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "L25" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "L50" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "L50" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "L100" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "L100" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "H50" ,  ]) 
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "H50" ,  ]  )
  
  plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "H100" ,  ])
  
  boxplot(CO2 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "H100" ,  ]  )
  
  boxplot(CH4 ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "H100" ,  ]  )
  
  boxplot(N2O ~ ANAL.DATE, data = GC.standards[GC.standards$Factor.Name == "H100" ,  ]  )
  
  
  xyplot(N2O~N2O.ppm, data=GC.standards, type="p",main="N2O")
  
  xyplot(CH4~CH4.ppm, data=GC.standards, type="b",main="CH4")  
  
  
}



#### Adding an factor identification for each of the different standards in a run (2022 data)

# GC.standards$Series <- "None" ;
# 
# GC.standards[GC.standards$Position %in% c(1:5), c("Series")] <- 1 ;
# 
# GC.standards[GC.standards$Position %in% c(30:34), c("Series")] <- 2 ;
# 
# GC.standards[GC.standards$Position %in% c(51:55), c("Series")] <- 3 ;
# 
# GC.standards[GC.standards$Position %in% c(80:84), c("Series")] <- 4 ;
# 
# GC.standards[GC.standards$Position %in% c(101:105), c("Series")] <- 5 ;
# 
# 
# head(GC.standards)
# 
# GC.standards$Series <- as.factor(GC.standards$Series) ;
# 
# str(GC.standards)
# 
# 
# xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Series , data=GC.standards, 
#        
#        type="b",main="CO2", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3);
# 
# 
# xyplot(CH4 ~ CH4.ppm | GC.Date , groups = Series , data=GC.standards, 
#        
#        type="b",main="CH4", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3);
# 
# xyplot(N2O ~ N2O.ppm | GC.Date , groups = Series , data=GC.standards, 
#        
#        type="b",main="N2O", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3);
# 
# levels(GC.standards$Factor.Name)




#### Adding ablines to the lattice xyplot using the panel.abline parameter in the panel function.


# #### Testing lm to add the ablines ##
# 
# plot(CO2.ppm ~ CO2, data = GC.standards[GC.standards$Series == "1" & GC.standards$GC.Date == "2022-09-02",] )
# 
# lm(CO2.ppm ~ CO2, data = GC.standards[GC.standards$Series == "1" & GC.standards$GC.Date == "2022-09-02",] )
# 
# str(lm(CO2.ppm ~ CO2, data = GC.standards[GC.standards$Series == "1" & GC.standards$GC.Date == "2022-09-02",] ))
# 
# by(data = GC.standards, INDICES = GC.standards[, c("ANAL.DATE" , "Series")], function(x) lm(CO2.ppm ~ CO2, data = x))

# An example from from https://stackoverflow.com/questions/11949766/how-to-add-abline-with-lattice-xyplot-function
# 
# xyplot(Neff ~ Eeff, data = phuong,
#        panel = function(x, y) {
#          panel.xyplot(x, y)
#          panel.abline(lm(y ~ x))
#        }, 
#        xlab = "Energy efficiency (%)", 
#        ylab = "Nitrogen efficiency (%)")


# xyplot(N2O.ppm ~ N2O | GC.Date , groups = Series , data=GC.standards, 
#        
#        panel = function(x, y) { panel.xyplot(x, y)
#          
#          panel.xyplot(x, y) 
#          
#          panel.abline(lm(y ~ x))
#          
#          panel.text(3000, 30,labels = signif(lm(y ~ x)$coefficients[2], digits = 3))
#          
#          panel.abline(a= 30, b=0, col="RED")
#        },
#        
#        type="b",main="N2O", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3)
#        
#        
# 
# 
# 




###############################################################################################################
#                           
#        Regression for standards that were obtained on the same day of a particular GC analysis
#
###############################################################################################################




str(GC.standards)

levels(GC.standards$ANAL.DATE)[[1]]

GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ]

plot(CO2~CO2.ppm, data = GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ], col= "blue") ;


### Ordinary Least Square Regression OLS

OLS.regression <- lm(CO2~CO2.ppm, data = GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ] ) ;

summary(OLS.regression)

str(OLS.regression)

OLS.regression$coefficients[[1]]


abline(a = OLS.regression$coefficients[[1]], b = OLS.regression$coefficients[[2]] , col="red")



### quantile regression

Quantile.Reg <- rq( CO2~CO2.ppm, data = GC.standards[GC.standards$ANAL.DATE == levels(GC.standards$ANAL.DATE)[[8]], ], tau = c(0.25, 0.50, 0.75) )

summary(Quantile.Reg)


str(Quantile.Reg)

abline(a = Quantile.Reg$coefficients[1,1], b = Quantile.Reg$coefficients[2,1] , col="cyan")

abline(a = Quantile.Reg$coefficients[1,2], b = Quantile.Reg$coefficients[2,2] , col="green")

abline(a = Quantile.Reg$coefficients[1,3], b = Quantile.Reg$coefficients[2,3] , col="magenta")

# xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Series , data = GC.standards, 
#        
#        type="b",main="CO2", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3); 


###############################################################################################################
#                           
#        Comparison between ordinary least squares regression and quantile regresson for the standards
#
###############################################################################################################


####   CO2  ####

xyplot(CO2.ppm ~ CO2 | GC.Date , groups = Series , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 4000,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 3500,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 2000,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 2500,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="CO2", auto.key = T)


xyplot(CO2.ppm ~ CO2 , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 4000,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 3500,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 2000,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 2500,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="N2O", auto.key = T)


####   N2O  ####

xyplot(N2O.ppm ~ N2O | GC.Date , groups = Series , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="N2O", auto.key = T)


xyplot(N2O.ppm ~ N2O , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(10000, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(10000, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(20000, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(20000, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="N2O", auto.key = T)



####   CH4  ####

xyplot(CH4.ppm ~ CH4 | GC.Date , groups = Series , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(200, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(200, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(400, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(400, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="CH4", auto.key = T)


xyplot(CH4.ppm ~ CH4 , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
         
         panel.text(200, 40,labels = signif(rq(y ~ x)$coefficients[2], digits = 3), col = "red")
         
         panel.text(200, 35,labels = signif(rq(y ~ x)$coefficients[1], digits = 3), col = "red")
         
         panel.text(400, 20,labels = signif(lm(y ~ x)$coefficients[1], digits = 3), col = 'black' )
         
         panel.text(400, 25,labels = signif(lm(y ~ x)$coefficients[2], digits = 3), col = 'black' )
       },
       
       type="b",main="CH4", auto.key = T)


#################################################################################################################
# 
# the best model for the standard calibration is the one using all the standards data in a linear regression
# 
# 
################################################################################################################## 
 

#### CO2 standards calibration ####

CO2.Calibration <- lm(CO2.ppm ~ CO2 , data = GC.standards) ;

summary(CO2.Calibration)

# Call:
#   lm(formula = CO2.ppm ~ CO2, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2556.7  -272.6  -147.0   123.9  3928.5 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -31.605388  39.270520  -0.805    0.421    
# CO2           0.269938   0.004337  62.242   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 696.6 on 599 degrees of freedom
# Multiple R-squared:  0.8661,	Adjusted R-squared:  0.8659 
# F-statistic:  3874 on 1 and 599 DF,  p-value: < 2.2e-16


#### N20 standards calibration ####

N2O.Calibration <- lm(N2O.ppm ~ N2O , data = GC.standards) ;

summary(N2O.Calibration)

# Call:
#   lm(formula = N2O.ppm ~ N2O, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.093  -1.763  -1.525  -0.551  41.403 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.425e+00  3.382e-01   4.214  2.9e-05 ***
#   N2O         1.494e-03  2.159e-05  69.213  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.605 on 595 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.8895,	Adjusted R-squared:  0.8893 
# F-statistic:  4790 on 1 and 595 DF,  p-value: < 2.2e-16
# 

#### CH4 standards calibration ####

CH4.Calibration <- lm(CH4.ppm ~ CH4 , data = GC.standards) ;

summary(CH4.Calibration)

# Call:
#   lm(formula = CH4.ppm ~ CH4, data = GC.standards)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -30.104  -2.879  -1.213   1.128  40.544 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2.465241   0.353385   6.976 8.06e-12 ***
#   CH4         0.116513   0.001816  64.143  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.786 on 599 degrees of freedom
# Multiple R-squared:  0.8729,	Adjusted R-squared:  0.8727 
# F-statistic:  4114 on 1 and 599 DF,  p-value: < 2.2e-16




# ###############################################################################################################
#                           
#                              Exploring the data without standards
#
###############################################################################################################

##### Data with no standards included

str(GC.standards)

str(PeakArea.results)

levels(GC.standards$Factor.Name)

GC.Data.NoSTD<-PeakArea.results[!(PeakArea.results$Sample.Name %in% levels(GC.standards$Factor.Name)),];

str(GC.Data.NoSTD)


plot.CH4.hist.dat<-hist(GC.Data.NoSTD$CH4)
plot.CH4.density.dat<-density(GC.Data.NoSTD$CH4, na.rm=T)

plot.CO2.hist.dat<-hist(GC.Data.NoSTD$CO2)
plot.CO2.density.dat<-density(GC.Data.NoSTD$CO2, na.rm=T)


plot.N2O.hist.dat<-hist(GC.Data.NoSTD$N2O)
plot.N2O.density.dat<-density(GC.Data.NoSTD$N2O, na.rm=T)


###############################################################################################################
#                          
#            Calculation of concentration based on the Standard gas concentrations
#  
#
###############################################################################################################


str(GC.Data.NoSTD)


##### CO2 ######

GC.Data.NoSTD$CO2.Intercept <- coefficients(CO2.Calibration)[1] ;

GC.Data.NoSTD$CO2.Slope <- coefficients(CO2.Calibration)[2] ;

GC.Data.NoSTD$CO2.ppm <- (GC.Data.NoSTD$CO2 * GC.Data.NoSTD$CO2.Slope) + GC.Data.NoSTD$CO2.Intercept ;


##### N2O ######

GC.Data.NoSTD$N2O.Intercept <- coefficients(N2O.Calibration)[1] ;

GC.Data.NoSTD$N2O.Slope <- coefficients(N2O.Calibration)[2] ;

GC.Data.NoSTD$N2O.ppm <- (GC.Data.NoSTD$N2O * GC.Data.NoSTD$N2O.Slope) + GC.Data.NoSTD$N2O.Intercept ;



##### CH4 ######

GC.Data.NoSTD$CH4.Intercept <- coefficients(CH4.Calibration)[1] ;

GC.Data.NoSTD$CH4.Slope <- coefficients(CH4.Calibration)[2] ;

GC.Data.NoSTD$CH4.ppm <- (GC.Data.NoSTD$CH4 * GC.Data.NoSTD$CH4.Slope) + GC.Data.NoSTD$CH4.Intercept ;


###############################################################################################################
#                           
#                               Organizing the data for visualization
#
###############################################################################################################

str(GC.Data.NoSTD)



### Organizing the data according to Treatments

unique(GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$Treatment<-c("NONE");


GC.Data.NoSTD[grep("AT",GC.Data.NoSTD$Sample.Name), c("Treatment")]<-c("A");

GC.Data.NoSTD[grep("BT",GC.Data.NoSTD$Sample.Name), c("Treatment")]<-c("B");

GC.Data.NoSTD[grep("CT",GC.Data.NoSTD$Sample.Name), c("Treatment")]<-c("C");

GC.Data.NoSTD[grep("DT",GC.Data.NoSTD$Sample.Name), c("Treatment")]<-c("D");

### Check if there was any treatment left with "NONE" label

GC.Data.NoSTD[which(GC.Data.NoSTD$Treatment == "NONE"), ];




### Organizing the data according to Blocks

grep("B1",GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$BLOCK<-c(9999);

GC.Data.NoSTD[grep("B1",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(1);

GC.Data.NoSTD[grep("B2",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(2);

GC.Data.NoSTD[grep("B3",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(3);

GC.Data.NoSTD[grep("B4",GC.Data.NoSTD$Sample.Name), c("BLOCK")]<-c(4);

### Check if there was any BLOCK labeled 9999

GC.Data.NoSTD[which(GC.Data.NoSTD$BLOCK == 9999 ), ];



### Organizing the data according to CoverCrop

grep("3Spp",GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$CoverCrop<-c("NONE");

GC.Data.NoSTD[grep("3Spp",GC.Data.NoSTD$Sample.Name), c("CoverCrop")]<-c("3Spp");

GC.Data.NoSTD[grep("Clover",GC.Data.NoSTD$Sample.Name), c("CoverCrop")]<-c("Clover");

GC.Data.NoSTD[grep("Trit",GC.Data.NoSTD$Sample.Name), c("CoverCrop")]<-c("Trit");


### Check if there was any  CoverCrop labeled "NONE"

GC.Data.NoSTD[which(GC.Data.NoSTD$CoverCrop == "NONE" ), ];


### Organizing the data according to Sampling Time Order

grep("T0",GC.Data.NoSTD$Sample.Name)

GC.Data.NoSTD$Sampling.Time<-c(9999);

GC.Data.NoSTD[grep("T0",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(0);

GC.Data.NoSTD[grep("T15",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(15);

GC.Data.NoSTD[grep("T30",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(30);

GC.Data.NoSTD[grep("T45",GC.Data.NoSTD$Sample.Name), c("Sampling.Time")]<-c(45);


### Check if there was any Sampling.Time left with "NONE" label

GC.Data.NoSTD[which(GC.Data.NoSTD$Sampling.Time==9999),];


### Converting experimental designations into factors

GC.Data.NoSTD$Treatment.F<-as.factor(GC.Data.NoSTD$Treatment) ;

GC.Data.NoSTD$BLOCK.F<-as.factor(GC.Data.NoSTD$BLOCK) ;

GC.Data.NoSTD$CoverCrop.F<-as.factor(GC.Data.NoSTD$CoverCrop) ;





                  



###############################################################################################################
#                           
#                               Exploratory Data visualization
#
###############################################################################################################

str(GC.Data.NoSTD)

levels(GC.Data.NoSTD$Treatment.F)

levels(GC.Data.NoSTD$BLOCK.F)

levels(GC.Data.NoSTD$CoverCrop.F)

GC.Data.NoSTD[GC.Data.NoSTD$CoverCrop.F == "Clover" ,]

xyplot(CO2.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD$CO2.ppm)) ,   
       
       type="o", auto.key = T, main = "CO2");


xyplot(N2O.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD$N2O.ppm)) ,   
       
       type="o", auto.key = T , main = "N2O");


xyplot(CH4.ppm ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, 
       
       data = GC.Data.NoSTD , xlim=c(0,45), ylim = c(0, max(GC.Data.NoSTD$CH4.ppm)) , 
       
       type="o", auto.key = T , main = "CH4");





###############################################################################################################
#
#  Reference data taken from Allison Kohele's Calculations Excel Files
#
###############################################################################################################





Chamber.Dimensions<-data.frame(DIMENSION=c("Length", "Width" , "Height", "Volume" , "Surface.Area"),UNITS=c("m"), VALUE=c(0.52705, 0.32385, 0.1016, 9999, 9999));

Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Volume", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3]*Chamber.Dimensions[3,3] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Surface.Area", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3] ;

Molar.Mass<-data.frame(GAS=c("CH4" , "CO2" , "N2O"), UNITS=c("g/mol"), VALUE=c(16.04, 44.01, 44.013));

Gas.Law<-data.frame(UNITS=c("L-atm/Mol-K", "J/K-Mol", "m3-Pa/K-Mol", "Kg-m2-s2/K-Mol", "m3-atm/K-Mol"), VALUE=c(0.08205736, 8.314462,8.314462, 8.314462, 8.205736e-5 ))  ;


###############################################################################################################
#
#  Calculation of flux rates based on the paper:
# 
# Pedersen, A. R., S. O. Petersen, and K. Schelde. “A Comprehensive Approach to Soil-Atmosphere Trace-Gas Flux 
# 
# Estimation with Static Chambers.” European Journal of Soil Science 61, no. 6 (2010): 888–902. 
# 
# https://doi.org/10.1111/j.1365-2389.2010.01291.x.
# 
# 
# and the r package : 

# Pedersen, Asger R. “HMR: Flux Estimation with Static Chamber Data,” May 20, 2020. https://CRAN.R-project.org/package=HMR.
# 
#
###############################################################################################################

str(GC.Data.NoSTD)

unique(GC.Data.NoSTD$Sampling.Day)


####### Creating series data names for the HMR analysis ############

str(GC.Data.NoSTD)
unique(GC.Data.NoSTD$Sampling.Date)
unique(GC.Data.NoSTD$Sampling.Day)

 
# CO2.ppm : Sampling.Day - BLOCK. - CoverCrop.F - Treatment.F  
# 
# N2O.ppm : Sampling.Day - BLOCK. - CoverCrop.F - Treatment.F  
# 
# CH4.ppm : Sampling.Day - BLOCK. - CoverCrop.F - Treatment.F  
# 
# names(Test.data.HMR.CO2.1) <- c("Series" , "V" , "A" , "Time" , "Concentration")
# 
# write.table(x = Test.data.HMR.CO2.1, sep = ";", dec = "." ,file = "TEST_DATA.csv", row.names = F)

GC.Data.NoSTD$Series <- paste( GC.Data.NoSTD$Sampling.Day , GC.Data.NoSTD$BLOCK.F , 
                               
                               GC.Data.NoSTD$CoverCrop.F , GC.Data.NoSTD$Treatment.F, sep = "_") ;

head(GC.Data.NoSTD)

#### there is  duplicated data for a Sample day 20220615, need to get rid of the duplicated data


GC.Data.NoSTD$Series.Sampling.Time <- paste(GC.Data.NoSTD$Series , GC.Data.NoSTD$Sampling.Time , sep = "_" )

str(GC.Data.NoSTD)

duplicated(GC.Data.NoSTD$Series.Sampling.Time)

GC.Data.NoSTD.1 <- GC.Data.NoSTD[ duplicated(GC.Data.NoSTD$Series.Sampling.Time),]



save.image(file = paste0("FluxDataAnalysisResults\\GCAnalysis" , Year , ".RData"))

