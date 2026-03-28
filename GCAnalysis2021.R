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

##############################################################################################################
#                           
#                           Working with Standards obtained in 2021
#
###############################################################################################################

head(GC.standards);

  
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

###############################################################################################################
#                           
#                                            CO2   Standards
#
###############################################################################################################  
  
str(GC.standards)

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
  
  
  