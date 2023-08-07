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

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)

library(quantreg)



###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

# setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")




###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored

File.List.directory.path <- "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Felipe2022\\Results" ;

File.List.directory <- list.files(File.List.directory.path); length(File.List.directory) ; 

File.List.Sub.directory <- list.files(paste0(File.List.directory.path , "\\" ,File.List.directory [[1]] )); length(File.List.Sub.directory) ;

Summary.directory <- grep("Sum", File.List.Sub.directory, value = T) ;


###############################################################################################################
#                           Read all the GC result reports in the File.List
###############################################################################################################



## initialize the dataframe to collect all the data in the directory files in the Excel.Results.Files

PeakArea.results.0<-data.frame(Sample.Name = character(), Position = integer() , Vial.number = integer(), 
                               
                               CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), File = character(),
                               
                               Sampling.Day = character(),  DateOfAnalysis = character(), AnalysisName = character() );





###############################################################################################################
# 
# Inputs required by the function ReadGCReportPDF
# 
#  1-> GCPDF.File.path = path to the file containing the Gas Chromatograph analysis report in pdf format
#  
#  in the code below GCPDF.File=paste0(File.List.directory,"\\",PDF.Results.Files[1])
# 
#    GCPDF.File.path = "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results"
#  
#  2-> GCPDF.File.name= Name of the Gas Cromatograph analysis report in pdf format
# 
# 
#    GCPDF.File.name="2021027B3B4peakareas.pdf" 
# 
# 
# 
###############################################################################################################



# i = 20

#for (i in seq(1,length(File.List.directory)))

for (i in seq(1,length(File.List.directory)-2)){
  
  File.List.Sub.directory <- list.files(paste0(File.List.directory.path , "\\" ,File.List.directory [[i]] ));
  
  Summary.directory <- grep("Sum", File.List.Sub.directory, value = T) ;
  
  
  File.List <- list.files(paste0(File.List.directory.path , "\\" , File.List.directory [[i]] , "\\" , 
                                 
                                Summary.directory )); length(File.List) ;
  
  
  
  PDF.Results.Files<-File.List[grep(".pdf", x = File.List)] ;
  
 # j = 2
  
  for (j in seq(1, length(PDF.Results.Files))) {
    
    PeakArea.results.1<-ReadGCReportPDF2022(GCPDF.File.path = paste0(File.List.directory.path , "\\" , File.List.directory [[i]] , "\\" , 
                                                                 
                                                                 Summary.directory ), GCPDF.File.name = PDF.Results.Files[j]) ;
    
    #names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
    
    
    PeakArea.results.1$AnalysisName<-PDF.Results.Files[[j]] ;
    
    PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );
    
    
    PeakArea.results.0<-PeakArea.results ;
    
    # Delete objects and files that are not longer needed
    
    rm(PeakArea.results.1)
    
    
    
  }
  
 }


write.csv(x = PeakArea.results, file = "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Analysis\\GCcompiledResults2022.csv", row.names = F )

PeakArea.results <- read.csv(file = "C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\Analysis\\GCcompiledResults2022.csv" , header = T)

###############################################################################################################
#                           
#                               Working with standards
#
###############################################################################################################

str(PeakArea.results)

head(PeakArea.results)
   
# PeakArea.results$CH4<-as.double(PeakArea.results$CH4) ;
# 
# PeakArea.results$CO2<-as.double(PeakArea.results$CO2) ;
# 
# PeakArea.results$N2O<-as.double(PeakArea.results$N2O) ;


# getting the GC samples with standards together

GC.standards <- PeakArea.results[grep( pattern = "B" , x = PeakArea.results$Sample.Name, invert = T) ,] ;
   

GC.Data <-  PeakArea.results[grep( pattern = "B" , x = PeakArea.results$Sample.Name, invert = F) ,] ;



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

levels(GC.standards$Factor.Name)


GC.standards$CH4.ppm <- NaN

GC.standards$CO2.ppm <- NaN

GC.standards$N2O.ppm <- NaN

# Values of the standards 
# CH4	CO2	N2O
# 0 perSTD	ppm 	uL/Lgas	0	0	0
# L25 perSTDA	ppm 	uL/Lgas	1.25	125	0.25
# L50 perSTDA	ppm 	uL/Lgas	2.5	250	0.5
# L75 perSTDA	ppm 	uL/Lgas	3.75	375	0.75
# L100 perSTDA	ppm 	uL/Lgas	5	500	1
# H50 PerSTD	ppm 	uL/Lgas	25	2500	25
# H100 PerSTD	ppm 	uL/Lgas	100	5000	50



head(GC.standards);

GC.standards[GC.standards$Sample.Name == 'L100',c('CH4.ppm')] <- 5 ;
GC.standards[GC.standards$Sample.Name == 'L100',c('CO2.ppm')] <- 500 ;
GC.standards[GC.standards$Sample.Name == 'L100',c('N2O.ppm')] <- 1 ;


GC.standards[GC.standards$Sample.Name == 'H100',c('CH4.ppm')] <- 50 ;
GC.standards[GC.standards$Sample.Name == 'H100',c('CO2.ppm')] <- 5000 ;
GC.standards[GC.standards$Sample.Name == 'H100',c('N2O.ppm')] <- 50 ;    


GC.standards[GC.standards$Sample.Name == 'L50',c('CH4.ppm')]<-2.5  ;
GC.standards[GC.standards$Sample.Name == 'L50',c('CO2.ppm')]<-250  ;
GC.standards[GC.standards$Sample.Name == 'L50',c('N2O.ppm')]<-0.5  ;


GC.standards[GC.standards$Sample.Name == 'H50',c('CH4.ppm')]<-25 ;
GC.standards[GC.standards$Sample.Name == 'H50',c('CO2.ppm')]<-2500 ;
GC.standards[GC.standards$Sample.Name == 'H50',c('N2O.ppm')]<-25;


GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CH4.ppm')]<-3.75 ;
GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CO2.ppm')]<-375  ;
GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('N2O.ppm')]<-0.75 ;


GC.standards[GC.standards$Sample.Name == 'L25',c('CH4.ppm')]<-1.25 ;
GC.standards[GC.standards$Sample.Name == 'L25',c('CO2.ppm')]<-125  ;
GC.standards[GC.standards$Sample.Name == 'L25',c('N2O.ppm')]<-0.25  ;


GC.standards[GC.standards$Sample.Name == '0',c('CH4.ppm', 'CO2.ppm', 'N2O.ppm')]<-1e-5 ;


##### Exploring the standards data

str(GC.standards)

plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "0" ,  ])

plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "L25" ,  ]) 

plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "L50" ,  ])

plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "L100" ,  ])

plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "H50" ,  ])

plot(CO2 ~ CO2.ppm, data = GC.standards[GC.standards$Factor.Name == "H100" ,  ])




xyplot(N2O~N2O.ppm, data=GC.standards, type="p",main="N2O")

xyplot(CH4~CH4.ppm, data=GC.standards, type="b",main="CH4")


##### Exploring the standards data with CO2 areas below 5000, CH4 below100 and N2O below 10000

xyplot(CO2 ~ CO2.ppm, data=GC.standards, type="p",main="CO2")


xyplot(CH4 ~ CH4.ppm, data=GC.standards, type="p",main="CH4")

xyplot(N2O ~ N2O.ppm, data=GC.standards, type="p",main="N2O")


##### Exploring the standards by date of analysis Date Of Analysis

GC.standards$ANAL.DATE<-as.factor(GC.standards$GC.Date)  ;

xyplot(CO2 ~ CO2.ppm, groups = GC.Date, data=GC.standards, type="b", main="CO2", auto.key = T)

xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Position , data=GC.standards, type="p",main="CO2", auto.key = T)

simpleKey(text = GC.standards$Position )


#### Adding an factor identification for each of the different standards in a run

GC.standards$Series <- "None" ;

GC.standards[GC.standards$Position %in% c(1:5), c("Series")] <- 1 ;

GC.standards[GC.standards$Position %in% c(30:34), c("Series")] <- 2 ;

GC.standards[GC.standards$Position %in% c(51:55), c("Series")] <- 3 ;

GC.standards[GC.standards$Position %in% c(80:84), c("Series")] <- 4 ;

GC.standards[GC.standards$Position %in% c(101:105), c("Series")] <- 5 ;


head(GC.standards)

GC.standards$Series <- as.factor(GC.standards$Series) ;

str(GC.standards)


xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Series , data=GC.standards, 
       
       type="b",main="CO2", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3);


xyplot(CH4 ~ CH4.ppm | GC.Date , groups = Series , data=GC.standards, 
       
       type="b",main="CH4", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3);

xyplot(N2O ~ N2O.ppm | GC.Date , groups = Series , data=GC.standards, 
       
       type="b",main="N2O", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3);

#### Adding ablines to the lattice xyplot using the panel.abline parameter in the panel function.

# An example from from https://stackoverflow.com/questions/11949766/how-to-add-abline-with-lattice-xyplot-function
# 
# xyplot(Neff ~ Eeff, data = phuong,
#        panel = function(x, y) {
#          panel.xyplot(x, y)
#          panel.abline(lm(y ~ x))
#        }, 
#        xlab = "Energy efficiency (%)", 
#        ylab = "Nitrogen efficiency (%)")


xyplot(N2O ~ N2O.ppm | GC.Date , groups = Series , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x))
         
         panel.abline(a= 1000, b=0, col="RED")
       },
       
       type="b",main="N2O", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3)
       
       







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

xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Series , data=GC.standards, 
       
       type="b",main="CO2", auto.key = T, col = c("BLACK" , "RED" , "BLUE", "CYAN", "MAGENTA"),  lwd=3); 


xyplot(CO2 ~ CO2.ppm | GC.Date , groups = Series , data=GC.standards, 
       
       panel = function(x, y) { panel.xyplot(x, y)
         
         panel.xyplot(x, y) 
         
         panel.abline(lm(y ~ x), col = "BLACK", lwd = 2)
         
         panel.abline(rq(y ~ x), col="RED" , lwd = 2)
       },
       
       type="b",main="N2O", auto.key = T)




##### Exploring the  OLS and quntile regression coefficients







###############################################################################################################
#                           
#                              Exploring the data without standards
#
###############################################################################################################

##### Data with no standards included
str(GC.All.Data)

GC.Data.NoSTD<-GC.All.Data[!(GC.All.Data$Sample.Name %in% GC.standards$Sample.Name),];

plot.CH4.hist.dat<-hist(GC.Data.NoSTD$CH4.Area)
plot.CH4.density.dat<-density(GC.Data.NoSTD$CH4.Area, na.rm=T)

plot.CO2.hist.dat<-hist(GC.Data.NoSTD$CO2.Area)
plot.CO2.density.dat<-density(GC.Data.NoSTD$CO2.Area, na.rm=T)


plot.N2O.hist.dat<-hist(GC.Data.NoSTD$N2O.Area)
plot.N2O.density.dat<-density(GC.Data.NoSTD$N2O.Area, na.rm=T)

##### compared with the standard data

###CH4

plot.CH4.hist.STD<-hist(GC.standards$CH4.Area)
plot.CH4.density.STD<-density(GC.standards$CH4.Area, na.rm=T)

plot(plot.CH4.hist.STD, col="RED" )
plot(plot.CH4.hist.dat, col="BLUE" ,add=T)

plot(plot.CH4.density.STD, col="RED")
lines(plot.CH4.density.dat, col="BLUE")

###CO2

plot.CO2.hist.STD<-hist(GC.standards$CO2.Area)
plot.CO2.density.STD<-density(GC.standards$CO2.Area, na.rm=T)

plot(plot.CO2.hist.STD, col="RED" )
plot(plot.CO2.hist.dat, col="BLUE" ,add=T)
plot(plot.CO2.hist.STD, col="RED", add=T )

plot(plot.CO2.density.STD, col="RED")
lines(plot.CO2.density.dat, col="BLUE")


###N2O

plot.N2O.hist.STD<-hist(GC.standards$N2O.Area)
plot.N2O.density.STD<-density(GC.standards$N2O.Area, na.rm=T)

plot(plot.N2O.hist.STD, col="RED" )
plot(plot.N2O.hist.dat, col="BLUE" ,add=T)
plot(plot.N2O.hist.STD, col="RED", add=T )

plot(plot.N2O.density.STD, col="RED")
lines(plot.N2O.density.dat, col="BLUE")

## initialize the dataframe to collect all the data together

GC.All.Data.1<-data.frame(Sample.Name = character(),  Vial.number= integer(),  DateOfAnalysis = character(),  CH4.Area = double(), CO2.Area = double(),       N2O.Area = double(), Treatment = character() , BLOCK = integer(), CoverCrop = character(), Sampling.Time = integer(),  Treatment.F = factor(), BLOCK.F = factor(), CoverCrop.F = factor()) ;   



###############################################################################################################
#                           
#                               Organizing the data for visualization
#
###############################################################################################################





### Organizing the data according to Treatments

PeakArea.results$Treatment<-c("NONE");

PeakArea.results[grep("STD",PeakArea.results$Sample.Name), c("Treatment")]<-c("STANDARD");

PeakArea.results[grep("AT",PeakArea.results$Sample.Name), c("Treatment")]<-c("A");

PeakArea.results[grep("BT",PeakArea.results$Sample.Name), c("Treatment")]<-c("B");

PeakArea.results[grep("CT",PeakArea.results$Sample.Name), c("Treatment")]<-c("C");

PeakArea.results[grep("DT",PeakArea.results$Sample.Name), c("Treatment")]<-c("D");

### Check if there was any treatment left with "NONE" label

PeakArea.results[which(PeakArea.results$Treatment=="NONE"), ];

PeakArea.results[51,c("Treatment")]<-c("C");


### Organizing the data according to Blocks

grep("B1",PeakArea.results$Sample.Name)

PeakArea.results$BLOCK<-c(9999);

PeakArea.results[grep("B1",PeakArea.results$Sample.Name), c("BLOCK")]<-c(1);

PeakArea.results[grep("B2",PeakArea.results$Sample.Name), c("BLOCK")]<-c(2);

PeakArea.results[grep("B3",PeakArea.results$Sample.Name), c("BLOCK")]<-c(3);

PeakArea.results[grep("B4",PeakArea.results$Sample.Name), c("BLOCK")]<-c(4);


### Organizing the data according to CoverCrop

grep("3Spp",PeakArea.results$Sample.Name)

PeakArea.results$CoverCrop<-c("NONE");

PeakArea.results[grep("3Spp",PeakArea.results$Sample.Name), c("CoverCrop")]<-c("3Spp");

PeakArea.results[grep("Clover",PeakArea.results$Sample.Name), c("CoverCrop")]<-c("Clover");

PeakArea.results[grep("Trit",PeakArea.results$Sample.Name), c("CoverCrop")]<-c("Trit");


### Organizing the data according to Sampling Time Order

grep("T0",PeakArea.results$Sample.Name)

PeakArea.results$Sampling.Time<-c(9999);

PeakArea.results[grep("T0",PeakArea.results$Sample.Name), c("Sampling.Time")]<-c(0);

PeakArea.results[grep("T15",PeakArea.results$Sample.Name), c("Sampling.Time")]<-c(15);

PeakArea.results[grep("T30",PeakArea.results$Sample.Name), c("Sampling.Time")]<-c(30);

PeakArea.results[grep("T45",PeakArea.results$Sample.Name), c("Sampling.Time")]<-c(45);


### Check if there was any Sampling.Time left with "NONE" label

PeakArea.results[which(PeakArea.results$Sampling.Time==9999),];

PeakArea.results[51,c("Sampling.Time")]<-c(30);


### Converting experimental designations into factors

PeakArea.results$Treatment.F<-as.factor(PeakArea.results$Treatment) ;

PeakArea.results$BLOCK.F<-as.factor(PeakArea.results$BLOCK) ;

PeakArea.results$CoverCrop.F<-as.factor(PeakArea.results$CoverCrop) ;



                  



###############################################################################################################
#                           
#                               Exploratory Data visualization
#
###############################################################################################################

str(PeakArea.results)

xyplot(CH4.Area + N2O.Area + CO2.Area ~ Sampling.Time | Treatment.F * BLOCK.F * CoverCrop.F, data=PeakArea.results[!PeakArea.results$Treatment.F == "STANDARD",],xlim=c(0,45), type="o", auto.key = T);




###############################################################################################################
#                          
#            Calculation of concentration based on the Standard gas concentrations
#  
#
###############################################################################################################


Standards.Plot<-PeakArea.results[PeakArea.results$Treatment.F == "STANDARD",] ;

Standards.Plot$Percent.Standard<-as.numeric(gsub(pattern = "PerSTD_1", x=Standards.Plot$Sample.Name, replacement = ""));
str(Standards.Plot) ; names(Standards.Plot) ;


GC.Standards<-data.frame(NAME=c("25PerSTD_1", "50PerSTD_1", "75PerSTD_1", "100PerSTD_1"), UNITS=c("uL/Lgas"), CH4.Conc=c(1.25, 2.5, 3.75, 5), CO2.Conc=c(125, 25, 375, 500), N2O.Conc=c(0.25, 0.5, 0.75, 1) );

str(GC.Standards) ; names(GC.Standards);


All.Standards<-merge(Standards.Plot, GC.Standards, by.x=c('Sample.Name'), by.y=c('NAME'), all.x = T);

str(All.Standards)

plot(CH4.Conc ~ CH4.Area , data=All.Standards, col='BLUE', pch=19, cex=2) ;

CH4.Conc.Reg<-lm(CH4.Conc ~ CH4.Area, data=All.Standards); summary(CH4.Conc.Reg) ;

predict.lm(CH4.Conc.Reg)

points(All.Standards$CH4.Area, predict.lm(CH4.Conc.Reg), col="RED", type="o")




plot( CO2.Conc ~ CO2.Area, data=All.Standards, col='GREEN', pch=19 , cex=2 ) ;

All.Standards[,c("CO2.Area" , "CO2.Conc", "Percent.Standard")]

### There seems to be a mistake in the CO2 standards. The CO2.Conc 125 (25% STD) and the CO2.Conc 25 (50% STD) seem to be interchanged

points(All.Standards[c(1,2,3,4),c("CO2.Area")], All.Standards[c(1,3,2,4),c("CO2.Conc")], col='BLUE', pch=19 , cex=2 ) ;

CO2.Standards.Corr<-data.frame(CO2.Conc = All.Standards[c(1,3,2,4),c("CO2.Conc")],  CO2.Area= All.Standards[c(1,2,3,4),c("CO2.Area")])

CO2.Conc.Reg<-lm(CO2.Conc ~ CO2.Area, data=CO2.Standards.Corr); summary(CO2.Conc.Reg) ;

points(CO2.Standards.Corr$CO2.Area, predict.lm(CO2.Conc.Reg), col="RED", type="o")


plot(All.Standards[c(1,2,3,4),c("CO2.Area")] ~ predict.lm(CO2.Conc.Reg, All.Standards[c(1,2,3,4),c("CO2.Area")]) , col='RED', pch=19 , cex=2) ;


N2O.Conc.Reg<-lm(N2O.Conc ~ N2O.Area, data=All.Standards); summary(N2O.Conc.Reg) ;

predict.lm(N2O.Conc.Reg)

points(All.Standards$N2O.Area, predict.lm(N2O.Conc.Reg), col="BLUE", type="o")





###############################################################################################################
#                          Calculations of gas emission rates base on GraceNet Protocols
#
#  All the reference data was taking from Allison Kohele's Calculations Excel Files
#
###############################################################################################################





Chamber.Dimensions<-data.frame(DIMENSION=c("Length", "Width" , "Height", "Volume" , "Surface.Area"),UNITS=c("m"), VALUE=c(0.52705, 0.32385, 0.1016, 9999, 9999));

Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Volume", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3]*Chamber.Dimensions[3,3] ;


Chamber.Dimensions[Chamber.Dimensions$DIMENSION =="Surface.Area", c("VALUE")]<-Chamber.Dimensions[1,3]*Chamber.Dimensions[2,3] ;

Molar.Mass<-data.frame(GAS=c("CH4" , "CO2" , "N2O"), UNITS=c("g/mol"), VALUE=c(16.04, 44.01, 44.013));

Gas.Law<-data.frame(UNITS=c("L-atm/Mol-K", "J/K-Mol", "m3-Pa/K-Mol", "Kg-m2-s2/K-Mol", "m3-atm/K-Mol"), VALUE=c(0.08205736, 8.314462,8.314462, 8.314462, 8.205736e-5 ));






