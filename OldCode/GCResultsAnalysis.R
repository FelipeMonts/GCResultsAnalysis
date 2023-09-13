##############################################################################################################
# 
# 
# Program to Analyze and plot GC data collected from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph  
# 
# 
#  Felipe Montes 2021/07/01
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################


#  Tell the program where the package libraries are  #####################

.libPaths("C:/Felipe/SotwareANDCoding/R_Library/library")  ;


###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() Willow Rock Spring\\SkyCap_SelectionTrial\\DataCollection") ;   # 


setwd("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\SummaryReport")

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


#install.packages("tabulizer",  dependencies = T, lib="C:/Felipe/SotwareANDCoding/R_Library/library")



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)


###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored



Directory.List<-list.files(); length(Directory.List) 



###############################################################################################################
#                           
# Directories, subdirectories and files are not always consistent, and some files and directories have no data 
# Therefore A catalog with objects with valid data will be build and that will be the information base to
#extract the data
#
###############################################################################################################

# j=1
# k=1



#### Looking at the structure of the directories and the files, some dates have the peak areas in files named sumaryreport ("20210528" "20210601" "20210604" "20210614" ); The rest have peak the peak areas in files named peak areas ("20210616" "20210621" "20210623" "20210629" "20210702" "20210707" "20210715" "20210720" "20210730" "20210805" "20210812" "20210819" "20210902" "20210917" "20210929").

# Let's get the data from the files that have the peak areas in sumaryreport

#initialize the dataframe to collect all the directories, subdirectories and file names

File.Catalog.sumaryreport<-data.frame( File.Name=character(), Directory=character());

#### Do a loop throughout the directories and collect all the file names
# i=1


for (i in seq(1, 4)){
  
  Files.List<-list.files(paste0(".\\", Directory.List[i]) ) ;
  
  SummaryReportFiles<-Files.List[grep("[Ss]ummary[Rr]eport.xlsx",Files.List)] ;
  
  
  File.Catalog.1<-data.frame(File.Name=SummaryReportFiles) ;
 
  File.Catalog.1$Directory<-Directory.List[i]
  
  File.Catalog<-rbind(File.Catalog.sumaryreport,File.Catalog.1 ) ;
  
  File.Catalog.sumaryreport<-File.Catalog ;
  
  
  
}

rm(File.Catalog.1, File.Catalog,Files.List,SummaryReportFiles,i)

# Let's get the data from the files that have the peak areas in peakareas


#initialize the dataframe to collect all the directories, subdirectories and file names

File.Catalog.peakareas<-data.frame( File.Name=character(), Directory=character());

#### Do a loop throughout the directories and collect all the file names
# j=19


for (j in seq(5, length(Directory.List))){
  
  Files.List<-list.files(paste0(".\\", Directory.List[j]) ) ;
  
  PeakareaFiles<-Files.List[grep("peakareas.xls|peakareas.\\.xlsx",Files.List)] ;
  
  File.Catalog.1<-data.frame(File.Name=PeakareaFiles) ;
 
  File.Catalog.1$Directory<-Directory.List[j]
  
  File.Catalog<-rbind(File.Catalog.peakareas,File.Catalog.1 ) ;
  
  File.Catalog.peakareas<-File.Catalog ;
  
  
}

rm(File.Catalog.1, File.Catalog,Files.List, PeakareaFiles,j)


###############################################################################################################
#                           Read all the  excel files in the file catalog
###############################################################################################################



## initialize the dataframe to collect all the data in the directory files in the File.Catalog.sumaryreport
    

    
PeakArea.results.0<-data.frame(Sample.Name = character(), Vial.number = integer(), CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), DateOfAnalysis = character(), AnalysisName = character() );

#i=7 
   
for (i in seq(1,dim(File.Catalog.sumaryreport)[1])) {
  
                       
      PeakArea.results.1<-read.xlsx(paste0(".\\", File.Catalog.sumaryreport[i,c("Directory")], "\\", File.Catalog.sumaryreport[i,c("File.Name")] ), sheet=1, startRow=3, colNames=F, cols= c(1,2,4:6) );
      
      names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
      
      
      PeakArea.results.1$DateOfAnalysis<-File.Catalog.sumaryreport[i,c("Directory")] ;
      
      PeakArea.results.1$AnalysisName<-File.Catalog.sumaryreport[i,c("File.Name")] ;
      
      PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );
      
      
      PeakArea.results.0<-PeakArea.results ;
      
      
    }

# Delete objects and files that are nbot longer needed

   rm(PeakArea.results.1, i)
###############################################################################################################
   


##  !!!!!  on 20210812 there is some GC data called ~$20210812B1B2peakareas.xlsx , ~$20210812B3B4peakareas.xlsx , and ~$20210812B3B4peakareas2.xlsx   that has not been read !!!!!

#these files appear to be corrupt, therefore lets get them out of the File.Catalog.peakareas
  
  BadFiles.Catalog.peakareas<-File.Catalog.peakareas[grep("~",File.Catalog.peakareas$File.Name),] ;
   
   File.Catalog.peakareas.Rev<-File.Catalog.peakareas[!File.Catalog.peakareas$File.Name %in% BadFiles.Catalog.peakareas$File.Name, ] ; 
   
## initialize the dataframe to collect all the data in the directory files in the File.Catalog.peakareas
   
   
PeakArea.results.2<-data.frame(Sample.Name = character(), Vial.number = integer(), CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), DateOfAnalysis = character(), AnalysisName = character() );



for (i in seq(1,dim(File.Catalog.peakareas.Rev)[1])) {
  
  
  PeakArea.results.1<-read.xlsx(paste0(".\\", File.Catalog.peakareas.Rev[i,c("Directory")], "\\", File.Catalog.peakareas.Rev[i,c("File.Name")] ), sheet=1, startRow=3, colNames=F, cols= c(1,2,4:6) );
  
  names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
  
  
  PeakArea.results.1$DateOfAnalysis<-File.Catalog.peakareas[i,c("Directory")] ;
  
  PeakArea.results.1$AnalysisName<-File.Catalog.peakareas[i,c("File.Name")] ;
  
  PeakArea.results<-rbind(PeakArea.results.2,PeakArea.results.1 );
  
  
  PeakArea.results.2<-PeakArea.results ;
  
  
}

   # Delete objects and files that are nbot longer needed
   
   rm(PeakArea.results.1, i)

# !!!!!  on 20210702 there is some GC data called 20210702B3B4peakareasFIDISSUES that has not been read !!!!!

# !!!!!  on 20210812 there is some GC data called ~$20210812B1B2peakareas.xlsx , ~$20210812B3B4peakareas.xlsx , and ~$20210812B3B4peakareas2.xlsx   that has not been read !!!!!

################################################################################################################     #                      Put all the data together
###############################################################################################################


GC.All.Data<-rbind(PeakArea.results.0,PeakArea.results.2)  ; 
   

   

###############################################################################################################
#                           
#                               Working with standards
#
###############################################################################################################

str(GC.All.Data)
   
GC.All.Data$CH4.Area<-as.numeric(GC.All.Data$CH4.Area) ;

GC.All.Data$CO2.Area<-as.numeric(GC.All.Data$CO2.Area) ;

GC.All.Data$N2O.Area<-as.numeric(GC.All.Data$N2O.Area) ;


# getting the GC samples with standards together
   

GC.standards<-GC.All.Data[grep("STD",GC.All.Data$Sample.Name),] ;

GC.standards$CH4.ppm<-NA
  
GC.standards$CO2.ppm<-NA

GC.standards$N2O.ppm<-NA

# Values of the standards 
# CH4	CO2	N2O
# 0 perSTD	ppm 	uL/Lgas	0	0	0
# 25 perSTDA	ppm 	uL/Lgas	1.25	125	0.25
# 50 perSTDA	ppm 	uL/Lgas	2.5	250	0.5
# 75 perSTDA	ppm 	uL/Lgas	3.75	375	0.75
# 100 perSTDA	ppm 	uL/Lgas	5	500	1
# 50 PerSTD	ppm 	uL/Lgas	25	2500	25
# 100 PerSTD	ppm 	uL/Lgas	100	5000	50



head(GC.standards);

GC.standards[GC.standards$Sample.Name == '100PerSTDA',c('CH4.ppm')]<-5 ;
GC.standards[GC.standards$Sample.Name == '100PerSTDA',c('CO2.ppm')]<-500 ;
GC.standards[GC.standards$Sample.Name == '100PerSTDA',c('N2O.ppm')]<-1 ;


GC.standards[GC.standards$Sample.Name == '100PerSTD',c('CH4.ppm')]<-50 ;
GC.standards[GC.standards$Sample.Name == '100PerSTD',c('CO2.ppm')]<-5000 ;
GC.standards[GC.standards$Sample.Name == '100PerSTD',c('N2O.ppm')]<-50 ;    
             

GC.standards[GC.standards$Sample.Name == '50PerSTDA',c('CH4.ppm')]<-2.5  ;
GC.standards[GC.standards$Sample.Name == '50PerSTDA',c('CO2.ppm')]<-250  ;
GC.standards[GC.standards$Sample.Name == '50PerSTDA',c('N2O.ppm')]<-0.5  ;


GC.standards[GC.standards$Sample.Name == '50PerSTD',c('CH4.ppm')]<-25 ;
GC.standards[GC.standards$Sample.Name == '50PerSTD',c('CO2.ppm')]<-2500 ;
GC.standards[GC.standards$Sample.Name == '50PerSTD',c('N2O.ppm')]<-25;


GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CH4.ppm')]<-3.75 ;
GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('CO2.ppm')]<-375  ;
GC.standards[GC.standards$Sample.Name == '75PerSTDA',c('N2O.ppm')]<-0.75 ;


GC.standards[GC.standards$Sample.Name == '25PerSTDA',c('CH4.ppm')]<-1.25 ;
GC.standards[GC.standards$Sample.Name == '25PerSTDA',c('CO2.ppm')]<-125  ;
GC.standards[GC.standards$Sample.Name == '25PerSTDA',c('N2O.ppm')]<-0.25  ;


GC.standards[GC.standards$Sample.Name == '0PerSTDA',c('CH4.ppm', 'CO2.ppm', 'N2O.ppm')]<-1e-5 ;

GC.standards[GC.standards$Sample.Name == '0PerSTD',c('CH4.ppm', 'CO2.ppm', 'N2O.ppm')]<-1e-5  ;

# There are some that are different 

GC.standards[is.na(GC.standards$CO2.ppm),c("Sample.Name")] ;


GC.standards[GC.standards$Sample.Name == '25PerSTD',c('CH4.ppm')]<-1.25 ;
GC.standards[GC.standards$Sample.Name == '25PerSTD',c('CO2.ppm')]<-125  ;
GC.standards[GC.standards$Sample.Name == '25PerSTD',c('N2O.ppm')]<-0.25  ;

GC.standards[GC.standards$Sample.Name == '25perSTD',c('CH4.ppm')]<-1.25 ;
GC.standards[GC.standards$Sample.Name == '25perSTD',c('CO2.ppm')]<-125  ;
GC.standards[GC.standards$Sample.Name == '25perSTD',c('N2O.ppm')]<-0.25  ;

  
GC.standards[GC.standards$Sample.Name == '75PerSTD',c('CH4.ppm')]<-3.75 ;
GC.standards[GC.standards$Sample.Name == '75PerSTD',c('CO2.ppm')]<-375 ;
GC.standards[GC.standards$Sample.Name == '75PerSTD',c('N2O.ppm')]<-0.75 ;

GC.standards[GC.standards$Sample.Name == '75perSTD',c('CH4.ppm')]<-3.75 ;
GC.standards[GC.standards$Sample.Name == '75perSTD',c('CO2.ppm')]<-375 ;
GC.standards[GC.standards$Sample.Name == '75perSTD',c('N2O.ppm')]<-0.75 ;

GC.standards[GC.standards$Sample.Name == '50perSTD',c('CH4.ppm')]<-25 ;
GC.standards[GC.standards$Sample.Name == '50perSTD',c('CO2.ppm')]<-2500 ;
GC.standards[GC.standards$Sample.Name == '50perSTD',c('N2O.ppm')]<-25;

GC.standards[GC.standards$Sample.Name == '100perSTD',c('CH4.ppm')]<-5 ;
GC.standards[GC.standards$Sample.Name == '100perSTD',c('CO2.ppm')]<-500 ;
GC.standards[GC.standards$Sample.Name == '100perSTD',c('N2O.ppm')]<-1 ;


##### Exploring the standards data

str(GC.standards)

xyplot(CO2.Area~CO2.ppm, data=GC.standards, type="b",main="CO2")

xyplot(N2O.Area~N2O.ppm, data=GC.standards, type="b",main="N2O")

xyplot(CH4.Area~CH4.ppm, data=GC.standards, type="b",main="CH4")

##### Exploring the standards data with CO2 areas below 5000, CH4 below100 and N2O below 10000

xyplot(CO2.Area~CO2.ppm, data=GC.standards[GC.standards$CO2.Area <= 5000,], type="p",main="CO2")

xyplot(CH4.Area~CH4.ppm, data=GC.standards[GC.standards$CH4.Area <= 100,], type="p",main="CH4")

xyplot(N2O.Area~N2O.ppm, data=GC.standards[GC.standards$CH4.Area <= 100,], type="p",main="N2O")


##### Exploring the standards by date of analysis DateOfAnalysis

GC.standards$ANAL.DATE<-as.factor(GC.standards$DateOfAnalysis)  ;

xyplot(CO2.Area~CO2.ppm, groups=ANAL.DATE, data=GC.standards, type="b",main="CO2", auto.key = T)
xyplot(CO2.Area~CO2.ppm, groups=ANAL.DATE, data=GC.standards, type="b",main="CO2", auto.key = T, xlim = c(0,1000), ylim= c(0, 5000))
xyplot(CO2.Area~CO2.ppm |ANAL.DATE , data=GC.standards, type="b",main="CO2")



xyplot(N2O.Area~N2O.ppm, groups=ANAL.DATE, data=GC.standards, type="b",main="N2O", auto.key = T)
xyplot(N2O.Area~N2O.ppm, groups=ANAL.DATE, data=GC.standards, type="b",main="N2O", auto.key = T,xlim = c(0,10), ylim= c(0, 2000))
xyplot(N2O.Area~N2O.ppm | ANAL.DATE, data=GC.standards, type="b",main="N2O", auto.key = T)

xyplot(CH4.Area~CH4.ppm, groups=ANAL.DATE, data=GC.standards, type="b",main="CH4", auto.key = T)
xyplot(CH4.Area~CH4.ppm, groups=ANAL.DATE, data=GC.standards, type="b",main="CH4", auto.key = T,xlim = c(0,10), ylim= c(0, 50))
xyplot(CH4.Area~CH4.ppm | ANAL.DATE, data=GC.standards, type="b",main="CH4", auto.key = T)



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






