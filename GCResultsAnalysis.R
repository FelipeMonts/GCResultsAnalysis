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

"https://pennstateoffice365.sharepoint.com/:f:/s/StrategicTillageAndN2O/Ehl9Lh_gza5FiOtKIyDD7MQBOKFdFk6h_k4EEYEktWJUYw?e=uYLqL0"

#setwd("C:\\Felipe\\Willow_Project\\Willow_Experiments\\

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################


#install.packages("tabulizer", "tabulizerjars" , dependencies = T)



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)


library(tabulizer)





###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored



Directory.List<-list.files("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\SummaryReport");



###############################################################################################################
#                           
# Directories, subdirectories and files are not always consistent, and some files and directories have no data 
# Therefore A catalog with objects with valid data will be build and that will be the information base to
#extract the data
#
###############################################################################################################

# j=1
# k=1



####initialize the dataframe to collect all the directories, subdirectories and file names

File.Catalog.0<-data.frame( File.Name=character(), Directory=character());

#### Do a loop throughout the directories and collect all the file names


for (j in seq(1, length(Directory.List))){
  
    Files.List<-list.files(paste0("C:\\Users\\frm10\\The Pennsylvania State University\\StrategicTillageAndN2O - Documents\\Data\\GCResults\\SummaryReport\\", Directory.List[j]) ) ;
    
    ## Select files that are ".csv" only
    
   SummaryFile<-Files.List[grep("summaryreport.xlsx",Files.List)] ;
   PeakareaFile<-Files.List[grep("peakareas.xlsx",Files.List)] ;
    
    File.Catalog.1<-data.frame(File.Name=SummaryFile) ;
    File.Catalog.2<-data.frame(File.Name=PeakareaFile) ;
    
    File.Catalog.3<-rbind(File.Catalog.1,File.Catalog.2);
    
    
    File.Catalog.3$Directory<-Directory.List[j]

    File.Catalog<-rbind(File.Catalog.0,File.Catalog.3 ) ;

    File.Catalog.0<-File.Catalog ;

    
}
  

## initialize the dataframe to collect all the data together

GC.All.Data.1<-data.frame(Sample.Name = character(),  Vial.number= integer(),  DateOfAnalysis = character(),  CH4.Area = double(), CO2.Area = double(),       N2O.Area = double(), Treatment = character() , BLOCK = integer(), CoverCrop = character(), Sampling.Time = integer(),  Treatment.F = factor(), BLOCK.F = factor(), CoverCrop.F = factor()) ;   

   
    ###############################################################################################################
    #                           Read all the   excel files in the directory
    ###############################################################################################################
    
    
    ## initialize the dataframe to collect all the data in the directory files
    
    PeakArea.results.0<-data.frame(Sample.Name = character(), Vial.number = integer(), CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), DateOfAnalysis = character(), AnalysisName = character() );
    
    
    
    for (i in (seq(1, dim.data.frame(File.Catalog)[1]))) {
  #i=24    
      
      PeakArea.results.1<-read.xlsx(paste0("C:\\Users\\frm10\\Downloads\\SummaryReport\\", File.Catalog[i,c("Directory")], "\\", File.Catalog[i,c("File.Name")] ), sheet=1, startRow=3, colNames=F, cols= c(1,2,4:6) );
      
      names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
      
      
      PeakArea.results.1$DateOfAnalysis<-File.Catalog[i,c("Directory")] ;
      
      PeakArea.results.1$AnalysisName<-File.Catalog[i,c("File.Name")] ;
      
      PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );
      
      
      PeakArea.results.0<-PeakArea.results ;
      
      
    }

    



###############################################################################################################
#                           
#                               Organizing the data for visualization
#
###############################################################################################################

str(PeakArea.results)


### Organizing the data according to Treatments

grep("STD",PeakArea.results$Sample.Name)

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






