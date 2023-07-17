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




###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() Willow Rock Spring\\SkyCap_SelectionTrial\\DataCollection") ;   # 


setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")

###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("openxlsx",  dependencies = T, lib = "C:/Users/frm10/AppData/Local/Temp/RtmpqMRxqa/downloaded_packages")

# install.packages("Rtools",  dependencies = T, lib = "C:/Users/frm10/AppData/Local/Temp/RtmpqMRxqa/downloaded_packages")

# install.packages("pdftools",  dependencies = T, lib = "C:/Users/frm10/AppData/Local/Temp/RtmpqMRxqa/downloaded_packages")

# install.packages("askpass",  dependencies = T, lib = "C:/Users/frm10/AppData/Local/Temp/RtmpqMRxqa/downloaded_packages")

# install.packages("cli",  dependencies = T, lib = "C:/Users/frm10/AppData/Local/Temp/RtmpqMRxqa/downloaded_packages")

# install.packages("utf8",  dependencies = T, lib = "C:/Users/frm10/AppData/Local/Temp/RtmpqMRxqa/downloaded_packages")



###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)





###############################################################################################################
#                           Explore the files and directory and files with the data
###############################################################################################################
### Read the Directories where the GC data are stored

File.List.directory<-"C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results" ;

File.List<-list.files("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\GCResults\\Alli_Felipe2021\\Results"); length(File.List) ; 

# Only select the pdf files

PDF.Results.Files<-File.List[grep(".pdf", File.List)] ;

Excel.Results.Files<-File.List[grep(".xlsx", File.List)] ;


# ###############################################################################################################
# #                           Read all the  excel files in the File.List
# ###############################################################################################################
# 
# 
# 
# ## initialize the dataframe to collect all the data in the directory files in the Excel.Results.Files
# 
# 
# 
# PeakArea.results.0<-data.frame(Sample.Name = character(), Vial.number = integer(), CH4.Area = double(), CO2.Area = double(), N2O.Area = double(), DateOfAnalysis = character(), AnalysisName = character() );
# 
# #i=1 
# 
# for (i in seq(1,length(Excel.Results.Files))) {
#   
#   
#   PeakArea.results.1<-read.xlsx(xlsxFile = paste0(".\\Alli_Felipe2021\\Results\\",Excel.Results.Files[i]), sheet= 1, startRow=3, colNames=F, cols= c(1,2,4:6) );
#   
#   names(PeakArea.results.1)<-c('Sample.Name' , 'Vial.number' , 'CH4.Area' , 'CO2.Area', 'N2O.Area' );
#   
#   
#   PeakArea.results.1$AnalysisName<-Excel.Results.Files[[i]] ;
#   
#   PeakArea.results<-rbind(PeakArea.results.0,PeakArea.results.1 );
#   
#   
#   PeakArea.results.0<-PeakArea.results ;
#   
#   rm(PeakArea.results.1)
#   
# }
# 
# # Delete objects and files that are not longer needed
# 
# rm(PeakArea.results.1, i)
# 
# 


###############################################################################################################
#                           Read all the  pdf files in the File.List
###############################################################################################################

# read pdf files using the pdftools package

# get useful info from the pdf file

Report.pdf.info<-pdf_info(pdf=paste0(File.List.directory,"\\",PDF.Results.Files[1]))  ;

print(Report.pdf.info)

# number of pages in the report

Report.pdf.info$pages


# read the pdf file as a text


Report.PDF.1<-pdf_text(pdf=paste0(File.List.directory,"\\",PDF.Results.Files[1])) ;
str(Report.PDF.1)

# separate each page and each line using strsplit() and "\n" as he separation pattern

Report.PDF.2<-strsplit(x=Report.PDF.1,split="\n") ;

str(Report.PDF.2)


# get the data from the list starting with page 1 [[1]]

Report.PDF.2[[1]] ;   str(Report.PDF.2[[1]]) ;

# get all the pages in one long character vector Report.PDF.3 by unlisting the list produced by Report.PDF.2

Report.PDF.3<-unlist(Report.PDF.2);

# remove all the blank components of the vector



### in reg exp ^ means start of a string, and $ means end of a string. Subsequently, ^$ means empty or ""

grep(pattern="^$",x=Report.PDF.3)

grep(pattern="^$",x=Report.PDF.3, value=T)

grep(pattern="^$",x=Report.PDF.3, value=T, invert=T)

Report.PDF.4<-grep(pattern="^$",x=Report.PDF.3, value=T, invert=T) ;


## Remove lines of the report that do not contain data

print(Report.PDF.4[1:4])

# [1] "Sequence Summary Report"                                                   
# [2] "                                   CH4            CO2            N2O"      
# [3] "                                   Peak Area      Peak Area      Peak Area"
# [4] " 0PerSTD                 1    1    4.071          781.033        149.931"  
# 



Report.PDF.5<-grep(pattern="Koehle*|Sequence|CH4|Peak", x=Report.PDF.4, value=T, invert=T)



# use  str_squish from the package stringr to get rid of the spaces between characters

str_squish(Report.PDF.5[1])


strsplit(str_squish(Report.PDF.5[1]), " ")[1]


str(unlist(strsplit(str_squish(Report.PDF.5[1]), " ")[1]))


# or even better  use strsplit to remove the the spaces between characters in character vectors

strsplit(x=Report.PDF.5[1], split=c(" ", ""))

strsplit(x=Report.PDF.5[5], split=c(" ", ""))

str(strsplit(x=Report.PDF.5[80], split=c(" ", "")))

# even better

strsplit(x=Report.PDF.5, split=c(" "))

Report.PDF.6<-strsplit(x=Report.PDF.5, split=c(" ")) ;

str(Report.PDF.6) ; head(Report.PDF.6,3 )


### in regular  expressions "^" means start of a string, and $ means end of a string. Subsequently, ^$ means empty or ""

grep(pattern="^$",x=Report.PDF.6[[1]])

grep(pattern="^$",x=Report.PDF.6[[1]], value=T)

grep(pattern="^$",x=Report.PDF.6[[1]], value=T, invert=T)

### using sapply to apply the grep function to all the elements in the Report.PDF.6 list

lapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T))

lapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T))[[1]]

sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T))

str(sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T)))

t(sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T)))

Report.PDF.7<-data.frame(t(sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T)))) ;

# Names of the pdf report columns c("Sample", "Position", "Vial", "CH4" , "CO2" , "N2O" )

names(Report.PDF.7)<-c("Sample.Name", "Position" , "Vial", "CH4" , "CO2" , "N2O" );

head(Report.PDF.7)

######################### Add the sampling date and the processing date ####################

##CHANGE PDF.Results.Files[1] TO PDF.Results.Files[i]


Report.PDF.7$File<-PDF.Results.Files[1] ;

head(Report.PDF.7)

### Get the sampling date from the file name

head(Report.PDF.7$File)

strsplit(x=Report.PDF.7$File, split=c("pea")) [[1]]

XXXX<-sapply(strsplit(x=Report.PDF.7$File, split=c("pea")), FUN="[[",1)

regmatches(XXXX, regexpr("[[:digit:]]+",XXXX))


Report.PDF.7$Sampling.Day<-regmatches(XXXX, regexpr("[[:digit:]]+",XXXX)) ;

Report.PDF.7$Sampling.Date<-as.Date(Report.PDF.7$Sampling.Day, format="%Y%M%d")

###### Get the Gas Chromatograph processing date 

grep(pattern="Koehle*", x=Report.PDF.4, value=T)[[1]]

Report.PDF.DateLine<-strsplit(x=grep(pattern="Koehle*", x=Report.PDF.4, value=T)[[1]], split=c(" ", ""))[[1]] ;

as.Date(Report.PDF.DateLine, format="%Y-%m-%d" )

is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))

!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))

Report.PDF.DateLine[!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))]

as.Date(Report.PDF.DateLine[!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))])

Report.PDF.7$GC.Date<-as.Date(Report.PDF.DateLine[!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))]);

head(Report.PDF.7)


###############################################################################################################
#                           
#                               Working with standards
#
###############################################################################################################

str(PeakArea.results)
   
PeakArea.results$CH4.Area<-as.numeric(PeakArea.results$CH4.Area) ;

PeakArea.results$CO2.Area<-as.numeric(PeakArea.results$CO2.Area) ;

PeakArea.results$N2O.Area<-as.numeric(PeakArea.results$N2O.Area) ;


# getting the GC samples with standards together
   

GC.standards<-PeakArea.results[grep("ST",PeakArea.results$Sample.Name),] ;

###############################################################################################################
#                           
#                            Data dispersion and variability 
#
###############################################################################################################


### group the data by teh different standards

GC.standards.STDA<-GC.standards[grep("STDA",GC.standards$Sample.Name),] ;

GC.standards.STDA.100<-GC.standards.STDA[grep("100",GC.standards.STDA$Sample.Name),] ;

boxplot(GC.standards.STDA.100$CH4.Area ~ GC.standards.STDA.100$AnalysisName, las= 2) ;



abline(h=mean(GC.standards.STDA.100$CH4.Area), col= "RED");

densityplot(GC.standards.STDA.100$CH4.Area)   ;
hist(GC.standards.STDA.100$CH4.Area, plot = T, freq = T, breaks = 20) ;

bwplot(CH4.Area ~ AnalysisName, data = GC.standards.STDA.100, scales=list(y=list(rot=0), x=list(rot=45)))



bwplot(CH4.Area~Sample.Name, data = GC.standards)





















GC.standards$CH4.ppm<-NA
  
GC.standards$CO2.ppm<-NA

GC.standards$N2O.ppm<-NA

#Values of the standards 
#CH4	CO2	N2O
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






