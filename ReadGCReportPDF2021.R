##############################################################################################################
# 
# Function to read the results of the gas chromatograph from the PDF reports saved after analyzing the results. 
# 
#    Felipe Montes 2022/10/13 
#   
#    Rev 2023/07/17
# 
# 
# 
# 
# 
############################################################################################################### 



###############################################################################################################
#                             Tell the program where the package libraries are stored                        
###############################################################################################################
# 
# 
# 
# #  Tell the program where the package libraries are  #####################
# 
# .libPaths("C:\\Users\\frm10\\AppData\\Local\\R\\win-library\\4.2")  ;


###############################################################################################################
#                            Install the packages that are needed                       
###############################################################################################################

# install.packages("openxlsx",  dependencies = T)

# install.packages("Rtools",  dependencies = T)

# install.packages("pdftools",  dependencies = T)

# install.packages("askpass",  dependencies = T)

# install.packages("cli",  dependencies = T)

# install.packages("utf8",  dependencies = T)


###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(pdftools)

library(stringr)



###############################################################################################################
#                             Setting up working directory                         
###############################################################################################################


#      set the working directory

# readClipboard() ;    


# setwd("D:\\Felipe\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\Data\\GasChromatograph")



###############################################################################################################
#                           Start Code
###############################################################################################################


###############################################################################################################
# 
# Inputs required by the function
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


ReadGCReportPDF<-function(GCPDF.File.path, GCPDF.File.name ){
  
 
  
  # read the pdf file as a text
  
  
  # Report.PDF.1<-pdf_text(pdf=paste0(File.List.directory,"\\",PDF.Results.Files[1])) ;
  
  Report.PDF.1<-pdf_text(pdf=paste0(GCPDF.File.path,"\\",GCPDF.File.name)) ;
  
  # str(Report.PDF.1)
  
  # separate each page and each line using strsplit() and "\n" as the separation pattern
  
  Report.PDF.2<-strsplit(x=Report.PDF.1,split="\n") ;
  
  # str(Report.PDF.2)
  
  
  # get the data from the list starting with page 1 [[1]]
  
  Report.PDF.2[[1]] ;   str(Report.PDF.2[[1]]) ;
  
  # get all the pages in one long character vector Report.PDF.3 by unlisting the list produced by Report.PDF.2
  
  Report.PDF.3<-unlist(Report.PDF.2);
  
  # remove all the blank components of the vector
  
  # remove all the blank components of the vector
  
  ### in reg exp ^ means start of a string, and $ means end of a string. Subsequently, ^$ means empty or ""
  
  # grep(pattern="^$",x=Report.PDF.3)
  # 
  # grep(pattern="^$",x=Report.PDF.3, value=T)
  # 
  # grep(pattern="^$",x=Report.PDF.3, value=T, invert=T)
  
  Report.PDF.4<-grep(pattern="^$",x=Report.PDF.3, value=T, invert=T) ;
  
  
  ## Remove lines of the report that do not contain data
  
  print(Report.PDF.4[1:4])
  
  # [1] "Sequence Summary Report"                                                   
  # [2] "                                   CH4            CO2            N2O"      
  # [3] "                                   Peak Area      Peak Area      Peak Area"
  # [4] " 0PerSTD                 1    1    4.071          781.033        149.931"  
  # 
  
  
  
  Report.PDF.5<-grep(pattern="Koehle*|Sequence|CH4|Peak", x=Report.PDF.4, value=T, invert=T)
  
  
  
  # # use  str_squish from the package stringr to get rid of the spaces between characters
  # 
  # str_squish(Report.PDF.5[1])
  # 
  # 
  # strsplit(str_squish(Report.PDF.5[1]), " ")[1]
  # 
  # 
  # str(unlist(strsplit(str_squish(Report.PDF.5[1]), " ")[1]))
  
  
  # # or even better  use strsplit to remove the the spaces between characters in character vectors
  # 
  # strsplit(x=Report.PDF.5[1], split=c(" ", ""))
  # 
  # strsplit(x=Report.PDF.5[5], split=c(" ", ""))
  # 
  # str(strsplit(x=Report.PDF.5[80], split=c(" ", "")))
  
  # even better
  
  # strsplit(x=Report.PDF.5, split=c(" "))
  
  Report.PDF.6<-strsplit(x=Report.PDF.5, split=c(" ")) ;
  
  # str(Report.PDF.6) ; head(Report.PDF.6,3 )
  
  
  ### in regular  expressions "^" means start of a string, and $ means end of a string. Subsequently, ^$ means empty or ""
  
  # grep(pattern="^$",x=Report.PDF.6[[1]])
  # 
  # grep(pattern="^$",x=Report.PDF.6[[1]], value=T)
  # 
  # grep(pattern="^$",x=Report.PDF.6[[1]], value=T, invert=T)
  # 
  # ### using sapply to apply the grep function to all the elements in the Report.PDF.6 list
  # 
  # lapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T))
  # 
  # lapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T))[[1]]
  # 
  # sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T))
  # 
  # str(sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T)))
  # 
  # t(sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T)))
  
  Report.PDF.7<-data.frame(t(sapply(Report.PDF.6, function(x) grep(pattern="^$",x, value=T, invert=T)))) ;
  
  # Names of the pdf report columns c("Sample", "Position", "Vial", "CH4" , "CO2" , "N2O" )
  
  names(Report.PDF.7)<-c("Sample.Name", "Position" , "Vial", "CH4" , "CO2" , "N2O" );
  
  # head(Report.PDF.7)
  
  ######################### Add the sampling date and the processing date ####################
  
  Report.PDF.7$File<-GCPDF.File.name ;
  
  # head(Report.PDF.7)
  
  ### Get the sampling date from the file name
  
  # head(Report.PDF.7$File)
  
  # strsplit(x=Report.PDF.7$File, split=c("pea")) [[1]]
  
  #  Testing and building this complex call based on sapply, strsplit and regmatches
  
  # XXXX<-sapply(strsplit(x=Report.PDF.7$File, split=c("pea")), FUN="[[",1)
  # 
  # regmatches(XXXX, regexpr("[[:digit:]]+",XXXX))
  
  # regmatches(sapply(strsplit(x=Report.PDF.7$File, split=c("pea")), FUN="[[",1),
  #            regexpr("[[:digit:]]+",sapply(strsplit(x=Report.PDF.7$File, split=c("pea")), FUN="[[",1)));
  # 
  
  Report.PDF.7$Sampling.Day<-regmatches(sapply(strsplit(x=Report.PDF.7$File, split=c("pea")), FUN="[[",1),
                                        regexpr("[[:digit:]]+",sapply(strsplit(x=Report.PDF.7$File, split=c("pea")), FUN="[[",1)));
  
  
  
  Report.PDF.7$Sampling.Date<-as.Date(Report.PDF.7$Sampling.Day, format="%Y%M%d");
  
  
  
  ###### Get the Gas Chromatograph processing date 
  
  # grep(pattern="Koehle*", x=Report.PDF.4, value=T)[[1]]
  # 
  # Report.PDF.DateLine<-strsplit(x=grep(pattern="Koehle*", x=Report.PDF.4, value=T)[[1]], split=c(" ", ""))[[1]] ;
  # 
  # as.Date(Report.PDF.DateLine, format="%Y-%m-%d" )
  # 
  # is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))
  # 
  # !is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))
  # 
  # Report.PDF.DateLine[!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))]
  # 
  # as.Date(Report.PDF.DateLine[!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))])
  
  Report.PDF.DateLine<-strsplit(x=grep(pattern="Koehle*", x=Report.PDF.4, value=T)[[1]], split=c(" ", ""))[[1]] ;
  
  Report.PDF.7$GC.Date<-as.Date(Report.PDF.DateLine[!is.na(as.Date(Report.PDF.DateLine, format="%Y-%m-%d" ))]);
  
  # remove all the intermediary objects that were used during the execution of the program
  
  # ls() objects() ls()[!ls() %in% c("Report.PDF.7", "GCPDF.File.name" ,"GCPDF.File.path")]
  
  #  rm(list=ls()[!ls() %in% c("Report.PDF.7", "GCPDF.File.name" ,"GCPDF.File.path")])
  
  print(head(Report.PDF.7))
  
  return(Report.PDF.7)
  
  
}


