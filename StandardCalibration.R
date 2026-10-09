##############################################################################################################
# 
# 
# Program to form a calibration curve from all the GC standards collected from tha analysis of the 2021 and 2022
# 
# sampling seasons and analyzed using from Professor Lauren McPhillips Agilent 8890 Gas Chromatograph
# 
#    
#  Felipe Montes 2026/18/26
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

# install.packages("MASS", dependencies = T) 

###############################################################################################################
#                           load the libraries that are needed   
###############################################################################################################

library(openxlsx)

library(lattice)

library(quantreg)

library(paletteer)

library(MASS) 

###############################################################################################################
#                             Setting up working directory  Loading Packages and Setting up working directory                        
###############################################################################################################


#      set the working directory

# readClipboard() 

Working.Directory.File.Path <- file.path("C:\\Users\\frm10\\OneDrive - The Pennsylvania State University\\",

"Current_Projects\\CCC Based Experiments\\StrategicTillage_NitrogenLosses_OrganicCoverCrops\\DataAnalysis\\",

"RCode\\GCResultsAnalysis\\FluxDataAnalysisResults");

setwd(Working.Directory.File.Path)

###############################################################################################################
#               Read curated GC Standards data from 2021 and 20222                        
###############################################################################################################


C.GC.Standards.2021 <- read.csv(file = "StandardsDataset2021.csv" , header = T);

str(C.GC.Standards.2021)

head(C.GC.Standards.2021, 20)

tail(C.GC.Standards.2021, 20)


C.GC.Standards.2022 <- read.csv(file = "StandardsDataset2022.csv" , header = T);

str(C.GC.Standards.2022)

head(C.GC.Standards.2022, 20)

tail(C.GC.Standards.2022, 20)

names(C.GC.Standards.2021)

names(C.GC.Standards.2022)

######### C.GC.Standards.2022 have two column names that do not match C.GC.Standards.2021 #####

######### "X" and "Factor.Name" ########

names(C.GC.Standards.2022)

str(C.GC.Standards.2022$Factor.Name)

######## Change column name from "X" to "Rownames"#######

colnames(C.GC.Standards.2022) [colnames(C.GC.Standards.2022) == "X" ] <- "Rownames" ;

######## Remove the "Factor.Name" column #########

C.GC.Standards.2022 <- subset(C.GC.Standards.2022, select = - Factor.Name)

colnames(C.GC.Standards.2022)

###### Add a factor name that identifies the year of sampling ########

C.GC.Standards.2021$YEAR <- 2021 ;

C.GC.Standards.2022$YEAR <- 2022 ;




###############################################################################################################
#                           
#        Merge the two standards data set to get a universal calibration line
#
###############################################################################################################


C.GC.Standards.All <- rbind(C.GC.Standards.2021,C.GC.Standards.2022 ) ;

str(C.GC.Standards.All)

C.GC.Standards.All$YEAR <- as.factor(C.GC.Standards.All$YEAR) ;

###### Plot Standards  #####

xyplot(CO2 ~ CO2.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
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
       
       data = C.GC.Standards.All, 
       
       main = "N2O",
       
       groups = YEAR,
       
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


xyplot(CH4 ~ CH4.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "CH4",
       
       groups = YEAR,
       
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



###############################################################################################################
#                           
#        Do a regression with Ordinary least squares OLS
#
###############################################################################################################


OLS.regression.CO2 <- lm(CO2 ~ CO2.ppm, 
                     
                     data = C.GC.Standards.All) ;

summary(OLS.regression.CO2)

plot(OLS.regression.CO2)


str(OLS.regression.CO2)


plot(OLS.regression.CO2$fitted.values, OLS.regression.CO2$residuals)

plot(C.GC.Standards.All$CO2.ppm , OLS.regression.CO2$residuals ) 

##### it seems that the residuals grow as the level of CO2 increases. Heteroscedasticity.  Try log transformation

C.GC.Standards.All$log.CO2 <- log(C.GC.Standards.All$CO2) ; 

C.GC.Standards.All$log.CO2.ppm <- log(C.GC.Standards.All$CO2.ppm) ; 


#### CO2 ~ log.CO2.ppm  ####


xyplot(CO2 ~ log.CO2.ppm,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
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

OLS.regression.Log <- lm(CO2 ~ log.CO2.ppm , 
                     
                     data = C.GC.Standards.All) ;

#### There are standards with 0 CO2 ppm and log 0 is  NA/NaN/Inf, the regression failed ###

# ###############################################################################################################
# 
# 
#    Using the Box-Cox transformation analysis to find the data transformation that best reduces Heteroscedasticity 
#  
#     Draper and Smith, 1998. Applied regression Analysis, Page 280. Using the R package MASS from the Book
#
#     Venables and Ripley, "Modern Applied Statistics with S" (4th edition, 2002), page 170
#
#    https://cran.r-project.org/web/packages/MASS/index.html  
#
#    https://cran.r-project.org/web/packages/MASS/refman/MASS.html
#
#    https://cran.r-project.org/web/packages/MASS/MASS.pdf
# 
# 
# ################################################################################################################

#### ratio of the largest to smalles response data  ####

max(C.GC.Standards.All$CO2)/min(C.GC.Standards.All$CO2)

### Transformation is granted and recommended!!!! #####

boxcox(OLS.regression.CO2)

boxcox(CO2 ~ CO2.ppm , data = C.GC.Standards.All , lambda = seq(from = -2, to = 2 , by = 0.25),
       
       plotit = T )

boxcox(OLS.regression.CO2 , data = C.GC.Standards.All , lambda = seq(from = -2, to = 2 , by = 0.25),
       
       plotit = T )


boxcox(OLS.regression.CO2 , data = C.GC.Standards.All , lambda = seq(from = 0, to = 1 , by = 0.01),
       
       plotit = T )

boxcox(OLS.regression.CO2 , data = C.GC.Standards.All , lambda = seq(from = 0.3, to = 0.6 , by = 0.01),
       
       plotit = T )


OLS.regression.CO2.BoxCox <- boxcox(OLS.regression.CO2 , lambda = seq(from = 0.3, to = 0.6 , by = 0.01),
       
       plotit = T) ;

str(OLS.regression.CO2.BoxCox)

max(OLS.regression.CO2.BoxCox$y)

which(OLS.regression.CO2.BoxCox$y == max(OLS.regression.CO2.BoxCox$y))

OLS.regression.CO2.BoxCox$x[59]

##### The best lambda is OLS.regression.CO2.BoxCox$x[59] = 0.4757576 , which is very close to 0.5  ######

##### Considering the transformation Yt = Log(Y+alpha])  the function logtrans of the pakage MASS helps determine the 
##### optimal alpha. Venables and Ripley, "Modern Applied Statistics with S" (4th edition, 2002), page 170

logtrans(OLS.regression.CO2)

logtrans(OLS.regression.CO2, alpha = seq(from = 0, to = 10, by = 1 ) )

logtrans(OLS.regression.CO2, alpha = seq(from = 0, to = 100, by = 10) )

logtrans(OLS.regression.CO2, alpha = seq(from = 0, to = 1000, by = 100) )

##### The likelihood function explodes as alpha increases... best not to use that transformation ######

#### The optimal transformation according to the Box-Cox transformation analysis is sqrt(y) = a + X1...

C.GC.Standards.All$sqrt_CO2 <- sqrt(C.GC.Standards.All$CO2) ;

str(C.GC.Standards.All)


OLS.regression.sqrt_CO2 <- lm(sqrt_CO2 ~ CO2.ppm, 
                              
                              data = C.GC.Standards.All) ;

summary(OLS.regression.sqrt_CO2)  
  
plot(OLS.regression.sqrt_CO2) 

str(OLS.regression.sqrt_CO2)

#### The best model for calibration of the CO2 GC data is 

summary(OLS.regression.sqrt_CO2) 

# Call:
#   lm(formula = sqrt_CO2 ~ CO2.ppm, data = C.GC.Standards.All)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -39.204  -5.497   0.577   5.345  36.231 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.556e+01  5.495e-01   64.72   <2e-16 ***
#   CO2.ppm     2.012e-02  2.395e-04   84.02   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.93 on 645 degrees of freedom
# Multiple R-squared:  0.9163,	Adjusted R-squared:  0.9162 
# F-statistic:  7060 on 1 and 645 DF,  p-value: < 2.2e-16
# 

# sqrt_CO2 = 3.556e+01 + 2.012e-02 * CO2.ppm
# 
# sqrt_CO2 = 35.56 + 0.0201 * CO2.ppm 
# 
# The calibration curve is then 
# 
# (sqrt_CO2 - 35.56) / 0.0201 = CO2.ppm 
#
#  CO2.ppm = (sqrt_CO2 - 35.56) / 0.0201 

CO2.ppm.Calibrated <- (C.GC.Standards.All$sqrt_CO2 - 35.56) / 0.0201  ; 

plot(CO2.ppm ~ CO2,
         
         data = C.GC.Standards.All, 
         
         main = "CO2",
         
         pch = 16,
         
         cex = 1,
         
         col = "red")

points(CO2.ppm.Calibrated ~ C.GC.Standards.All$CO2, pch = 1, cex = 1.5 , col = "blue")
       
       

legend("topleft", legend = c("CO2.ppm", "CO2.ppm.Calibrated"), col = c("red" , "blue"), pch = c(16, 1))
       


CO2.ppm.Calibrated <- C.GC.Standards.All$sqrt_CO2 - 35.56) / 201.2 ; 

  
points((sqrt_CO2 - 35.56) / 201.2) ~ CO2,
     
     data = C.GC.Standards.All, 
     
     main = "CO2",
     
     pch = 16,
     
     cex = 1,
     
     col = "red",
     
     autokey = T)  
  
  
  
  
  


##### decoding the data, reverting the transformation  ######

fitted(OLS.regression.sqrt_CO2)


 

  
  



 


######### For Log,Log regression 0 CO2.ppm needs to be changed. It is changed to 0.001 for this analysis #######

C.GC.Standards.All$CO2.ppm.NoZero <- C.GC.Standards.All$CO2.ppm  ;


C.GC.Standards.All[C.GC.Standards.All$CO2.ppm.NoZero == 0, "CO2.ppm.NoZero"] <- 0.1 ;

C.GC.Standards.All$log.CO2.ppm.NoZero <- log(C.GC.Standards.All$CO2.ppm.NoZero) ;


#############################################################################################################

OLS.regression.Log <- lm(CO2 ~ log.CO2.ppm.NoZero , 
                         
                         data = C.GC.Standards.All) ;

summary(OLS.regression.Log)

plot(OLS.regression.Log$residuals ~ C.GC.Standards.All$log.CO2.ppm.NoZero,
       
       main = "CO2",
       
       pch = 16,
       
       cex = 1,
       
       col = c("red"))
      
plot(OLS.regression.Log$residuals ~ OLS.regression.Log$fitted.values,
     
     main = "CO2",
     
     pch = 16,
     
     cex = 1,
     
     col = c("red"))



#### log.CO2 ~ log.CO2.ppm  ####




xyplot(log.CO2 ~ log.CO2.ppm.NoZero,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
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

OLS.regression.Log.log <- lm( log.CO2 ~ log.CO2.ppm.NoZero , 
                          
                          data = C.GC.Standards.All) ;

str(OLS.regression.Log.log)

summary(OLS.regression.Log.log)

plot(C.GC.Standards.All$log.CO2.ppm.NoZero , OLS.regression.Log.log$residuals)

plot( C.GC.Standards.All$CO2.ppm , OLS.regression.Log.log$residuals)

plot(OLS.regression.Log.log$fitted.values , OLS.regression.Log.log$residuals)










######## The best results are without transformation of the data  ########

xyplot(CO2.ppm ~ CO2 ,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
         
         panel = function(x,y,...) {
           
           panel.xyplot(x,y, ...)
           
          # panel.lmline(CO2,CO2.ppm, col = "blue", lwd =2)
           
           panel.abline(a = OLS.regression$coefficients[1] , b = OLS.regression$coefficients[2],
                        
                        col = "red" , lwd = 1.5)
         
           panel.text(x = 4000,  y = 4000, 
                    
                    labels = paste(round(OLS.regression$coefficients[1],3), "+",
                                   
                                   round(OLS.regression$coefficients[2],3), "X",
                                   sep = " "),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         
         
       }
        
       )


#################### Ordinary least square regression for N2O #######################

OLS.regression.N2O <- lm(N2O.ppm ~ N2O, 
                     
                     data = C.GC.Standards.All) ;


summary(OLS.regression.N2O)

plot(OLS.regression.N2O)


xyplot(N2O.ppm ~ N2O ,
       
       data = C.GC.Standards.All, 
       
       main = "N2O",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         # panel.lmline(CO2,CO2.ppm, col = "blue", lwd =2)
         
         panel.abline(a = OLS.regression.N2O$coefficients[1] , b = OLS.regression.N2O$coefficients[2],
                      
                      col = "red" , lwd = 1.5)
         
         panel.text(x = 2000,  y = 40, 
                    
                    labels = paste(round(OLS.regression.N2O$coefficients[1],3),"+",
                                   
                                   round(OLS.regression.N2O$coefficients[2],3), "X",
                                   sep = " "),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         
         
       }
       
)





###############################################################################################################
#                           
#        Use quantile regression for the standards
#
###############################################################################################################


############## quantile regression  CO2 ###########

Quantile.Reg.2 <- rq( CO2.ppm ~ CO2, data = C.GC.Standards.All, tau = seq(from = 0.1, to = 0.9 , by = 0.1) )

str(Quantile.Reg.2 )

plot(Quantile.Reg.2)

summary(Quantile.Reg.2)

coefficients(Quantile.Reg.2)[,1]


xyplot(CO2.ppm ~ CO2 ,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         panel.lmline(C.GC.Standards.All$CO2 , C.GC.Standards.All$CO2.ppm, col = "blue", lwd =2)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,1] , b = coefficients(Quantile.Reg.2)[2,1],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,2] , b = coefficients(Quantile.Reg.2)[2,2],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,3] , b = coefficients(Quantile.Reg.2)[2,3],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,4] , b = coefficients(Quantile.Reg.2)[2,4],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,5] , b = coefficients(Quantile.Reg.2)[2,5],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,6] , b = coefficients(Quantile.Reg.2)[2,6],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,7] , b = coefficients(Quantile.Reg.2)[2,7],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,8] , b = coefficients(Quantile.Reg.2)[2,8],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,9] , b = coefficients(Quantile.Reg.2)[2,9],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.text(x = 4000,  y = 4000, 
                    
                    labels = paste(round(OLS.regression$coefficients[1],3),
                                   
                                   round(OLS.regression$coefficients[2],3),
                                   sep = " + "),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         
         
       }
       
)


############## quantile regression  N2O ###########

Quantile.Reg.N2O <- rq( N2O.ppm ~ N2O, data = C.GC.Standards.All, tau = seq(from = 0.1, to = 0.9 , by = 0.1) )

str(Quantile.Reg.N2O )

plot(Quantile.Reg.N2O)

summary(Quantile.Reg.N2O)

coefficients(Quantile.Reg.N2O)[,1]



xyplot(N2O.ppm ~ N2O ,
       
       data = C.GC.Standards.All, 
       
       main = "N2O",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         panel.lmline(C.GC.Standards.All$N2O , C.GC.Standards.All$N2O.ppm, col = "blue", lwd =2)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,1] , b = coefficients(Quantile.Reg.N2O)[2,1],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,2] , b = coefficients(Quantile.Reg.N2O)[2,2],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,3] , b = coefficients(Quantile.Reg.N2O)[2,3],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,4] , b = coefficients(Quantile.Reg.N2O)[2,4],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,5] , b = coefficients(Quantile.Reg.N2O)[2,5],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,6] , b = coefficients(Quantile.Reg.N2O)[2,6],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,7] , b = coefficients(Quantile.Reg.N2O)[2,7],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,8] , b = coefficients(Quantile.Reg.N2O)[2,8],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O)[1,9] , b = coefficients(Quantile.Reg.N2O)[2,9],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.text(x = 2000,  y = 40, 
                    
                    labels = paste(round(OLS.regression.N2O$coefficients[1],3), "+",
                                   
                                   round(OLS.regression.N2O$coefficients[2],3), "X",
                                   sep = " "),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         
         
       }
       
)





######### It seems that two extreme points are moving the regression and the quantile regression to the right

C.GC.Standards.All[C.GC.Standards.All$CO2.ppm == 5000 & C.GC.Standards.All$CO2 >= 25000 ,]

C.GC.Standards.All[ (C.GC.Standards.All$YEAR == 2021 & C.GC.Standards.All$Rownames == 1768),]

C.GC.Standards.All[ (C.GC.Standards.All$YEAR == 2022 & C.GC.Standards.All$Rownames == 1486),]


C.GC.Standards.All <- C.GC.Standards.All[! (C.GC.Standards.All$YEAR == 2021 & C.GC.Standards.All$Rownames == 1768),]

C.GC.Standards.All <- C.GC.Standards.All[! (C.GC.Standards.All$YEAR == 2022 & C.GC.Standards.All$Rownames == 1486),]


######## OLS regression with out the outliers ################


OLS.regression.2 <- lm(CO2.ppm ~ CO2, 
                     
                     data = C.GC.Standards.All) ;

######## quantile regression with out the outliers ################


Quantile.Reg.2 <- rq( CO2.ppm ~ CO2, data = C.GC.Standards.All, tau = seq(from = 0.1, to = 0.9 , by = 0.1) )

str(Quantile.Reg.2)

str(coef(Quantile.Reg.2))

coef(Quantile.Reg.2)[c(1:2),"tau= 0.1"]



###############################################################################################################
#                           
#        Comparison between ordinary least squares regression and quantile regression for the CO2 standards
#
###############################################################################################################

xyplot(CO2.ppm ~ CO2 ,
       
       data = C.GC.Standards.All, 
       
       main = "CO2",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         panel.lmline(C.GC.Standards.All$CO2 , C.GC.Standards.All$CO2.ppm, col = "blue", lwd =2)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,1] , b = coefficients(Quantile.Reg.2)[2,1],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,2] , b = coefficients(Quantile.Reg.2)[2,2],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,3] , b = coefficients(Quantile.Reg.2)[2,3],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,4] , b = coefficients(Quantile.Reg.2)[2,4],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,5] , b = coefficients(Quantile.Reg.2)[2,5],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,6] , b = coefficients(Quantile.Reg.2)[2,6],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,7] , b = coefficients(Quantile.Reg.2)[2,7],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,8] , b = coefficients(Quantile.Reg.2)[2,8],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.2)[1,9] , b = coefficients(Quantile.Reg.2)[2,9],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.text(x = 4000,  y = 4000, 
                    
                    labels = paste0("OLS - CO2ppm = " , round(OLS.regression$coefficients[1],5),
                                    
                                    " + " , round(OLS.regression$coefficients[2],5),"CO2"),
                    
                    col = "black" ,
                    
                    cex = 2)
         
       
         panel.text(x = 4000,  y = 3000, 

                    labels = paste0("Quant - CO2ppm = " , round(coef(Quantile.Reg.2)[1, "tau= 0.5"],3),

                                    " + " , round(coef(Quantile.Reg.2)[2, "tau= 0.5"],3),"CO2"),

                    col = "black" ,

                    cex = 2)

         
         
       }
       
)


###############################################################################################################
#                           
#        Comparison between ordinary least squares regression and quantile regression for the N2O standards
#
###############################################################################################################

######## OLS regression with out the outliers ################


OLS.regression.N2O.2 <- lm(N2O.ppm ~ N2O, 
                       
                       data = C.GC.Standards.All) ;

######## quantile regression with out the outliers ################


Quantile.Reg.N2O.2 <- rq( N2O.ppm ~ N2O, data = C.GC.Standards.All, tau = seq(from = 0.1, to = 0.9 , by = 0.1) )

str(Quantile.Reg.N2O.2 )


######## quantile regression with out the outliers ################


xyplot(N2O.ppm ~ N2O ,
       
       data = C.GC.Standards.All, 
       
       main = "N2O",
       
       groups = YEAR,
       
       pch = c(16, 1),
       
       cex = c(1, 1.5),
       
       col = c("red" , "blue"),
       
       key = list(
         
         space = "top",
         
         columns = 2,
         
         text = list(c("2021" , "2022")),
         
         points = list(pch = c(16, 1),
                       
                       col = c("red" , "blue"))),
       
       panel = function(x,y,...) {
         
         panel.xyplot(x,y, ...)
         
         panel.lmline(C.GC.Standards.All$N2O , C.GC.Standards.All$N2O.ppm, col = "blue", lwd =2)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,1] , b = coefficients(Quantile.Reg.N2O.2)[2,1],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,2] , b = coefficients(Quantile.Reg.N2O.2)[2,2],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,3] , b = coefficients(Quantile.Reg.N2O.2)[2,3],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,4] , b = coefficients(Quantile.Reg.N2O.2)[2,4],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,5] , b = coefficients(Quantile.Reg.N2O.2)[2,5],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,6] , b = coefficients(Quantile.Reg.N2O.2)[2,6],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,7] , b = coefficients(Quantile.Reg.N2O.2)[2,7],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,8] , b = coefficients(Quantile.Reg.N2O.2)[2,8],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.abline(a = coefficients(Quantile.Reg.N2O.2)[1,9] , b = coefficients(Quantile.Reg.N2O.2)[2,9],
                      
                      col = "gray" , lwd = 1.5)
         
         panel.text(x = 10000,  y = 50, 
                    
                    labels = paste0("OLS - N2Oppm = ", round(OLS.regression.N2O$coefficients[1],5), "+",
                                   
                                   round(OLS.regression.N2O$coefficients[2],5), "N2O"),
                    
                    col = "black" ,
                    
                    cex = 2)
         
         
         panel.text(x = 10000,  y = 40, 

                    labels = paste0("Quant - N2Oppm = " , round(coef(Quantile.Reg.N2O.2)[1, "tau= 0.5"],5),

                                    " + " , round(coef(Quantile.Reg.N2O.2)[2, "tau= 0.5"],5),"N2O"),

                    col = "black" ,

                    cex = 2)

         
         
         
       }
       
)





